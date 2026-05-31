library(shiny)
library(data.table)
library(tidyverse)
library(purrr)
library(ggplot2)
library(viridisLite)
source("R/helpers.R")
#library("shiny.collections")

## Harpur College SOOT Aggregation

#The following script was written by Professor Xingye Qiao of the Math Department, 
#and modified for the Cloud by Professor Nancy Um of Art History and the Harpur 
#Dean's Office, Binghamton University, to aid in the conversion of SOOT 
#(Student Opinion of Teaching) data into an aggregated format.

#In order to use this script, you must upload your own data. Navigate to the #
#SOOT surveys page in `myBinghamton`, which can be found under the 
#`Academic Services` tab. For each relevant course, select `Download.csv`. 
#Do not rename the files. Upload all the .csv files at the same time. 


ui <- fluidPage(
    # Application title
    title = "Harpur College SOOT Processing",
    # Add header and information
    h1("Harpur College SOOT Aggregator"),
    br(),
    p("This aggregator allows you to upload your raw SOOT (Student Opinion of Teaching) 
      data to generate results in the form of aggregated graphs and spreadsheets, in 
      addition to a course inventory. In order to use it, you must first download your SOOT data. Go to this link: "),
    HTML("<a href='https://my.binghamton.edu/page/ACADEMIC_SERVICES/sootsurveys'>https://my.binghamton.edu/page/ACADEMIC_SERVICES/sootsurveys</a>."), 
    p("Download the files shown under RESULTS by clicking on the `Download CSV` button for each course. Do not rename the files."), 
    p("Once you have downloaded all of the CSV files, upload them through the interface below. Finally, click `Process uploaded data` below."), 
    p("The aggregated graphs will appear. They are available for download as PNG files. The associate spreadsheets may also be downloaded. 
    Note that these files are not stored or kept on the server and will not be available to you after you close the browser window. No one will have access to them."),
    p("This script was written by Professor Xingye Qiao of the Department of Mathematics and then modified by Professors Carl Lipo and Nancy Um of the Harpur
      College Dean's Office for online use."),
    p(),
    fluidRow(
        column(width = 4,
               fileInput("csvs",
                         label="Upload SOOT CSV files here",
                         multiple = TRUE)),
        column(width = 4,
               br(),
               actionButton("process", "Process uploaded data")
        )
    ),
    h2("Overall Results Aggregated by Question"),
    fluidRow(
        # Main panel for displaying outputs ----
        plotOutput("overall_plot")
    ),
    fluidRow(
        # Button
        downloadButton("downloadOverallPlot", "Download Overall Plot"),
        downloadButton("downloadOverallData", "Download Overall Data as CSV")
    ),
    h2("Results Aggregated by Term"),
    fluidRow(
        # Main panel for displaying outputs ----
        plotOutput("term_plot")
    ),
    fluidRow(
        downloadButton("downloadTermPlot", "Download Aggregated Term Plot"),
        downloadButton("downloadTermData", "Download Aggregated Term Data as CSV")
    ),
    h2("Results Aggregated by Course"),
    fluidRow(
        # Main panel for displaying outputs ----
        plotOutput("course_plot")
    ),
    fluidRow(
        downloadButton("downloadCoursePlot", "Download Aggregated Course Plot"),
        downloadButton("downloadCourseData", "Download Aggregated Course Data as CSV")
    ),
    h2("Summary Score by Question"),
    fluidRow(plotOutput("summary_student_plot")),
    fluidRow(
        downloadButton("downloadSummaryStudentPlot", "Download Student-Weighted Plot"),
        downloadButton("downloadSummaryStudentData", "Download Student-Weighted Data")
    ),
    fluidRow(plotOutput("summary_course_plot")),
    fluidRow(
        downloadButton("downloadSummaryCoursePlot", "Download Course-Weighted Plot"),
        downloadButton("downloadSummaryCourseData", "Download Course-Weighted Data")
    ),
    p("Two summaries of the same ratings are shown. The student-weighted version pools
      every student response together, so courses with more respondents have more
      influence; it reflects the experience of the average student. The course-weighted
      version summarizes each course on its own and then averages those course summaries
      equally, so every course counts the same regardless of size; it reflects the typical
      course. The two differ when class sizes vary: a single large course pulls the
      student-weighted number toward its own ratings, while the course-weighted number
      gives a small seminar the same say as a large lecture."),
    h2("Summary Score Trends by Term"),
    fluidRow(plotOutput("trend_student_plot", height = "600px")),
    fluidRow(
        downloadButton("downloadTrendStudentPlot", "Download Student-Weighted Trend"),
        downloadButton("downloadTrendStudentData", "Download Student-Weighted Trend Data")
    ),
    fluidRow(plotOutput("trend_course_plot", height = "600px")),
    fluidRow(
        downloadButton("downloadTrendCoursePlot", "Download Course-Weighted Trend"),
        downloadButton("downloadTrendCourseData", "Download Course-Weighted Trend Data")
    ),
    h2("Response Counts by Course"),
    p("Number of rating respondents per course. Percentages from courses with few
      respondents are less reliable; use this to judge how much weight to give each bar."),
    fluidRow(plotOutput("response_count_plot")),
    fluidRow(
        downloadButton("downloadResponseCountPlot", "Download Response Count Plot"),
        downloadButton("downloadResponseCountData", "Download Response Count Data")
    ),
    h2("Course Inventory"),
    p("Generate a course inventory, which includes the number of responses for each course. 
         You may also want to add a column for the overall enrollment, in order to show the overall response rate for each course."),
    fluidRow(
        downloadButton("downloadInventory", "Download Course Inventory as CSV")
    ),
    fluidRow(
        downloadButton("downloadAnswers", "Download Answers as CSV")
    )
)

server <- function(input, output) {
    observeEvent(input$process, {

        # Guard against the process button being clicked with no files uploaded
        if (is.null(input$csvs) || nrow(input$csvs) == 0) {
            showNotification("Please upload at least one SOOT CSV file before processing.",
                             type = "warning")
            return()
        }

        # Read every uploaded file through the ingestion layer into the
        # canonical long format. Empty/unsupported files are skipped (NULL).
        full <- NULL
        for (i in seq_along(input$csvs$datapath)) {
            one <- tryCatch(
                read_soot_file(input$csvs$datapath[i], input$csvs$name[i]),
                error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }
            )
            if (!is.null(one)) full <- bind_rows(full, one)
        }
        if (is.null(full) || nrow(full) == 0) {
            showNotification("No usable data found in the uploaded files.", type = "warning")
            return()
        }

        # Aggregate the info by question
        instructor_related_ques <- full |>
            filter(QUES_TEXT %in% INSTRUCTOR_QUESTIONS)
        
        ques_count = instructor_related_ques %>% 
            group_by(QUES_TEXT,term,course) %>% 
            summarize(ques_count = sum(ANS_COUNT))
        
        overall_dist <- compute_rating_distribution(instructor_related_ques, "QUES_TEXT")
        na_overall <- round(mean(overall_dist$na_share$na_pct, na.rm = TRUE), 1)
        percentages_overall <- overall_dist$dist |>
            rename(Answer = ANS_TEXT, mean_PCT = pct) |>
            mutate(QUES_TEXT = factor(QUES_TEXT, levels = INSTRUCTOR_QUESTIONS))
        
        # Generate the plot
        overall_plot <- percentages_overall %>% 
            ggplot(aes(x=QUES_TEXT, y = mean_PCT, fill = Answer)) +
            geom_bar(stat="identity", position="stack")+
            theme(axis.text.x=element_text(size=8, angle = -45, hjust = 0, face = "bold"))+
            theme(legend.title = element_text(size=8)) +
            theme(legend.text = element_text(size=8)) +
            scale_fill_viridis_d()+labs(x = "", y = "Percentage")
        
        overall_plot <- overall_plot + labs(
            title = "Overall Results Aggregated by Question",
            subtitle = sprintf(
                "Generated by the Harpur College SOOT Aggregator. Bars show ratings only; 'Not Applicable' averaged %.1f%% of responses and is excluded.",
                na_overall))
        
        output$overall_plot <- renderPlot({
            overall_plot
        }) 
        output$downloadOverallPlot <- downloadHandler(
            filename = "OverallPlot.png",
            content = function(file) {
                ggsave(file,overall_plot)
            }
        )

        output$downloadOverallData<- downloadHandler(
            filename = "percentages_overall.csv",
            content = function(file) { 
                write.table(percentages_overall, file, sep = ",", row.names = FALSE)
            }
        )
        
        term_dist <- compute_rating_distribution(instructor_related_ques, c("QUES_TEXT", "term"))
        percentages_by_term <- term_dist$dist |>
            rename(Answer = ANS_TEXT, mean_PCT = pct) |>
            mutate(term = order_terms(term))
        
        # Create string wrap so that the titles are legible
        swr = function(percentages_by_term, nwrap=20) {
            paste(strwrap(percentages_by_term, width=nwrap), collapse="\n")
        }
        swr = Vectorize(swr)
        
        percentages_by_term$QUES_TEXT = swr(percentages_by_term$QUES_TEXT)
        num_term = length(unique(instructor_related_ques$term))
        font_size<-.9
        if(num_term>10){ font_size<-.7 }
        if(num_term>20){ font_size<-.5 }
        
        # Generate plot
        term_plot <- percentages_by_term %>% 
            ggplot(aes(x=term, y = mean_PCT, fill = Answer)) +
            geom_bar(stat="identity", position="stack")+
            theme(axis.text.x=element_text(size = rel(font_size), angle = -45, hjust = 0, face = "bold"))+
            scale_fill_viridis_d()+labs(x = "", y = "Percentage") + facet_wrap(~QUES_TEXT)
        
        term_plot <- term_plot + labs(title = "Results Aggregated by Term",
              subtitle = "Generated by the Harpur College SOOT Aggregator")
        
        output$term_plot <- renderPlot({
            term_plot
        })
        output$downloadTermPlot <- downloadHandler(
            filename = "TermPlot.png",
            content = function(file) {
                ggsave(file,term_plot)
            }
        )
        
        output$downloadTermData<- downloadHandler(
            filename = "percentages_by_term.csv",
            content = function(file) { 
                write.table(percentages_by_term, file, sep = ",", row.names = FALSE)
                }
            )
        
        num_courses = length(unique(instructor_related_ques$course))
        font_size<-.9
        if(num_courses>10){ font_size<-.7 }
        if(num_courses>20){ font_size<-.5 }
        course_dist <- compute_rating_distribution(instructor_related_ques, c("QUES_TEXT", "course"))
        percentages_by_course <- course_dist$dist |>
            rename(Answer = ANS_TEXT, mean_PCT = pct)
        
        # Create string wrap so that the titles are legible
        swr = function(percentages_by_course, nwrap=20) {
            paste(strwrap(percentages_by_course, width=nwrap), collapse="\n")
        }
        swr = Vectorize(swr)
        
        percentages_by_course$QUES_TEXT = swr(percentages_by_course$QUES_TEXT)
        
        # Generate plot
        course_plot <- percentages_by_course %>% 
            ggplot(aes(x=course, y = mean_PCT, fill = Answer)) +
            geom_bar(stat="identity", position="stack")+
            theme(axis.text.x=element_text(size = rel(font_size), angle = -45, hjust = 0, face = "bold"))+
            scale_fill_viridis_d()+labs(x = "", y = "Percentage") + facet_wrap(~QUES_TEXT) #+
           #scale_x_discrete(guide = guide_axis(n.dodge = 2))
        
        course_plot <- course_plot + labs(title = "Results Aggregated by Course",
              subtitle = "Generated by the Harpur College SOOT Aggregator")
        
        output$course_plot <- renderPlot({
            course_plot
        })
        output$downloadCoursePlot <- downloadHandler(
            filename = "CoursePlot.png",
            content = function(file) {
                ggsave(file,course_plot)
            }
        )
        output$downloadCourseData<- downloadHandler(
            filename = "percentages_by_course.csv",
            content = function(file) { 
                write.table(percentages_by_course, file, sep = ",", row.names = FALSE)
            }
        )
        
        output$downloadAnswers<- downloadHandler(
            filename = "answers.csv",
            content = function(file) { 
                write.table(instructor_related_ques, file, sep = ",", row.names = FALSE)
            }
        )
        ttb_student <- compute_top_two_box(instructor_related_ques, "QUES_TEXT", "student")
        ttb_course  <- compute_top_two_box(instructor_related_ques, "QUES_TEXT", "course")

        summary_student_plot <- plot_summary_by_question(ttb_student, "Student-Weighted", "n")
        summary_course_plot  <- plot_summary_by_question(ttb_course, "Course-Weighted", "n_courses")

        output$summary_student_plot <- renderPlot({ summary_student_plot })
        output$summary_course_plot  <- renderPlot({ summary_course_plot })

        output$downloadSummaryStudentPlot <- downloadHandler(
            filename = "SummaryByQuestion_StudentWeighted.png",
            content = function(file) { ggsave(file, summary_student_plot, width = 7, height = 5, dpi = 300) })
        output$downloadSummaryCoursePlot <- downloadHandler(
            filename = "SummaryByQuestion_CourseWeighted.png",
            content = function(file) { ggsave(file, summary_course_plot, width = 7, height = 5, dpi = 300) })
        output$downloadSummaryStudentData <- downloadHandler(
            filename = "summary_by_question_student.csv",
            content = function(file) { write.table(ttb_student, file, sep = ",", row.names = FALSE) })
        output$downloadSummaryCourseData <- downloadHandler(
            filename = "summary_by_question_course.csv",
            content = function(file) { write.table(ttb_course, file, sep = ",", row.names = FALSE) })

        trend_student <- compute_top_two_box(instructor_related_ques, c("QUES_TEXT","term"), "student")
        trend_course  <- compute_top_two_box(instructor_related_ques, c("QUES_TEXT","term"), "course")

        trend_student_plot <- plot_trends(trend_student, "Student-Weighted")
        trend_course_plot  <- plot_trends(trend_course, "Course-Weighted")

        output$trend_student_plot <- renderPlot({ trend_student_plot })
        output$trend_course_plot  <- renderPlot({ trend_course_plot })

        output$downloadTrendStudentPlot <- downloadHandler(
            filename = "Trends_StudentWeighted.png",
            content = function(file) { ggsave(file, trend_student_plot, width = 9, height = 6, dpi = 300) })
        output$downloadTrendCoursePlot <- downloadHandler(
            filename = "Trends_CourseWeighted.png",
            content = function(file) { ggsave(file, trend_course_plot, width = 9, height = 6, dpi = 300) })
        output$downloadTrendStudentData <- downloadHandler(
            filename = "trends_student.csv",
            content = function(file) { write.table(trend_student, file, sep = ",", row.names = FALSE) })
        output$downloadTrendCourseData <- downloadHandler(
            filename = "trends_course.csv",
            content = function(file) { write.table(trend_course, file, sep = ",", row.names = FALSE) })

        response_count_plot <- plot_response_counts(instructor_related_ques)
        response_count_data <- instructor_related_ques |>
            group_by(course, term, QUES_TEXT) |>
            summarise(n = sum(ANS_COUNT[ANS_TEXT != "Not Applicable"]), .groups = "drop") |>
            group_by(course, term) |>
            summarise(respondents = max(n), .groups = "drop")

        output$response_count_plot <- renderPlot({ response_count_plot })
        output$downloadResponseCountPlot <- downloadHandler(
            filename = "ResponseCountsByCourse.png",
            content = function(file) { ggsave(file, response_count_plot, width = 7, height = 5, dpi = 300) })
        output$downloadResponseCountData <- downloadHandler(
            filename = "response_counts_by_course.csv",
            content = function(file) { write.table(response_count_data, file, sep = ",", row.names = FALSE) })

        #Create a course inventory
        responses_by_course = ques_count %>% 
            group_by(term, course) %>%  
            summarise(response = max(ques_count))
        
        output$downloadInventory<- downloadHandler(
            filename = "course_inventory.csv",
            content = function(file) { 
                write.table(responses_by_course, file, sep = ",", row.names = FALSE)
            }
        )
    })
}        


shinyApp(ui, server)

