library(shiny)
library(data.table)
library(tidyverse)
library(purrr)
library(ggplot2)
library(viridisLite)
library(bslib)
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


ui <- page_fluid(
    title = "Harpur College SOOT Aggregator",
    theme = bs_theme(version = 5, primary = "#005A43", success = "#6CC24A"),
    tags$head(tags$style(HTML("
      .bu-header { background:#ffffff; display:flex; align-items:center; gap:18px; padding:14px 22px; }
      .bu-header img { height:54px; }
      .bu-header .bu-title { color:#005A43; font-weight:700; font-size:1.6rem; margin:0; }
      .bu-accent { height:6px; background:#005A43; margin-bottom:14px; }
    "))),
    div(class = "bu-header",
        tags$img(src = "BU-logo.png", alt = "Binghamton University"),
        tags$h1(class = "bu-title", "Harpur College SOOT Aggregator")),
    div(class = "bu-accent"),
    layout_sidebar(
        sidebar = sidebar(
            width = 360,
            title = "Upload and process",
            p("Upload your SOOT results to generate aggregated graphs. The app accepts two
               formats: the older CSV export (one file per course, already tallied) and the
               newer XLSX export (one file per course, individual responses). You can mix both."),
            p("Two ways to upload: select all your individual .csv or .xlsx files at once, or
               put them together in a single .zip file and upload that one zip. Do not rename
               the files. Then click Process uploaded data."),
            fileInput("csvs", label = "SOOT files (.csv, .xlsx, or one .zip)",
                      multiple = TRUE, accept = c(".csv", ".xlsx", ".zip")),
            actionButton("process", "Process uploaded data", class = "btn-primary"),
            downloadButton("downloadReport", "Download full report (PDF)", class = "btn-success"),
            p(class = "text-muted", style = "font-size:0.85em;",
              "Files are processed in your browser session and are not stored on the server.")
        ),
        navset_card_tab(
            id = "results",
            nav_panel(
                "Overview",
                card(card_header("Summary Score by Question"),
                     plotOutput("summary_student_plot"),
                     div(downloadButton("downloadSummaryStudentPlot", "Plot"),
                         downloadButton("downloadSummaryStudentData", "Data")),
                     plotOutput("summary_course_plot"),
                     div(downloadButton("downloadSummaryCoursePlot", "Plot"),
                         downloadButton("downloadSummaryCourseData", "Data")),
                     p("Two summaries of the same ratings are shown. The student-weighted version
                        pools every student response together, so courses with more respondents have
                        more influence; it reflects the experience of the average student. The
                        course-weighted version summarizes each course on its own and then averages
                        those course summaries equally, so every course counts the same regardless of
                        size; it reflects the typical course. The two differ when class sizes vary: a
                        single large course pulls the student-weighted number toward its own ratings,
                        while the course-weighted number gives a small seminar the same say as a large
                        lecture.")),
                card(card_header("Overall Results Aggregated by Question"),
                     plotOutput("overall_plot"),
                     div(downloadButton("downloadOverallPlot", "Plot"),
                         downloadButton("downloadOverallData", "Data")))
            ),
            nav_panel(
                "By Term",
                card(card_header("Results Aggregated by Term"),
                     plotOutput("term_plot"),
                     div(downloadButton("downloadTermPlot", "Plot"),
                         downloadButton("downloadTermData", "Data"))),
                card(card_header("Summary Score Trends by Term"),
                     plotOutput("trend_student_plot", height = "600px"),
                     div(downloadButton("downloadTrendStudentPlot", "Plot"),
                         downloadButton("downloadTrendStudentData", "Data")),
                     plotOutput("trend_course_plot", height = "600px"),
                     div(downloadButton("downloadTrendCoursePlot", "Plot"),
                         downloadButton("downloadTrendCourseData", "Data")))
            ),
            nav_panel(
                "By Course",
                card(card_header("Results Aggregated by Course"),
                     plotOutput("course_plot"),
                     div(downloadButton("downloadCoursePlot", "Plot"),
                         downloadButton("downloadCourseData", "Data"))),
                card(card_header("Question-by-Course Comparison"),
                     p("Top-two-box percentage for each question in each course. Use it to see
                        whether a weaker dimension is specific to one course or consistent across
                        courses."),
                     plotOutput("heatmap_plot", height = "500px"),
                     div(downloadButton("downloadHeatmapPlot", "Plot"),
                         downloadButton("downloadHeatmapData", "Data")))
            ),
            nav_panel(
                "Reliability",
                card(card_header("Response Counts by Course"),
                     p("Number of rating respondents per course. Percentages from courses with few
                        respondents are less reliable; use this to judge how much weight to give each
                        bar."),
                     plotOutput("response_count_plot"),
                     div(downloadButton("downloadResponseCountPlot", "Plot"),
                         downloadButton("downloadResponseCountData", "Data"))),
                card(card_header("Response Rate by Course"),
                     p("Percentage of enrolled students who responded, available for XLSX-format
                        uploads (the older CSV export does not include enrollment)."),
                     plotOutput("response_rate_plot"),
                     div(downloadButton("downloadResponseRatePlot", "Plot"),
                         downloadButton("downloadResponseRateData", "Data"))),
                card(card_header("Course Inventory"),
                     p("A course inventory with the number of responses for each course. You may add
                        a column for the overall enrollment to show the response rate for each course."),
                     div(downloadButton("downloadInventory", "Course Inventory CSV"),
                         downloadButton("downloadAnswers", "Answers CSV")))
            )
        )
    )
)

server <- function(input, output) {
    # Default report handler: returns a one-page "no data" PDF if the user clicks
    # download before processing. Reassigned with the real report after processing.
    output$downloadReport <- downloadHandler(
        filename = "SOOT_Report.pdf",
        content = function(file) { build_report_pdf(file, list(), NULL) }
    )
    observeEvent(input$process, {

        # Guard against the process button being clicked with no files uploaded
        if (is.null(input$csvs) || nrow(input$csvs) == 0) {
            showNotification("Please upload at least one SOOT CSV file before processing.",
                             type = "warning")
            return()
        }

        # Expand the upload batch (any .zip is extracted) into a work list, then
        # read each file through the ingestion layer into the canonical schema.
        work <- expand_uploads(input$csvs$datapath, input$csvs$name)
        full <- NULL
        for (i in seq_len(nrow(work))) {
            one <- tryCatch(
                read_soot_file(work$path[i], work$name[i]),
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

        heatmap_plot <- plot_question_course_heatmap(instructor_related_ques)
        heatmap_data <- compute_top_two_box(instructor_related_ques, c("QUES_TEXT","course"), "student")

        output$heatmap_plot <- renderPlot({ heatmap_plot })
        output$downloadHeatmapPlot <- downloadHandler(
            filename = "QuestionByCourseHeatmap.png",
            content = function(file) { ggsave(file, heatmap_plot, width = 8, height = 5, dpi = 300) })
        output$downloadHeatmapData <- downloadHandler(
            filename = "question_by_course.csv",
            content = function(file) { write.table(heatmap_data, file, sep = ",", row.names = FALSE) })

        response_rate_plot <- plot_response_rate(instructor_related_ques)
        response_rate_data <- response_rate_by_course(instructor_related_ques)

        output$response_rate_plot <- renderPlot({ response_rate_plot })
        output$downloadResponseRatePlot <- downloadHandler(
            filename = "ResponseRateByCourse.png",
            content = function(file) { ggsave(file, response_rate_plot, width = 7, height = 5, dpi = 300) })
        output$downloadResponseRateData <- downloadHandler(
            filename = "response_rate_by_course.csv",
            content = function(file) { write.table(response_rate_data, file, sep = ",", row.names = FALSE) })

        output$downloadReport <- downloadHandler(
            filename = "SOOT_Report.pdf",
            content = function(file) {
                report_plots <- list(
                    "Summary Score by Question (Student-Weighted)" = summary_student_plot,
                    "Summary Score by Question (Course-Weighted)" = summary_course_plot,
                    "Overall Results Aggregated by Question" = overall_plot,
                    "Results Aggregated by Term" = term_plot,
                    "Summary Trends by Term (Student-Weighted)" = trend_student_plot,
                    "Summary Trends by Term (Course-Weighted)" = trend_course_plot,
                    "Results Aggregated by Course" = course_plot,
                    "Question-by-Course Comparison" = heatmap_plot,
                    "Response Counts by Course" = response_count_plot,
                    "Response Rate by Course" = response_rate_plot
                )
                report_meta <- list(
                    courses = sort(unique(instructor_related_ques$course)),
                    terms = levels(order_terms(unique(instructor_related_ques$term))),
                    instructor = instructor_related_ques$instructor,
                    n_courses = length(unique(instructor_related_ques$course)),
                    date = Sys.Date()
                )
                build_report_pdf(file, report_plots, report_meta)
            }
        )

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

