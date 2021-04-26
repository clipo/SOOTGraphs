library(shiny)
library(data.table)
library(tidyverse)
library(purrr)
library(ggplot2)
library(viridisLite)
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
      addition to a course inventory. In order to use it, you must first navigate to 
      the `SOOT surveys` page in `My.Binghamton.edu`, which can be found under the `Academic Services` -> 'SOOT Surveys'
      tab towards the top of the screen. This script was 
      written by Professor Xingye Qiao of the Department of
      Mathematics and then modified by Professors Carl Lipo and Nancy Um of the Harpur
      College Dean's Office for online use."),
    p(),
    HTML("To download SOOT data, go to this link: <a href='https://my.binghamton.edu/page/ACADEMIC_SERVICES/sootsurveys'>https://my.binghamton.edu/page/ACADEMIC_SERVICES/sootsurveys</a>. Download each of the files shown under RESULTS by clicking on the Download CSV button. 
     Select the `Download CSV` 
    function for each course. Do not rename the files."), 
    p(),
    p("Once complete, upload all of the files through the interface below. Finally, click `Process uploaded data` below"),
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
    h2("Results Aggregated by Question and Term"),
    fluidRow(
        # Main panel for displaying outputs ----
        plotOutput("term_plot")
    ),
    fluidRow(
        downloadButton("downloadTermPlot", "Download Aggregated Term Plot"),
        downloadButton("downloadTermData", "Download Aggregated Term Data as CSV")
    ),
    h2("Course Inventory"),
    p("Generate a course inventory, which includes the number of responses for each course."),
    fluidRow(
        downloadButton("downloadInventory", "Download Course Inventory as CSV")
    ),
    fluidRow(
        downloadButton("downloadAnswers", "Download Answers as CSV")
    )
)

server <- function(input, output) {
    observeEvent(input$process, {
        
        info<-tibble(course = character(), term = character())
        nfiles = nrow(input$csvs) 
        csv = list()
        for (i in 1 : nfiles)
        {
            csv[[i]] = read.csv(input$csvs$datapath[i])
        }
        files <- input$csvs$datapath
        terms=list()
        courses=list()
        for(i in 1:length(files)){
            file_info <-  str_split(input$csvs$name[i], "-|_|\\.")
            terms[i]<-unlist(file_info)[3]
            courses[i]<-unlist(file_info)[1]
        }

        # Generate data frame
        full = NULL
        
        for(i in 1:length(files)){

            full = rbind(full, as_tibble(read.csv(files[i],header=T)) 
                         %>% mutate(course = unlist(courses[i]), term = unlist(terms[i])))
            #full <- full %>% add_column(course = unlist(courses[i]), term = unlist(terms[i]))
        }

        # Aggregate the info by question
        instructor_related_ques = full %>% filter(QUES_TEXT %in% c("The instructor is well prepared for class.",
                                                                   "The instructor demonstrates a thorough knowledge of the subject.",
                                                                   "The instructor communicates his/her subject well.",
                                                                   "The instructor explains complex ideas clearly.",
                                                                   "The instructor stimulates my interest in the core subject.",
                                                                   "The instructor is receptive to questions.",
                                                                   "The instructor is available to help me outside of class.",
                                                                   "The instructor encourages me to think analytically.",
                                                                   "Overall, the instructor is an effective teacher.")) %>% 
            mutate(fall.spring = ifelse(nchar(term)==6,substr(term,1,4),substr(term,1,3)),
                   year = ifelse(nchar(term)==6,substr(term,5,6),substr(term,4,5)),
                   term_month = ifelse(fall.spring=="FALL", 9, 2)) %>% 
            unite(term, year, term_month, fall.spring)
        
        ques_count = instructor_related_ques %>% 
            group_by(QUES_TEXT,term,course) %>% 
            summarize(ques_count = sum(ANS_COUNT))
        
        percentages_overall = instructor_related_ques %>% 
            left_join(ques_count) %>% 
            group_by(QUES_TEXT, ANS_TEXT) %>% 
            summarise(mean_PCT = sum(ANS_PCT*ques_count, na.rm = T)/sum(ques_count)) %>% 
            mutate(Answer = factor(ANS_TEXT, levels=c("Not Applicable", "Very Low or Never", "Low", "Average", "High", "Very High or Always")), QUES_TEXT = factor(QUES_TEXT,levels = c("The instructor is well prepared for class.", "The instructor demonstrates a thorough knowledge of the subject.", "The instructor communicates his/her subject well.", "The instructor explains complex ideas clearly.", "The instructor stimulates my interest in the core subject.", "The instructor is receptive to questions.", "The instructor is available to help me outside of class.", "The instructor encourages me to think analytically.", "Overall, the instructor is an effective teacher.")))
        
        # Generate the plot
        overall_plot <- percentages_overall %>% 
            ggplot(aes(x=QUES_TEXT, y = mean_PCT, fill = Answer)) +
            geom_bar(stat="identity", position="stack")+
            theme(axis.text.x=element_text(size=8, angle = -45, hjust = 0, face = "bold"))+
            theme(legend.title = element_text(size=8)) +
            theme(legend.text = element_text(size=8)) +
            scale_fill_viridis_d()+labs(x = "", y = "Percentage")
        
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
        
        percentages_by_term = instructor_related_ques %>% 
            left_join(ques_count) %>% 
            group_by(QUES_TEXT, ANS_TEXT, term) %>% 
            summarise(mean_PCT = sum(ANS_PCT*ques_count, na.rm = T)/sum(ques_count)) %>% 
            mutate(Answer = factor(ANS_TEXT, levels=c("Not Applicable", "Very Low or Never", "Low", "Average", "High", "Very High or Always")), 
                   QUES_TEXT = factor(QUES_TEXT,levels = c("The instructor is well prepared for class.", "The instructor demonstrates a thorough knowledge of the subject.", "The instructor communicates his/her subject well.", "The instructor explains complex ideas clearly.", "The instructor stimulates my interest in the core subject.", "The instructor is receptive to questions.", "The instructor is available to help me outside of class.", "The instructor encourages me to think analytically.", "Overall, the instructor is an effective teacher.")))
        
        # Create string wrap so that the titles are legible
        swr = function(percentages_by_term, nwrap=20) {
            paste(strwrap(percentages_by_term, width=nwrap), collapse="\n")
        }
        swr = Vectorize(swr)
        
        percentages_by_term$QUES_TEXT = swr(percentages_by_term$QUES_TEXT)
        
        # Generate plot
        term_plot <- percentages_by_term %>% 
            ggplot(aes(x=term, y = mean_PCT, fill = Answer)) +
            geom_bar(stat="identity", position="stack")+
            theme(axis.text.x=element_text(size = 8, angle = -45, hjust = 0, face = "bold"))+
            scale_fill_viridis_d()+labs(x = "", y = "Percentage") + facet_wrap(~QUES_TEXT)
        
        output$term_plot <- renderPlot({
            term_plot
        })
        output$downloadTermPlot <- downloadHandler(
            filename = "TermPlot.png",
            content = function(file) {
                ggsave(file,termplot)
            }
        )
        output$downloadTermData<- downloadHandler(
            filename = "percentages_by_term.csv",
            content = function(file) { 
                write.table(percentages_by_term, file, sep = ",", row.names = FALSE)
                }
            )
        output$downloadAnswers<- downloadHandler(
            filename = "answers.csv",
            content = function(file) { 
                write.table(instructor_related_ques, file, sep = ",", row.names = FALSE)
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

