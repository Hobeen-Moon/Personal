library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(tidyverse)
library(DT)
library(rsconnect)
library(qualtRics)
# https://shiny.byui.edu/

qualtrics_api_credentials(api_key = "GfSD59QSwFLlNpjCP04XiPetqMdpfytZ8w206maX",
                          base_url = "https://az1.qualtrics.com",
                          install = TRUE,
                          overwrite = TRUE)

surveys <- all_surveys()

mysurvey <- fetch_survey(surveyID = surveys$id[which(surveys$id == "SV_6n7surJqp0Z8v6l")])
mysurvey2 <- mysurvey[c(4, 6:8, 12, 17:44, 49:50, 58, 60:66, 71:80, 85:86, 94, 96:108)]
cm <- mysurvey2 %>% filter(Q11 != "")
General <- cm %>%
  select("Q11", "Q12", "Q13", "Q22", "Q21", "Q47", "Q2", "Q3", "Q113", "Q57", "StartDate")

names(General) <- c("First Name", "Last Name", "I-Number","Home City", "Home State", "Class", "Ant Grad Year", "Grad Semester", "Num of Internship Completed", "Post Grad Plan", "Survey Date")

Anticipated <- cm %>%
  select("Q11", "Q12", "Q13","Q113","Q114", "Q116", "Q118", "Q118_1", "Q122_1") %>% 
  filter(Q114 != "")

names(Anticipated)  <- c("First Name", "Last Name", "I-Number", "Num of Internship Completed", "Ant Internship Year", "Ant Internship Semester", "City", "State","Internship Focus")

FirstInternship <- cm %>%
  select("Q11", "Q12", "Q13","Q120", "Q126", "Q138_1", "Q124", "Q122", "Q134_1", "Q130_1", "Q132_1") %>% 
  filter(Q138_1 != "")

names(FirstInternship) <- c("First Name", "Last Name", "I-Number", "Internship Company", "Internship City", "Internship State", "Year", "Semester", "Job title & Work", "Payrate ($/hour)", "Overall Exp")

SecondInternship <- cm %>%
  select("Q11", "Q12", "Q13","Q132", "Q135", "Q136", "Q134", "Q133", "Q139_1", "Q137_1", "Q138") %>% 
  filter(Q136 != "")

names(SecondInternship) <- c("First Name", "Last Name", "I-Number", "Internship Company", "Internship City", "Internship State", "Year", "Semester", "Job title & Work", "Payrate ($/hour)", "Overall Exp")

AcceptedWork <- cm %>%
  select("Q11", "Q12", "Q13","Q30", "Q59", "Q31", "Q32", "Q145", "Q34_1", "Q35", "Q36") %>% 
  filter(Q30 != "")

names(AcceptedWork) <- c("First Name", "Last Name", "I-Number", "Accepted Company", "Type of Work", "Job title & Work", "City", "State", "Salary $/year", "Post Grad Email", "Post Grad Phone Number")

PostBusiness <- cm %>%
  select("Q11", "Q12", "Q13","Q61", "Q64", "Q146", "Q67", "Q68") %>% 
  filter(Q146 != "")

names(PostBusiness) <- c("First Name", "Last Name", "I-Number", "Type of Business", "City", "State", "Post Grad Email", "Post Grad Phone Number")

LookingForJob <- cm %>%
  select("Q11", "Q12", "Q13","Q2", "Q3","Q70", "Q76", "Q72", "Q147", "Q75", "Q76_1") %>% 
  filter(Q147 != "")

names(LookingForJob) <- c("First Name", "Last Name", "I-Number","Grad Year", "Grad Semester", "Type of Work", "Attributes to be hired", "City", "State", "Post Grad Email", "Post Grad Phone Number")

GradSchool <- cm %>%
  select("Q11", "Q12", "Q13","Q80", "Q81", "Q82", "Q83", "Q110", "Q85", "Q87", "Q88") %>% 
  filter(Q85 != "")

names(GradSchool) <- c("First Name", "Last Name", "I-Number","School", "Degree", "Purpose of Degree", "City", "State", "Program Cost", "Post Grad Email", "Post Grad Phone Number")

OverallData <- cm %>% 
  select("StartDate", "Q11", "Q12", "Q13", "Q22", "Q21", "Q47", "Q2", "Q3", "Q113", "Q143", "Q57", "Q114", "Q116", "Q118", "Q118_1", "Q122_1", "Q120", "Q126", "Q138_1", "Q124", "Q122", "Q134_1", "Q130_1", "Q132_1", "Q132", "Q135", "Q136", "Q134", "Q133", "Q139_1", "Q137_1", "Q138", "Q30", "Q59", "Q31", "Q32", "Q145", "Q34_1", "Q35", "Q36", "Q61", "Q64", "Q146", "Q67", "Q68", "Q70", "Q76", "Q72", "Q147", "Q75", "Q76_1", "Q80", "Q81", "Q82", "Q83", "Q110", "Q85", "Q87", "Q88")

names(OverallData) <- c("Survey Date", "First Name", "Last Name", "I-Number","Home City", "Home State", "Class", "Ant Grad Year", "Grad Semester", "Num of Internship Completed", "Programming Exp", "Post Grad Plan", "Ant Internship Year", "Ant Internship Semester", "Ant City", "Ant State","Internship Focus", "FI Company", "FI City", "FI State", "FI Year", "FI Semester", "FI Job title & Work", "FI Payrate ($/hour)", "FI Overall Exp", "SI Company", "SI City", "SI State", "SI Year", "SI Semester", "SI Job title & Work", "SI Payrate ($/hour)", "SI Overall Exp", "Accepted Company", "Type of Work", "Job title & Work", "City", "State", "Salary $/year", "Post Grad Email", "Post Grad Phone Number", "Type of Business", "City", "State", "Post Grad Email", "Post Grad Phone Number", "Type of Work", "Attributes to be hired", "City", "State", "Post Grad Email", "Post Grad Phone Number", "Grad School", "Degree", "Purpose of Degree", "Gradschool City", "Gradschool State", "Program Cost", "Post Grad Email", "Post Grad Phone Number")



ui <- dashboardPage(
  dashboardHeader(title="DCM Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Reactive Datatable", tabName = "cmtable", icon=icon("table")),
      menuItem("Demographics", tabName="demographics", icon=icon("bar-chart-o"))
    ),
    
    conditionalPanel("input.dataset === 'General'",
                     checkboxGroupInput("show_vars1", "Choose columns you want from the survey", names(General), selected = names(General))),
    conditionalPanel("input.dataset === 'Anticipated'",
                     checkboxGroupInput("show_vars2", "Choose columns you want from the survey", names(Anticipated), selected = names(Anticipated))),
    conditionalPanel("input.dataset === 'FirstInternship'",
                     checkboxGroupInput("show_vars3", "Choose columns you want from the survey", names(FirstInternship), selected = names(FirstInternship))),
    conditionalPanel("input.dataset === 'SecondInternship'",
                     checkboxGroupInput("show_vars4", "Choose columns you want from the survey", names(SecondInternship), selected = names(SecondInternship))),
    conditionalPanel("input.dataset === 'AcceptedWork'",
                     checkboxGroupInput("show_vars5", "Choose columns you want from the survey", names(AcceptedWork), selected = names(AcceptedWork))),
    conditionalPanel("input.dataset === 'PostBusiness'",
                     checkboxGroupInput("show_vars6", "Choose columns you want from the survey", names(PostBusiness), selected = names(PostBusiness))),
    conditionalPanel("input.dataset === 'LookingForJob'",
                     checkboxGroupInput("show_vars7", "Choose columns you want from the survey", names(LookingForJob), selected = names(LookingForJob))),
    conditionalPanel("input.dataset === 'GradSchool'",
                     checkboxGroupInput("show_vars8", "Choose columns you want from the survey", names(GradSchool), selected = names(GradSchool))),
    conditionalPanel("input.dataset === 'OverallData'",
                     checkboxGroupInput("show_vars9", "Choose columns you want from the survey", names(OverallData), selected = names(OverallData))))  
  ,
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "cmtable", 
              h2("Reactive Datatable")
              ,
              tabsetPanel(
                id = 'dataset',
                tabPanel("General", DT::dataTableOutput("mytable1")),
                tabPanel("Anticipated", DT::dataTableOutput("mytable2")),
                tabPanel("FirstInternship", DT::dataTableOutput("mytable3")),
                tabPanel("SecondInternship", DT::dataTableOutput("mytable4")),
                tabPanel("AcceptedWork", DT::dataTableOutput("mytable5")),
                tabPanel("PostBusiness", DT::dataTableOutput("mytable6")),
                tabPanel("LookingForJob", DT::dataTableOutput("mytable7")),
                tabPanel("GradSchool", DT::dataTableOutput("mytable8")),
                tabPanel("OverallData", DT::dataTableOutput("mytable9"))
              ),
              DT::dataTableOutput("table"), style = "overflow-x: scroll;"
      ),
      tabItem(tabName = "demographics",
              h2("Student Demographics/Summaries"),
              fluidRow(
                box(height=463,
                    plotOutput("plot1")
                ),
                
                tabBox(
                  tabPanel("First Internship",
                           plotOutput("plot2")
                  ),
                  tabPanel("Second Internship",
                           plotOutput("plot3")
                  ), 
                  tabPanel("Post Grad",
                           plotOutput("plot4")
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output) { 
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(General[, input$show_vars1, drop = FALSE])
  })
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(Anticipated[, input$show_vars2, drop = FALSE])
  })
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(FirstInternship[, input$show_vars3, drop = FALSE])
  })
  output$mytable4 <- DT::renderDataTable({
    DT::datatable(SecondInternship[, input$show_vars4, drop = FALSE])
  })
  output$mytable5 <- DT::renderDataTable({
    DT::datatable(AcceptedWork[, input$show_vars5, drop = FALSE])
  })
  output$mytable6 <- DT::renderDataTable({
    DT::datatable(PostBusiness[, input$show_vars6, drop = FALSE])
  })
  output$mytable7 <- DT::renderDataTable({
    DT::datatable(LookingForJob[, input$show_vars7, drop = FALSE])
  })
  output$mytable8 <- DT::renderDataTable({
    DT::datatable(GradSchool[, input$show_vars8, drop = FALSE])
  })
  output$mytable9 <- DT::renderDataTable({
    DT::datatable(OverallData[, input$show_vars9, drop = FALSE])
  })
  
  output$plot1 <- renderPlot ({
    cm %>% 
      ggplot(aes(x= Q113))+
      geom_bar(fill="steelblue")+
      theme_minimal()+
      labs(title="Number of Students That Have Completed None, 1, and 2 Internships", 
           y="Number of Students", x="Internships Completed")
  })
  
  output$plot2 <- renderPlot ({
    FIrate <- FirstInternship %>% filter(!is.na(`Payrate ($/hour)`))
    avgFIrate <- mean(FIrate$`Payrate ($/hour)`)
    FirstInternship %>% ggplot(aes(x=`Payrate ($/hour)`))+
      geom_histogram(fill="steelblue", binwidth = 1)+
      geom_vline(xintercept = avgFIrate, size=2, color="grey30")+
      theme_minimal()+
      labs(title="Hourly Rate Distribution for First Internship", y="Count", x="Dollars Per Hour",
           subtitle=paste("Average hourly rate is", " $", round(avgFIrate, 0), sep=""))
  })
  
  output$plot3 <- renderPlot ({
    SIrate <- SecondInternship %>% filter(!is.na(`Payrate ($/hour)`))
    avgSIrate <- mean(SIrate$`Payrate ($/hour)`)
    SecondInternship %>% ggplot(aes(x=`Payrate ($/hour)`))+
      geom_histogram(fill="steelblue", binwidth = 1)+
      geom_vline(xintercept = avgSIrate, size=2, color="grey30")+
      theme_minimal()+
      labs(title="Hourly Rate Distribution for Second Internship", y="Count", x="Dollars Per Hour",
           subtitle=paste("Average hourly rate is", " $", round(avgSIrate, 0), sep=""))
    
  })
  
  output$plot4 <- renderPlot ({
    PGrate <- AcceptedWork %>% filter(!is.na(`Salary $/year`))
    avgPGrate <- mean(PGrate$`Salary $/year`)
    AcceptedWork %>% ggplot(aes(x=`Salary $/year`))+
      geom_histogram(fill="steelblue")+
      geom_vline(xintercept = avgPGrate, size=2, color="grey30")+
      theme_minimal()+
      scale_x_continuous(labels=scales::comma)+
      labs(title="Salary Distribution for Post Graduation Job", y="Count", x="Salary",
           subtitle=paste("Average salary is", " $", round(avgPGrate, 0), sep=""))
    
  }) 
  
  
}

shinyApp(ui, server)
