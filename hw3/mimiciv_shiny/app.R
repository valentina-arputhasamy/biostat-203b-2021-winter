#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rsconnect)
library(tidyverse)
#install.packages("shinythemes")
library(shinythemes)


icu_cohort <- readRDS("icu_cohort.rds")

demtable <- icu_cohort %>%
  select(subject_id, gender, age_at_adm, ethnicity, language, marital_status )

demtable$language <- ifelse(demtable$language == "?", "Unknown", "English" )

colnames(demtable) <- c("Subject ID", "Gender", "Age (years)", "Ethnicity", 
                        "Language", "Marital Status")

demtable$Gender <- as.factor(demtable$Gender)
demtable$Ethnicity <- as.factor(demtable$Ethnicity)
demtable$Language <- as.factor(demtable$Language)
demtable$`Marital Status` <- as.factor(demtable$`Marital Status`)

labtable <- icu_cohort %>%
  select(subject_id, bicarbonate, calcium, chloride, creatinine, glucose, magnesium,
         potassium, sodium, hematocrit, lactate)

colnames(labtable) <- c("Subject ID", "Bicarbonate", "Calcium", "Chloride", 
                        "Creatinine", "Glucose", "Magnesium", 
                        "Potassium", "Sodium", "Hematocrit", "Lactate")

labdemtable <- icu_cohort %>%
  select(gender, age_at_adm, ethnicity, language, marital_status, bicarbonate,
         calcium, chloride, creatinine, glucose, magnesium,
         potassium, sodium, hematocrit, lactate)

colnames(labdemtable) <- c("Gender", "Age (Years)", "Ethnicity", "Language", 
                           "Marital Status", "Bicarbonate", "Calcium", "Cholride",
                           "Creatinine", "Glucose", "Magnesium", "Potassium",
                           "Sodium","Hematocrit","Lactate")

vitaldemtable <- icu_cohort %>%
  select(gender, age_at_adm, ethnicity, language, marital_status, heart_rate, 
         non_invasive_blood_pressure_systolic, non_invasive_blood_pressure_mean,
         respiratory_rate, temperature_fahrenheit, arterial_blood_pressure_systolic,
         arterial_blood_pressure_mean)

colnames(vitaldemtable) <- c("Gender", "Age (Years)", "Ethnicity", "Language", 
                             "Marital Status", "Heart Rate", "Non-Invasive Systolic BP",
                             "Mean Non-Invasive BP", "Respiratory Rate",
                             "Temperature (Fahrenheit)", "Systolic Arterial BP", 
                             "Mean Arterial BP")

labdemtable$Gender <- as.factor(demtable$Gender)
labdemtable$Ethnicity <- as.factor(demtable$Ethnicity)
labdemtable$Language <- as.factor(demtable$Language)
labdemtable$`Marital Status` <- as.factor(demtable$`Marital Status`)


#fluid page - what the app looks like

# Define UI 

ui <- fluidPage(
  
  titlePanel("MIMIC-IV Data Visualization"),
  
  navbarPage("MIMIC Data", theme = shinytheme("lumen"),
             tabPanel("Demographics", fluid = T, #first tab for demographics
                 titlePanel("Demographic Charts"),
             sidebarLayout(
               sidebarPanel(
                 radioButtons("demchoice",
                             "Select Demographic Type:", c("Gender", 
                                                           "Age (years)", 
                                                           "Ethnicity", 
                                                           "Language", 
                                                           "Marital Status"))),
               mainPanel(plotOutput("demPlot"))),
             
               titlePanel("Summary Statistics"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("demsumm", "Select Demographic Type:", 
                             c("Gender", "Ethnicity", "Language", 
                               "Marital Status"))),
                mainPanel(
                  tableOutput("demsumtable"))),
                  
                  titlePanel("Table of Values"),
                  sidebarLayout(
                    sidebarPanel(
                      numericInput("obstable", 
                                   "Number of observations (max: 50,048):", 
                                   10, max = 50048)),
                    mainPanel(
                      tableOutput("view")))),
  
              tabPanel("Lab Measurements", fluid = T,
                       titlePanel("Distributions"),
                       sidebarLayout(
                         sidebarPanel(
                         radioButtons("labchoice", "Select Lab Measurement:", c("Bicarbonate", "Calcium", "Creatinine", "Glucose", "Magnesium", "Potassium",
                                                     "Sodium", "Hematocrit", "Lactate")),
                         numericInput("obslab", "Number of observations (max: 50,048)", 25000, max = 50048)),
                       mainPanel(
                         plotOutput("labplot"))),
                       
                       titlePanel("Summary Statistics"),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("labsumm", "Select Lab Type:", c("Bicarbonate", "Calcium", "Creatinine", "Glucose", "Magnesium",
                                                                        "Sodium", "Lactate"))),
                         mainPanel(
                           verbatimTextOutput("labsumtable"))),
                       
                       titlePanel("Lab Measurements by Age, by Demographic"),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("labtype", "Select Lab Type:", c("Bicarbonate", "Calcium", "Creatinine", "Glucose", "Magnesium",
                                                                        "Sodium", "Lactate")),
                           selectInput("demtype", "Select Demographic Type:", c("Gender", "Ethnicity", "Language", "Marital Status"))),
                         mainPanel(plotOutput("facetplot")))),
                       
                  tabPanel("Vital Measurements", fluid = T,
                           titlePanel("Distributions"),
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("vitaltype", "Select Vital Type:", 
                                            c("Heart Rate", "Non-Invasive Systolic BP", 
                                              "Mean Non-Invasive BP",
                                              "Respiratory Rate", "Temperature (Fahrenheit)",
                                              "Systolic Arterial BP", "Mean Arterial BP")),
                               numericInput("obsvital", "Number of observations (max: 50,048)", 25000, max = 50048)),
                             mainPanel(plotOutput("vitalplot"))),
                           
                           titlePanel("Summary Statistics"),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("vitalsumm", "Select Vital Type:",
                                           c("Heart Rate", "Non-Invasive Systolic BP", "Mean Non-Invasive BP",
                                             "Mean Non-Invasive BP", "Respiratory Rate",
                                             "Temperature (Fahrenheit)", "Systolic Arterial BP", "Mean Arterial BP",
                                             "Systolic Arterial BP", "Mean Arterial BP"))),
                           mainPanel(
                             verbatimTextOutput("vitalsumtable"))),
                           
                           titlePanel("Vital Measurements by Age, by Demographic"),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("vitaltype2", "Select Vital Type:", 
                                           c("Heart Rate", "Non-Invasive Systolic BP","Mean Non-Invasive BP",
                                             "Respiratory Rate", "Temperature (Fahrenheit)", "Systolic Arterial BP",
                                             "Mean Arterial BP", "Systolic Arterial BP", "Mean Arterial BP")),
                               selectInput("demtype2", "Select Demographic Type:", 
                                           c("Gender", "Ethnicity", "Language", "Marital Status"))),
                             mainPanel(plotOutput("facetplot2"))))))
          
                        
                           
                           
                          
                       
            





server <- function(input, output) {
  
    output$demPlot <- renderPlot({
      ggplot(demtable, aes(eval(as.name(input$demchoice)))) + 
        geom_bar(fill = "darkseagreen3", color = "darkseagreen4" ) + 
        coord_flip() + xlab(input$demchoice)
    })
    
    #return requested summary statistics
    
    output$demsumtable <- renderTable({
      demtable %>%
        count(eval(as.name(input$demsumm))) %>%
        mutate(percent = n/sum(n)) %>%
        rename(frequency = n)
        #rename(frequency)
        #summarize(frequency = n(input$demsumm))
        #summarize(frequency = n()) %>%
    })
    
    #show first n observations
    
    output$view <- renderTable({
      
      head(demtable, n = input$obstable)
    })
      
    output$labplot <- renderPlot({
      ggplot(labtable, aes(eval(as.name(input$labchoice)))) + 
        geom_histogram(binwidth = 1, fill="darkseagreen3", color = "darkseagreen4") +
        xlab(input$labchoice) 
    })
    
    output$labsumtable <- renderPrint({
      labtable %>% select(input$labsumm) %>% summary()
      
    })
    
    output$facetplot <- renderPlot({
      ggplot(labdemtable) + geom_point(mapping = aes(x = `Age (Years)`, y = eval(as.name(input$labtype))), color = "darkseagreen4") +
        facet_wrap(~eval(as.name(input$demtype))) + ylab(input$labtype)
        
    })
    
    output$vitalplot <- renderPlot({
      ggplot(vitaldemtable, aes(eval(as.name(input$vitaltype)))) + 
        geom_histogram(binwidth = 1, fill="darkseagreen3", color = "darkseagreen4") +
        xlab(input$vitaltype) 
    })
    
    output$vitalsumtable <- renderPrint({
      vitaldemtable %>% select(input$vitalsumm) %>% summary()
    })
    
    output$facetplot2 <- renderPlot({
      ggplot(vitaldemtable) + geom_point(mapping = aes(x = `Age (Years)`, y = eval(as.name(input$vitaltype2))), color = "darkseagreen4") +
        facet_wrap(~eval(as.name(input$demtype2))) + ylab(input$vitaltype2)
    })
  
}


# Run the application 
shinyApp(ui = ui, server = server)



