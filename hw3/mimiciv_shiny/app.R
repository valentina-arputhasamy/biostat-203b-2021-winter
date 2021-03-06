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

labtable <- icu_cohort %>%
  select(bicarbonate, calcium, chloride, creatinine, glucose, magnesium,
         potassium, sodium, hematocrit, lactate)

colnames(labtable) <- c("Bicarbonate", "Calcium", "Chloride", 
                        "Creatinine", "Glucose", "Magnesium", 
                        "Potassium", "Sodium", "Hematocrit", "Lactate")

demtable$language <- ifelse(demtable$language == "?", "Unknown", "English" )

colnames(demtable) <- c("Subject ID", "Gender", "Age (years)", "Ethnicity", 
                        "Language", "Marital Status")

demtable$Gender <- as.factor(demtable$Gender)
demtable$Ethnicity <- as.factor(demtable$Ethnicity)
demtable$Language <- as.factor(demtable$Language)
demtable$`Marital Status` <- as.factor(demtable$`Marital Status`)

#fluid page - what the app looks like

# Define UI 

ui <- fluidPage(
  
  titlePanel("MIMIC-IV Data Visualization"),
  
  navbarPage("MIMIC Data", theme = shinytheme("lumen"),
             tabPanel("Demographics", #first tab for demographics
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
  
              tabPanel("Lab Measurements",
                       titlePanel("Lab Measurement Distributions"),
                       sidebarLayout(
                         sidebarPanel(
                         radioButtons("labchoice", "Select Lab Measurement:", c("Bicarbonate", "Calcium", "Creatinine", "Glucose", "Magnesium", "Potassium",
                                                     "Sodium", "Hematocrit", "Lactate")),
                         numericInput("obslab", "Number of observations (max: 50,048)", 25000, max = 50048)),
                       mainPanel(
                         plotOutput("labplot"))))))
  


server <- function(input, output) {
  
    output$demPlot <- renderPlot({
      ggplot(demtable, aes(eval(as.name(input$demchoice)))) + 
        geom_bar(fill = "lightsteelblue2", color = "lightsteelblue2" ) + 
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
        geom_histogram(binwidth = 1, fill="lightsteelblue2", color = "lightsteelblue4") +
        xlab(input$labchoice) 
    })
    
        
  
}




# Run the application 
shinyApp(ui = ui, server = server)
