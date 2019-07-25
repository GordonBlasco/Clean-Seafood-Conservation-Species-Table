#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shinydashboard)
library(shiny)
library(formattable)

body <- dashboardBody(
    fluidRow(
        column(width = 3,
               box(width = NULL,
                   sliderInput("FMSY", "FMSY needed:",  0.00, 1.0, value = c(.50, 1.0)),
                   sliderInput("BMSY", "BMSY needed:",  0.00, 1.0, value = c(.50, 1.0)),
                   radioButtons("arrange",
                                h3("Arrange By:"),
                                choices = list("Revenue" = "Revenue (Thousands USD)",
                                               "FMSY" = "Proportion of Fisheries Experiencing Overfishing (FMSY > 1)",
                                               "BMSY" = "Proportion of Fishereis Being Overfished (BMSY < 1)"),
                                selected = "Revenue (Thousands USD)"
                   )
                   
               )
        ),
        
        
        
        column(width = 9,
               box(width = NULL,
                   formattableOutput("table")))
    )
)

ui <- dashboardPage(
    dashboardHeader(title = "Clean Seafood Conservation Species Table"),
    dashboardSidebar(disable = TRUE),
    body
)

server <- function(input, output) {
    
    #  data <- readRDS("shinydata.rds") %>% 
    #      filter(
    #          filter(`Proportion of Fishereis Being Overfished (BMSY < 1)` >= input$BMSY)
    #      )
    
    output$table <- renderFormattable({formattable(
        
        
        data <- readRDS("shinydata.rds") %>% 
            filter(`Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` >= input$FMSY[1] & 
                       `Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` <= input$FMSY[2]) %>% 
            filter(`Proportion of Fishereis Being Overfished (BMSY < 1)` >= input$BMSY[1] &
                       `Proportion of Fishereis Being Overfished (BMSY < 1)` <= input$BMSY[2]) %>% 
            arrange(dplyr::desc(!!rlang::sym(input$arrange))) %>% 
            mutate(`Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` = percent(`Proportion of Fisheries Experiencing Overfishing (FMSY > 1)`),
                   `Proportion of Fishereis Being Overfished (BMSY < 1)` = percent(`Proportion of Fishereis Being Overfished (BMSY < 1)`)) ,
        
        
        align = c("l", "l", "c", "c", "c", "c", "c", "c"),
        list(
            `IUCN Status` = color_tile("#DC143C", "yellow"), #("#C61717", "#FFFF00")
            `Causes or is Effected by Bycatch` = color_tile("#DC143C", "#3CB371"),
            `Aquaculture Alternative` = color_tile("#3CB371", "#DC143C"))
    )})
}
shinyApp(ui = ui, server = server)