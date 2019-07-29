#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(formattable)

#setwd("./Clean-Seafood-Conservation-Species-Table")
#data <- readRDS("./Clean-Seafood-Conservation-Species-Table/shinydata.rds")


#### Table Body ####
body <- dashboardBody(
    fluidRow(
        column(width = 3,
               box(width = NULL,
                   
                   
                   h3("Assessment Type:"),
                   switchInput(
                       inputId = "assess",
                       label = "RAM Only",
                       value = TRUE),
                   
                   
                   h3("Require Conservation Status"),
                   switchInput(
                       inputId = "conserv",
                       #label = "Yes",
                       onLabel = "Yes",
                       offLabel = "No",
                       value = TRUE),
                   
                   radioGroupButtons("aqua", h3("Aquaculture Option"),
                                     choices = list("Yes" = "Yes",
                                                    "Both" = "All",
                                                    "No"  = "No"),
                                     selected = "All",
                                     status = "primary"),
                   
                   
                   sliderInput("FMSY", h3("FMSY needed:"),  0.00, 1.0, value = c(.50, 1.0)),
                   
                   sliderInput("BMSY", h3("BMSY needed:"),  0.00, 1.0, value = c(.50, 1.0)),
                   
                   sliderTextInput(
                       inputId = "stat_slide", 
                       label = h3("IUCN Status:"), 
                       grid = TRUE, 
                       force_edges = TRUE,
                       choices = list("LC" ,
                                   "NT", 
                                   "VU", 
                                   "EN", 
                                   "CR"),
                       selected = c("VU", "CR")
                       
                       ),
                   
                   
                   
                   #checkboxGroupInput("require", h3("Require:"), choices = list(
                   #   "Aquaculture" = "Aquaculture Alternative == 'Yes'",
                   #   "IUCN Status" = "!is.na(`IUCN Status`)"
                   #)),
                   
                   radioGroupButtons("arrange",
                                h3("Arrange By:"),
                                choices = list("Revenue" = "Revenue (Thousands USD)",
                                               "FMSY" = "Proportion of Fisheries Experiencing Overfishing (FMSY > 1)",
                                               "BMSY" = "Proportion of Fishereis Being Overfished (BMSY < 1)"),
                                selected = "Revenue (Thousands USD)",
                                individual = TRUE
                                )
                   
               )
        ),
        
        
        
        column(width = 9,
               box(width = NULL,
                   formattableOutput("table")))
    )
)


#### UI ####
ui <- dashboardPage(
    dashboardHeader(title = "Clean Seafood Conservation Species Table"),
    dashboardSidebar(
        disable = TRUE
        #collapsed = TRUE
        ),
    body
)


#### Server ####
server <- function(input, output) {
    
    #  data <- readRDS("shinydata.rds") %>% 
    #      filter(
    #          filter(`Proportion of Fishereis Being Overfished (BMSY < 1)` >= input$BMSY)
    #      )

    

    
#### Table Output ####
    output$table <- renderFormattable({formattable(
        
        
        data <- readRDS("shinydata.rds") %>% 
            
            {if ((input$assess) == TRUE) dplyr::filter(., SOURCE == "RAM")
                
                else dplyr::filter(., SOURCE == "ALL")
                
            } %>% 
            
            {if 
                #(is.null(input$aqua)) . 
                ((input$conserv) == TRUE)    dplyr::filter(., !is.na(`IUCN Status`)) %>% 
                                             dplyr::filter(., `IUCN Status` >= input$stat_slide[1] &
                                                              `IUCN Status` <= input$stat_slide[2])
                else (dplyr::filter(., is.na(`IUCN Status`) |
                                        `IUCN Status` >= input$stat_slide[1] &
                                         `IUCN Status` <= input$stat_slide[2])
                                        )
            } %>% 
            
            select(-SOURCE) %>% 
            
            filter(`Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` >= input$FMSY[1] & 
                       `Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` <= input$FMSY[2]) %>% 
            filter(`Proportion of Fishereis Being Overfished (BMSY < 1)` >= input$BMSY[1] &
                       `Proportion of Fishereis Being Overfished (BMSY < 1)` <= input$BMSY[2]) %>% 
            {if 
                #(is.null(input$aqua)) . 
                 ((input$aqua) == "All")    dplyr::filter(., `Aquaculture Alternative` == "Yes" | `Aquaculture Alternative` == "No")
            else (dplyr::filter(., `Aquaculture Alternative` == input$aqua))
            } %>% 
            arrange(dplyr::desc(!!rlang::sym(input$arrange))) %>% 
            mutate(`Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` = percent(`Proportion of Fisheries Experiencing Overfishing (FMSY > 1)`),
                   `Proportion of Fishereis Being Overfished (BMSY < 1)` = percent(`Proportion of Fishereis Being Overfished (BMSY < 1)`)) ,
        
        
        align = c("l", "l", "c", "c", "c", "c", "c", "c"),
        list(
            
            
            
            
            `IUCN Status`  = formatter(
                "span",
                style = ~ style(
                    color = case_when(
                        is.na(`IUCN Status`) == TRUE ~ "white",
                        is.na(`IUCN Status`) == FALSE ~ "black"
                    ),
                    display = "inline-block",
                    padding = "0 4px",
                    "border-radius" = "4px",
                    "width" = "60px",
                    "background-color" = case_when(
                        `IUCN Status` == "LC" ~ "green",
                        `IUCN Status` == "NT"  ~"lightgreen",
                        `IUCN Status` == "VU"  ~"yellow",
                        `IUCN Status` == "EN"  ~"orange",
                        `IUCN Status` == "CR"  ~"red"
                    )
                )),
            
            
            `Causes or is Effected by Bycatch` = color_tile("#DC143C", "#3CB371"),
            
            
            
            `Aquaculture Alternative`  = formatter(
                "span",
                style = ~ style(
                    display = "inline-block",
                    padding = "0 4px",
                    "border-radius" = "4px",
                    "width" = "60px",
                    "background-color" = case_when(
                        `Aquaculture Alternative` == "Yes" ~ "green",
                        `Aquaculture Alternative` == "No"  ~"red"
                    )
                )),
            
            `Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` = formatter(
                "span",
                style = ~ style(
                    display = "inline-block",
                    padding = "0 4px",
                    "border-radius" = "4px",
                    "width" = "60px",
                    "background-color" = case_when(
                        `Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` < .25 ~ "green",
                        `Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` >= .25 &  
                            `Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` < .5  ~"yellow",
                        `Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` >= .5 &  
                            `Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` < .75  ~"orange",
                        `Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` >= .75 ~"red"
                    )
                )),
            `Proportion of Fishereis Being Overfished (BMSY < 1)` = formatter(
                "span",
                style = ~ style(
                    display = "inline-block",
                    padding = "0 4px",
                    "border-radius" = "4px",
                    "width" = "60px",
                    "background-color" = case_when(
                        `Proportion of Fishereis Being Overfished (BMSY < 1)` < .25 ~ "green",
                        `Proportion of Fishereis Being Overfished (BMSY < 1)` >= .25 &  
                            `Proportion of Fishereis Being Overfished (BMSY < 1)` < .5  ~"yellow",
                        `Proportion of Fishereis Being Overfished (BMSY < 1)` >= .5 &  
                            `Proportion of Fishereis Being Overfished (BMSY < 1)` < .75  ~"orange",
                        `Proportion of Fishereis Being Overfished (BMSY < 1)` >= .75 ~"red"
                    )))

            )
    )})
    
#### Plotly Output ####    

    
#### End of App ####
}
shinyApp(ui = ui, server = server)