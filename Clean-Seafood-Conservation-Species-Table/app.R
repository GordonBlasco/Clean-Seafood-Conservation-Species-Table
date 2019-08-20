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


# SET WORKING DIRECTORY BEFORE DEPLOYING APP DEPLOYING APP BUT DEACTIVATE THE CODE ASWELL!! 

#setwd("./Clean-Seafood-Conservation-Species-Table")


legendinfo <- tribble(
  ~"Color", ~"Percent of stocks being overfished or below sustainable biomass",
  "Red", "75% - 100%",
  "Orange", "50% - 75%",
  "Yellow", "25% - 50%",
  "Green", "0% - 25%"
)

IUCNinfo <- tribble(
  ~"IUCN Status", ~"Level",
  "LC", "Least Concern",
  "NT", "Not Threatened",
  "VU", "Vulnerable",
  "EN", "Endangered",
  "CR", "Critically Endangered"
)

#data <- readRDS("./Clean-Seafood-Conservation-Species-Table/shinydata.rds")
options(scipen = 999)


#### Sidebar ####
sidebar <- dashboardSidebar(
    #collapsed = TRUE,
    sidebarMenu(
        menuItem("Table Navigator", 
                 tabName = "navigator"#, 
                 #icon = icon("dashboard")
                 ),
        menuItem("Menu Groupings", 
                 #icon = icon("th"), 
                 tabName = "groupings"
                 #badgeLabel = "new", 
                 #badgeColor = "green"
                 )
    )
)

#### Table Body ####
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "navigator",
    fluidRow(
        column(width = 3,
               box(width = NULL,
                   
                   
                   h3("Formally Assessed:"),
                   switchInput(
                       inputId = "assess",
                       #label = "RAM Only",
                       onLabel = "Yes",
                       offLabel = "No",
                       value = TRUE),
                   
                   
                  # h3("Require Conservation Status:"),
                   #switchInput(
                   #    inputId = "conserv",
                   #    #label = "Yes",
                   #    onLabel = "Yes",
                   #    offLabel = "No",
                   #    value = TRUE),
                   
                   #radioGroupButtons("aqua", h3("Aquaculture Option"),
                   #                  choices = list("Yes" = "Yes",
                   #                                 "Both" = "All",
                   #                                 "No"  = "No"),
                   #                  selected = "All",
                   #                  status = "primary"),
                   
                   
                   sliderInput("FMSY", h3("Fishing Pressure:"),  0.00, 1.0, value = c(.50, 1.0)),
                   
                   sliderInput("BMSY", h3("Fishery Biomass:"),  0.00, 1.0, value = c(.50, 1.0)),
                   
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
                                choices = list("Countries" = "Number of Countries Fishing",
                                               "Overfishing" = "Proportion of Fisheries Experiencing Overfishing (FMSY > 1)",
                                               "Overfished" = "Proportion of Fishereis Being Overfished (BMSY < 1)",
                                               "Revenue" = "Revenue (Thousands USD)",
                                               "Caught" ="Average Yearly Caught (Tonnes)",
                                               "Farmed"= "Average Yearly Farmed (Tonnes)" 
                                               ),
                                selected = "Revenue (Thousands USD)",
                                individual = TRUE
                                )
               ),
               box(width = NULL,
                   
                   
                   h3("Fishing Pressure and Biomass"),
                   
                   formattableOutput("legend"),
                   
                   h3("IUCN Concervation Status"),
                   formattableOutput("IUCN")
                   
                   )
        ),
        
        
        
        column(width = 9,
               box(width = NULL,
                   formattableOutput("table")))
    ) ),
    tabItem(tabName = "groupings",
            fluidRow(
                column(width = 3,
                       box(width = NULL,
                           
                           h3("Formally Assessed:"),
                           switchInput(
                               inputId = "assess_group",
                               #label = "RAM Only",
                               onLabel = "Yes",
                               offLabel = "No",
                               value = FALSE),
                           
                           
                           h3("Filter by Menu Item:"),
                           
                           radioGroupButtons(
                               inputId = "groupings",
                               label = "Label",
                               choices = c("Crab",      
                                           "Clam",      
                                           "Shrimp",   
                                           "Squid",     
                                           "Oyster",   
                                           "Tuna", 
                                           "Lobster",   
                                           "Scallop",   
                                           "Catfish",   
                                           "Mussel",    
                                           "Salmon",    
                                           "Bass",      
                                           "Mahi Mahi",
                                           "Snapper",   
                                           "Anchovy",   
                                           "Tilapia",   
                                           "Octopus",   
                                           "Cod",       
                                           "Trout" 
                                           ),
                               direction = "vertical",
                               justified = TRUE
                           ),
                           
                           radioGroupButtons("arrange_group",
                                             h3("Arrange By:"),
                                             choices = list("Countries" = "Number of Countries Fishing",
                                                            "Overfishing" = "Proportion of Fisheries Experiencing Overfishing (FMSY > 1)",
                                                            "Overfished" = "Proportion of Fishereis Being Overfished (BMSY < 1)",
                                                            "Revenue" = "Revenue (Thousands USD)",
                                                            "Caught" ="Average Yearly Caught (Tonnes)",
                                                            "Farmed"= "Average Yearly Farmed (Tonnes)"  
                                             ),
                                             selected = "Revenue (Thousands USD)",
                                             individual = TRUE
                           )
                           
                           
                           ),
                       box(width = NULL,
                      
                           h3("Fishing Pressure and Biomass"),
                           
                           formattableOutput("legend2"),
                           
                           h3("IUCN Concervation Status"),
                           formattableOutput("IUCN2")
                           )
                       ),
                
                column(width = 9,
                       box(width = NULL,
                           formattableOutput("groupings")))
                )
            )
)
)


#### UI ####
ui <- dashboardPage(
    dashboardHeader(title = "Clean Seafood Conservation Species Table"),
    sidebar,
    #dashboardSidebar(
    #    disable = TRUE
    #    #collapsed = TRUE
    #    ),
    body
)


#### Server ####
server <- function(input, output) {
    
    #  data <- readRDS("shinydata.rds") %>% 
    #      filter(
    #          filter(`Proportion of Fishereis Being Overfished (BMSY < 1)` >= input$BMSY)
    #      )


    
#### Table Output ####
  
  output$IUCN <- renderFormattable({formattable(IUCNinfo,
                                                align= c("c", "c"),  
                                                list(
                                                  `IUCN Status` = formatter(
                                                    "span",
                                                    style = ~ formattable::style(
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
                                                      ))
                                                    ))
                                                
                                                
                                                )})
  
  
  
  output$IUCN2 <- renderFormattable({formattable(IUCNinfo,
                                                align= c("c", "c"),  
                                                list(
                                                  `IUCN Status` = formatter(
                                                    "span",
                                                    style = ~ formattable::style(
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
                                                      ))
                                                  ))
                                                
                                                
  )})
  
  
  output$legend <- renderFormattable({formattable(legendinfo,
               
        align= c("c", "c"),                                        
        list(   
        `Color` = formatter(
          "span",
          style = ~ formattable::style(
            color = "transparent",
            display = "inline-block",
            padding = "0 4px",
            "border-radius" = "4px",
            "width" = "60px",
            "background-color" = case_when(
              `Color` == "Green" ~ "green",
              `Color` == "Yellow" ~ "yellow",
              `Color` == "Orange" ~ "orange",
              `Color` == "Red" ~ "red"
            )
          )))
        )})
  
  
  
  output$legend2 <- renderFormattable({
    formattable(legendinfo,
                
                align = c("l", "c"),
                list(`Color` = formatter(
                  "span",
                  style = ~ formattable::style(
                    color = "transparent",
                    display = "inline-block",
                    padding = "0 4px",
                    "border-radius" = "4px",
                    "width" = "60px",
                    "background-color" = case_when(
                      `Color` == "Green" ~ "green",
                      `Color` == "Yellow" ~ "yellow",
                      `Color` == "Orange" ~ "orange",
                      `Color` == "Red" ~ "red"
                    )
                  )
                )))
  })
  
    output$table <- renderFormattable({formattable(
        
        
        
        
        data <- readRDS("shinydata.rds") %>%
            
            select(-"Number of Stocks", -"Aquaculture Alternative") %>% 
            #fitler(!is.na())
            
            {if ((input$assess) == TRUE) dplyr::filter(., SOURCE == "RAM")
                
                else dplyr::filter(., SOURCE == "ALL")
                
            } %>% 
          
          dplyr::filter(., is.na(`IUCN Status`) |
                          `IUCN Status` >= input$stat_slide[1] &
                          `IUCN Status` <= input$stat_slide[2]) %>% 
          
            
        #    {if 
        #        #(is.null(input$aqua)) . 
        #        ((input$conserv) == TRUE)    dplyr::filter(., !is.na(`IUCN Status`)) %>% 
        #                                     dplyr::filter(., `IUCN Status` >= input$stat_slide[1] &
        #                                                      `IUCN Status` <= input$stat_slide[2])
        #        else (dplyr::filter(., is.na(`IUCN Status`) |
      #                                  `IUCN Status` >= input$stat_slide[1] &
      #                                   `IUCN Status` <= input$stat_slide[2])
      #                                  )
      #      } %>% 
            
            select(-SOURCE) %>% 
            
            dplyr::select(-"Scientific Name", -"Percent Industrial") %>% 
            
            mutate(`Revenue (Thousands USD)` = signif(round((`Revenue (Thousands USD)`/1000),0), 2)) %>% 
            mutate(av_landed_biomass = signif(round(av_landed_biomass,0),2)) %>% 
            mutate(av_farmed_biomass = signif(round(av_farmed_biomass,0),2)) %>% 
            
            filter(`Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` >= input$FMSY[1] & 
                       `Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` <= input$FMSY[2]) %>% 
            filter(`Proportion of Fishereis Being Overfished (BMSY < 1)` >= input$BMSY[1] &
                       `Proportion of Fishereis Being Overfished (BMSY < 1)` <= input$BMSY[2]) %>% 
           # {if 
           #     #(is.null(input$aqua)) . 
           #      ((input$aqua) == "All")    dplyr::filter(., `Aquaculture Alternative` == "Yes" | `Aquaculture Alternative` == "No")
           # else (dplyr::filter(., `Aquaculture Alternative` == input$aqua))
           # } %>% 
            select(-menu_name) %>% 
            rename("Number of Countries Fishing" = country_num,
                   "Average Yearly Caught (Tonnes)" = av_landed_biomass,
                   "Average Yearly Farmed (Tonnes)" = av_farmed_biomass) %>% 
            arrange(dplyr::desc(!!rlang::sym(input$arrange))) %>% 
            mutate(`Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` = percent(`Proportion of Fisheries Experiencing Overfishing (FMSY > 1)`,0),
                   `Proportion of Fishereis Being Overfished (BMSY < 1)` = percent(`Proportion of Fishereis Being Overfished (BMSY < 1)`,0)#,
                   #`Percent Industrial` = percent(`Percent Industrial`)
                   ) %>% 
            select("Common Name", 
                   "Number of Countries Fishing",
                   "Proportion of Fisheries Experiencing Overfishing (FMSY > 1)",
                   "Proportion of Fishereis Being Overfished (BMSY < 1)",
                   "Revenue (Thousands USD)",
                   "Average Yearly Caught (Tonnes)",
                   "Average Yearly Farmed (Tonnes)",
                   "IUCN Status"
                   ) %>% 
            
            mutate(
                `Average Yearly Caught (Tonnes)` = formatC(`Average Yearly Caught (Tonnes)`, format="d", big.mark=","),
                `Average Yearly Farmed (Tonnes)` = formatC(`Average Yearly Farmed (Tonnes)`, format="d", big.mark=","),
                `Revenue (Thousands USD)` = formatC(`Revenue (Thousands USD)`, format="d", big.mark=","))
        
        
        
        
        # END OF DF FORMATTING
        ,
       
    
        col.names = c("Common Name",
                      #"Scientific Name",
                      "Number of Countries Fishing",
                      "Fishing Pressure",
                      "Fishery Biomass", 
                      "Revenue (Millions USD)",
                      "Average Yearly Caught (Tonnes)",
                      "Average Yearly Farmed (Tonnes)",
                      "IUCN Status"
                      #"Aquaculture Alternative"
                      #"Percent Industrial"
        ),
        
        
      
        
        align = c("l", "c", "c", "c", "c", "c", "c", "c"),
        list(
            
            
            
            `Number of Countries Fishing` = formatter(
                "span",
                style = ~ formattable::style(
                    color = case_when(
                        is.na(`Number of Countries Fishing`) == TRUE ~ "white",
                        is.na(`Number of Countries Fishing`) == FALSE ~ "black"
                    ))),
            
            
            `Revenue (Thousands USD)` = formatter(
                "span",
                style = ~ formattable::style(
                    color = case_when(
                        `Revenue (Thousands USD)` == "NA" ~ "white",
                        is.na(`Revenue (Thousands USD)`) == FALSE ~ "black"
                    ))),
            
            `Average Yearly Farmed (Tonnes)` = formatter(
                "span",
                style = ~ formattable::style(
                    color = case_when(
                        `Average Yearly Farmed (Tonnes)` == "NA" ~ "white",
                        is.na(`Average Yearly Farmed (Tonnes)`) == FALSE ~ "black"
                    ))),
            
            `Average Yearly Caught (Tonnes)` = formatter(
                "span",
                style = ~ formattable::style(
                    color = case_when(
                        `Average Yearly Caught (Tonnes)` == "NA" ~ "white",
                        is.na(`Average Yearly Caught (Tonnes)`) == FALSE ~ "black"
                    ))),
            
            
            `IUCN Status`  = formatter(
                "span",
                style = ~ formattable::style(
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
            
            
            #`Causes or is Effected by Bycatch` = color_tile("#DC143C", "#3CB371"),
            
           # `Percent Industrial` = color_tile("#c8f6ff", "#54e3ff"),
            
            
            
            `Aquaculture Alternative`  = formatter(
                "span",
                style = ~ formattable::style(
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
                style = ~ formattable::style(
                    color = "transparent",
                        
                        #case_when(
                        #is.na(`Proportion of Fisheries Experiencing Overfishing (FMSY > 1)`) == TRUE ~ "white",
                        #is.na(`Proportion of Fisheries Experiencing Overfishing (FMSY > 1)`) == FALSE ~ "black"
                    #),
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
                style = ~ formattable::style(
                    color ="transparent", 
                      #  case_when(
                      #  is.na(`Proportion of Fishereis Being Overfished (BMSY < 1)`) == TRUE ~ "white",
                      #  is.na(`Proportion of Fishereis Being Overfished (BMSY < 1)`) == FALSE ~ "black"
                    #),
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
    
#### groupings Output ####    

    output$groupings <- renderFormattable({formattable(
        
        
        data <- readRDS("shinydata.rds") %>%
            
            select(-"Number of Stocks", -"Aquaculture Alternative", -"Scientific Name") %>% 
            #fitler(!is.na())
            
            {if ((input$assess_group) == TRUE) dplyr::filter(., SOURCE == "RAM")
                
                else dplyr::filter(., SOURCE == "ALL")
                
            } %>% 
            
            select(-SOURCE, -"Percent Industrial") %>% 
            
            mutate(`Revenue (Thousands USD)` = as.integer(`Revenue (Thousands USD)`),
                `Revenue (Thousands USD)` = signif(round((`Revenue (Thousands USD)`/1000),0), 2)) %>% 
            mutate(av_landed_biomass = signif(round(av_landed_biomass,0) ,2)) %>% 
            mutate(av_farmed_biomass = signif(round(av_farmed_biomass,0) ,2))  %>% 
            mutate(`Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` = percent(`Proportion of Fisheries Experiencing Overfishing (FMSY > 1)`,0),
                   `Proportion of Fishereis Being Overfished (BMSY < 1)` = percent(`Proportion of Fishereis Being Overfished (BMSY < 1)`,0)
            ) %>% 
            
            
            filter(menu_name == input$groupings) %>% 
            
            rename("Number of Countries Fishing" = country_num,
                   "Average Yearly Caught (Tonnes)" = av_landed_biomass,
                   "Average Yearly Farmed (Tonnes)" = av_farmed_biomass) %>% 
            
            arrange(dplyr::desc(!!rlang::sym(input$arrange_group))) %>% 
            
            select("Common Name", 
                   "Number of Countries Fishing",
                   "Proportion of Fisheries Experiencing Overfishing (FMSY > 1)",
                   "Proportion of Fishereis Being Overfished (BMSY < 1)",
                   "Revenue (Thousands USD)",
                   "Average Yearly Caught (Tonnes)",
                   "Average Yearly Farmed (Tonnes)",
                   "IUCN Status"
            ) %>% 
            
            mutate(
                `Average Yearly Caught (Tonnes)` = formatC(`Average Yearly Caught (Tonnes)`, format="d", big.mark=","),
                `Average Yearly Farmed (Tonnes)` = formatC(`Average Yearly Farmed (Tonnes)`, format="d", big.mark=","),
                `Revenue (Thousands USD)` = formatC(`Revenue (Thousands USD)`, format="d", big.mark=",")),
        
        
        col.names = c("Common Name",
                      #"Scientific Name",
                      "Number of Countries Fishing",
                      "Fishing Pressure",
                      "Fishery Biomass", 
                      "Revenue (Millions USD)",
                      "Average Yearly Caught (Tonnes)",
                      "Average Yearly Farmed (Tonnes)",
                      "IUCN Status"
                      #"Aquaculture Alternative"
                      #"Percent Industrial"
        ),
        
        align = c("l", "c", "c", "c", "c", "c", "c", "c"),
        list(
            
            `Number of Countries Fishing` = formatter(
                "span",
                style = ~ formattable::style(
                    color = case_when(
                        is.na(`Number of Countries Fishing`) == TRUE ~ "white",
                        is.na(`Number of Countries Fishing`) == FALSE ~ "black"
                    ))),
            
            
            `Revenue (Thousands USD)` = formatter(
                "span",
                style = ~ formattable::style(
                    color = case_when(
                        `Revenue (Thousands USD)` == "NA" ~ "white",
                        is.na(`Revenue (Thousands USD)`) == FALSE ~ "black"
                    ))),
            
            `Average Yearly Farmed (Tonnes)` = formatter(
                "span",
                style = ~ formattable::style(
                    color = case_when(
                        `Average Yearly Farmed (Tonnes)` == "NA" ~ "white",
                        is.na(`Average Yearly Farmed (Tonnes)`) == FALSE ~ "black"
                    ))),
            
            `Average Yearly Caught (Tonnes)` = formatter(
                "span",
                style = ~ formattable::style(
                    color = case_when(
                        `Average Yearly Caught (Tonnes)` == "NA" ~ "white",
                        is.na(`Average Yearly Caught (Tonnes)`) == FALSE ~ "black"
                    ))),
            
            
            
            `IUCN Status`  = formatter(
                "span",
                style = ~ formattable::style(
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
            
            
            #`Causes or is Effected by Bycatch` = color_tile("#DC143C", "#3CB371"),
            
            # `Percent Industrial` = color_tile("#c8f6ff", "#54e3ff"),
            
            
            
           # `Aquaculture Alternative`  = formatter(
           #     "span",
           #     style = ~ formattable::style(
           #         display = "inline-block",
           #         padding = "0 4px",
           #         "border-radius" = "4px",
           #         "width" = "60px",
           #         "background-color" = case_when(
           #             `Aquaculture Alternative` == "Yes" ~ "green",
           #             `Aquaculture Alternative` == "No"  ~"red"
           #         )
           #     )),
            
            `Proportion of Fisheries Experiencing Overfishing (FMSY > 1)` = formatter(
                "span",
                style = ~ formattable::style(
                    color = "transparent",
                  #      case_when(
                   #     is.na(`Proportion of Fisheries Experiencing Overfishing (FMSY > 1)`) == TRUE ~ "white",
                   #     is.na(`Proportion of Fisheries Experiencing Overfishing (FMSY > 1)`) == FALSE ~ "black"
                  #  ),
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
                style = ~ formattable::style(
                    color = "transparent",
                 #       case_when(
                 #       is.na(`Proportion of Fishereis Being Overfished (BMSY < 1)`) == TRUE ~ "white",
                 #       is.na(`Proportion of Fishereis Being Overfished (BMSY < 1)`) == FALSE ~ "black"
                 #   ),
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
    
    
#### End of App ####
}
shinyApp(ui = ui, server = server)