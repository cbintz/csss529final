# Define UI for app that draws a histogram ----
pacman::p_load(ggplot2, readr, ggformula, shiny, ggiraph, RColorBrewer, data.table, cowplot)
ui <- fluidPage(
  
  # App title ----
  titlePanel("Incidence of LRI under 5 years old by Hib3 or PCV3 vaccination rates"),
      
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs", id = "tabselected",
                  tabPanel("Scatter",
                           # Sidebar layout with input and output definitions ----
                           sidebarLayout(
                     
                             # Sidebar panel for inputs ----
                             sidebarPanel(
                               
                               # Input: Slider for the type of fit
                               # options: LM, GAM, Loess, Quadratic, 5th order polynomial
                               
                               radioButtons(inputId = "fit",
                                            label = c("Data fit type:"),
                                            choices = c("LM", "GAM" ,"Loess", "Quadratic", "5th order polynomial")
                               ),
                               radioButtons(inputId = "covariate",
                                            label = c("Covariate:"),
                                            choices = c("Hib3 vaccination", "PCV3 vaccination")
                               ),
                               radioButtons(inputId = "ci",
                                            label = c("Confidence level:"),
                                            choices = c(0.85,0.90,0.95)
                               )
                             ),
                             mainPanel (
                               plotOutput(outputId = "scatterPlot")
                             )
                             ),
                           fluidRow(
                             column(10,
                                    h4("Year"),
                                    sliderInput('year', 'Year', 
                                                min=1990, max=2019, value=2019, sep = "", 
                                                step=5, round=0)))
                 
      
                  ),
                  tabPanel("Map",   # Sidebar layout with input and output definitions
                           sidebarLayout(
                             
                             # Output: interactive world map
                             mainPanel(width = 40,
                                       fluidRow(girafeOutput("distPlot"))),

                             fluidRow(
                               column(10,
                                      h4("UI level"),
                                      sliderInput('ui_level', 'UI Level', 
                                                  min=5, max=95, value=50, 
                                                  step=45, round=0)),
                               column(10,
                                      h4("Year"),
                                      sliderInput('year', 'Year', 
                                                  min=1990, max=2019, value=2019, sep = "",
                                                  step=5, round=0))
                               
                             )
                           
                             )
      
    )
    )
  )
)
