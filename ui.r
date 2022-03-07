# Define UI for app that draws a histogram ----
pacman::p_load(ggplot2, readr, ggformula, shiny, ggiraph, RColorBrewer, data.table)
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
                             )),
                 
      
                  ),
                  tabPanel("Map",   # Sidebar layout with input and output definitions
                           sidebarLayout(
                             
                             # Sidebar panel for inputs 
                             sidebarPanel(
                               
                               # First input: Type of data
                               selectInput(inputId = "ui_level",
                                           label = "Choose the uncertainty level you want to see:",
                                           choices = list("lower_value_lri", "mean_value_lri", "upper_value_lri")),
                             ),
                             
                             # Main panel for displaying outputs
                             mainPanel(
                               
                               
                               # Hide errors
                               tags$style(type = "text/css",
                                          ".shiny-output-error { visibility: hidden; }",
                                          ".shiny-output-error:before { visibility: hidden; }"),
                               
                               # Output: interactive world map
                               girafeOutput("distPlot")
                               
                             )
                           ))
      
    )
    )
  
)
