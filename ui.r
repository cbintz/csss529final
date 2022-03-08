# Define UI for app that draws a histogram ----
pacman::p_load(ggplot2, readr, ggformula, shiny, ggiraph, RColorBrewer, data.table, cowplot, googlesheets4)
ui <- fluidPage(
  
  # App title ----
  titlePanel("Incidence of LRI in children under 5 years old"),
      
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
                                            choices = c("Linear", "GAM" ,"Loess", "Quadratic", "5th order polynomial")
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
                                    sliderInput('year_scatter', 'Year', 
                                                min=1990, max=2019, value=2019, sep = "", 
                                                step=5, round=0)))
                 
      
                  ),
                  tabPanel("Map",   # Sidebar layout with input and output definitions
                             
                            # Sliders
                            fluidRow(
                             column(6,
                                    sliderInput('year_map', 'Year', 
                                                min=1990, max=2019, value=2019, sep = "",
                                                step=5, round=0)),
                             column(6,
                                    sliderInput('ui_level', 'UI Level', 
                                                min=5, max=95, value=50, 
                                                step=45, round=0))
                             
                           ),
                           # Output: Interactive world map
                           girafeOutput("distPlot")

    )
    )
  )
)

