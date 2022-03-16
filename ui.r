# Define UI for app that draws a histogram ----
pacman::p_load(ggplot2, readr, ggformula, shiny, ggiraph, RColorBrewer, data.table, cowplot, googlesheets4, MASS, estimatr)
ui <- fluidPage(
  
  # App title ----
  titlePanel("LRI incident cases per 1000 in children under 5"),
      
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs", id = "tabselected",
                  
                  tabPanel("Map",   # Sidebar layout with input and output definitions
                             
                            # Sliders
                            fluidRow(
                             column(6,
                                    sliderInput('year_map', 'Year', 
                                                min=1990, max=2019, value=2019, sep = "",
                                                step=5, round=0)),
                             column(6,
                                    sliderInput('ui_level', 'UI level', 
                                                min=5, max=95, value=95, 
                                                step=90, round=0))
                             
                           ),
                           # Output: Interactive world map
                           girafeOutput("distPlot")

    ),
    tabPanel("Scatter",
             fluidRow(
               column(6,
                      sliderInput(inputId = 'year_scatter', 
                                  label = 'Year', 
                                  min=1990, max=2019, value=2019, sep = "", 
                                  step=5, round=0)),
               column(6,
                      sliderInput(inputId = 'ui_level_scatter', 
                                  label = 'UI level', 
                                  min=5, max=95, value=95, 
                                  step=5, round=0))),
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 # Input: Slider for the degree of polynomial fit
                 # Input: Checkbox for whether the fit is robust or not
                 # Input: Which covariate to plot on the x axis
                 
                 sliderInput(inputId = "fit",
                             label = c("Degree of polynomial"),
                             min=1, max=5, value=1, 
                             step=1, round=0
                 ),
                 checkboxInput(inputId = "robust",
                             label = c("Robust"),
                             value = FALSE
                 ),
                 radioButtons(inputId = "covariate",
                              label = c("Covariate"),
                              choices = c("Hib3 vaccination", "PCV3 vaccination")
                 )
               ),
               mainPanel (
                 plotOutput(outputId = "scatterPlot",
                            hover = hoverOpts("plot_hover", delay = 20, delayType = "debounce")),
                 uiOutput("hover_info")
               )
             )
             
             
    )
    )
  )
)

