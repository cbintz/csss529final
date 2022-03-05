# Define UI for app that draws a histogram ----
library(ggplot2,readr,ggformula)
ui <- fluidPage(
  
  # App title ----
  titlePanel("Incidence of LRI under 5 years old by HIB or PCV3 vaccination rates"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the type of fit
      # options: LM, GAM, Loess, Quadratic, 5th order polynoial
      
      radioButtons(inputId = "fit",
                  label = c("Data fit type:"),
                  choices = c("LM", "GAM" ,"Loess", "Quadratic", "5th order polynomial")
      ),
      radioButtons(inputId = "covariate",
                   label = c("Covariate:"),
                   choices = c("Hib vaccination", "Pcv3 vaccination")
      ),
      radioButtons(inputId = "ci",
                   label = c("Confidence level:"),
                   choices = c(0.85,0.90,0.95)
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "scatterPlot")
      
    )
  )
)
