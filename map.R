pacman::p_load(magrittr, rvest, readxl, dplyr, maps, ggplot2, reshape2, shiny, ggiraph, RColorBrewer, data.table)

# Load in data
data <- read.csv("C:/Users/rbender1/Desktop/csss539final/final_shiny_df.csv") # this needs to change

# Next, we load in our world data with geographical coordinates directly from package *ggplot2*. These data 
# contain geographical coordinates of all countries worldwide, which we'll later need to plot the worldmaps.

library(maps)
library(ggplot2)
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)

# Change some of the names in world_data to match IHME names
old_names <- c("Antigua", "Barbuda", "Bolivia", "Brunei", "Ivory Coast", "Czech Republic", "Micronesia", "UK", 
               "Iran", "Nevis", "Saint Kitts", "South Korea", "Laos",
               "North Korea", "Russia", "Syria", "Trinidad", "Tobago", 
               "Taiwan", "Tanzania", "USA", "Grenadines", "Saint Vincent", 
               "Venezuela", "Vietnam", "Cape Verde",
               "Moldova", "Swaziland", "Republic of Congo", "Virgin Islands")
new_names <- c(rep("Antigua and Barbuda", 2), "Bolivia (Plurinational State of)", "Brunei Darussalam", "Côte d'Ivoire", "Czechia", "Micronesia (Federated States of)", "United Kingdom",
               "Iran (Islamic Republic of)", rep("Saint Kitts and Nevis", 2), "Republic of Korea", "Lao People's Democratic Republic",
               "Democratic People's Republic of Korea", "Russian Federation", "Syrian Arab Republic", rep("Trinidad and Tobago", 2), 
               "Taiwan (Province of China)", "United Republic of Tanzania", "United States of America", rep("Saint Vincent and the Grenadines", 2), 
               "Venezuela (Bolivarian Republic of)", "Viet Nam", "Cabo Verde",
               "Republic of Moldova", "Eswatini", "Congo", "United States Virgin Islands")

for (i in 1:length(old_names)){
  world_data$region[world_data$region == old_names[i]] <- new_names[i]
}

# Define the function for building world map
# Inputs: world data, input data with LRI incidence
# Confidence interval level selected in R Shiny app

worldMaps <- function(df, world_data, input){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }

  # Add the data the user wants to see to the geographical world data
  plotdf <- merge(world_data, df, by.x = "region", by.y = "location_name")
  setnames(plotdf, input, "Value")
  
  # Create caption with the data source to show underneath the map
  capt <- paste0("LRI Incidence estimation range")
  
  # Specify the plot for the world map
  g <- ggplot() + geom_map_interactive(data = plotdf, map = world_data, color = 'gray70', 
                                       (aes(long, lat, map_id = region, fill = Value, 
                                            tooltip = sprintf("%s<br/>%s", region, Value)))) +
    scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') +
    labs(title = NULL, x = NULL, y = NULL, caption = capt) +
    my_theme()

  return(g)
}

# Define the UI
ui = fluidPage(
  
  # App title
  titlePanel("Incidence of Lower Respiratory Infections"),
  
  # Sidebar layout with input and output definitions
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
  )
)

# Define the server
server = function(input, output) {
  
  # Create the interactive world map
  output$distPlot <- renderGirafe({
    ggiraph(code = print(worldMaps(data, world_data, input$ui_level)))
  })
}

# Finally, we can run our app by either clicking "Run App" in the top of our RStudio IDE, or by running
shinyApp(ui = ui, server = server)
