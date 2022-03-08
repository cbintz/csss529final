# Define server logic required to draw a scatt plot ----
pacman::p_load(ggplot2, readr, ggformula, shiny, ggiraph, RColorBrewer, data.table, cowplot, googlesheets4)

# Load in data from Google Sheets
gs4_deauth() # Don't require authentication, it is a public sheet
data <- read_sheet("https://docs.google.com/spreadsheets/d/1odaGK89U5gNiO-GtyTWY6rfhq3WZ1fGteVhDabPAKl4/edit?usp=sharing")
setDT(data)

# Multiply rates by 100K to be per 100K
cols_transform <- c("mean_value_lri", "upper_value_lri", "lower_value_lri")
data[, (cols_transform) := lapply(.SD, "*", 100000), .SDcols = cols_transform]

# Load in world data with geographical coordinates directly from ggplot2
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

# Start server function
server <- function(input, output) {

  output$scatterPlot <- renderPlot({
    yr_id <- input$year
    data <- data[year_id == yr_id]
    
    
    goldenScatterCAtheme <- theme(
      ## Removes main plot gray background
      panel.background = element_rect(fill = "white"), 
      
      ## Golden rectangle plotting area (leave out for square)
      aspect.ratio = ((1 + sqrt(5))/2)^(-1), 
      
      ## All axes changes
      axis.ticks.length = unit(0.5, "char"),  # longer ticks
      
      ## Horizontal axis changes
      axis.line.x.top = element_line(size = 0.2),    # thinner axis lines
      axis.line.x.bottom = element_line(size = 0.2), # thinner axis lines
      axis.ticks.x = element_line(size = 0.2),       # thinner ticks
      axis.text.x = element_text(color = "black", size = 12),
      ## match type of axis labels and titles
      axis.title.x = element_text(size = 12,
                                  margin = margin(t = 7.5, r = 0, b = 0, l = 0)),
      ## match type; pad space between title and labels
      
      ## Vertical axis changes
      axis.ticks.y = element_blank(), # no y axis ticks (gridlines suffice)
      axis.text.y = element_text(color = "black", size = 12,
                                 margin = margin(t = 0, r = -4, b = 0, l = 0)),
      ## match type of axis labels and titles, pad
      axis.title.y = element_text(size = 12,
                                  margin = margin(t = 0, r = 7.5, b = 0, l = 0)),
      ## match type of axis labels and titles, pad
      
      ## Legend
      legend.key = element_rect(fill = NA, color = NA),
      ## Remove unhelpful gray background
      
      ## Gridlines (in this case, horizontal from left axis only
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray45", size = 0.2),
      
      ## Faceting (small multiples)
      strip.background = element_blank(),
      ## Remove unhelpful trellis-like shading of titles
      strip.text.x = element_text(size=12),  # Larger facet titles
      strip.text.y = element_blank(),        # No titles for rows of facets
      strip.placement = "outside",           # Place titles outside plot
      panel.spacing.x = unit(1.25, "lines"), # Horizontal space b/w plots
      panel.spacing.y = unit(1, "lines")     # Vertical space b/w plots
    )
    req(input$fit)
    req(input$covariate)
    req(input$ci)

 if(input$covariate == "Hib3 vaccination") {
      column <- "mean_value_hib"
    } else if (input$covariate == "PCV3 vaccination"){
      column <- "mean_value_pcv3"
    }

    if(input$fit == "LM"){
      ggplot(data, aes(x=get(column), y=mean_value_lri)) +
        geom_point(size=2, shape=24)+
        geom_smooth(aes(x=get(column), y=mean_value_lri), method = 'lm',level=as.numeric(input$ci)) +
        xlab(paste(input$covariate, "rate")) + ylab("LRI incidence rate")+
        goldenScatterCAtheme
    }
  
    else if (input$fit == "GAM"){
      ggplot(data, aes(x=mean_value_hib, y=mean_value_lri)) +
        geom_point(size=2, shape=24) +
        geom_smooth(aes(x=mean_value_hib, y=mean_value_lri), method = 'gam',level=as.numeric(input$ci))+
        xlab(paste(input$covariate, "rate")) + ylab("LRI incidence rate")+
        goldenScatterCAtheme
    }
    
    else if (input$fit == "Loess"){
      ggplot(data, aes(x=mean_value_hib, y=mean_value_lri)) +
        geom_point(size=2, shape=24) +
        geom_smooth(aes(x=mean_value_hib, y=mean_value_lri), method = 'loess',level=as.numeric(input$ci))+
        xlab(paste(input$covariate, "rate")) + ylab("LRI incidence rate")+
        goldenScatterCAtheme
    }
    
    else if (input$fit == "Quadratic"){
      ggplot(data, aes(x=mean_value_hib, y=mean_value_lri)) +
        geom_point(size=2, shape=24) +
        geom_smooth(aes(x=mean_value_hib, y=mean_value_lri), method = 'lm', formula = y~x+I(x^2),level=as.numeric(input$ci))+
        xlab(paste(input$covariate, "rate")) + ylab("LRI incidence rate")+
        goldenScatterCAtheme
    }
    
    else if (input$fit == "5th order polynomial"){
      ggplot(data, aes(x=mean_value_hib, y=mean_value_lri)) +
        geom_point(size=2, shape=24) +
        geom_smooth(aes(x=mean_value_hib, y=mean_value_lri), method = 'lm', formula = y ~ poly(x, 5),level=as.numeric(input$ci))+
        xlab(paste(input$covariate, "rate")) + ylab("LRI incidence rate")+
        goldenScatterCAtheme
    }
    
  })
  
  # Define the function for building world map
  # Inputs: world data, input data with LRI incidence
  # Confidence interval level selected in R Shiny app
  
  worldMaps <- function(df, world_data, input, legend.p = "none"){
    # Subset to the desired year
    yr_id <- input$year
    df <- df[year_id == yr_id]
    
    
    # Function for setting the aesthetics of the plot
    my_theme <- function () { 
      theme_bw() + theme(axis.title = element_blank(),
                         axis.text = element_blank(),
                         axis.ticks = element_blank(),
                         panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(),
                         panel.background = element_blank(), 
                         legend.position = legend.p,
                         legend.title = element_blank(),
                         text = element_text(size = 24),
                         panel.border = element_blank(), 
                         strip.background = element_rect(fill = 'white', colour = 'white'))
    }
    
    # Add the data the user wants to see to the geographical world data
    plotdf <- merge(world_data, df, by.x = "region", by.y = "location_name")
    if(input$ui_level == 5){
      column <- "lower_value_lri"
      capt <- paste0("5th percentile LRI incidence estimate")
    } else if (input$ui_level == 50){
      column <- "mean_value_lri"
      capt <- paste0("Mean LRI incdence estimate")
    } else if (input$ui_level == 95){
      column <- "upper_value_lri"
      capt <- paste0("95th percentile LRI incidence estimate")
    }
        
    # Specify the plot for the world map
    g <- ggplot() + geom_map_interactive(data = plotdf, map = world_data, color = 'gray70', 
                                         (aes(long, lat, map_id = region, fill = get(column), 
                                              tooltip = sprintf("%s<br/>%s", region, round(get(column), 1))))) +
      scale_fill_gradientn(colours = brewer.pal(5, "YlOrRd"), na.value = 'white', trans = "log", label = function(x) sprintf("%.2f", x), 
        limits = c(min(df$lower_value_lri),max(df$upper_value_lri))) +
      labs(title = NULL, x = NULL, y = NULL, caption = capt) +
      my_theme()
    
    return(g)
  }
  
  # Create the interactive world map
  output$distPlot <- renderGirafe({
    # Create the plots with each uncertainty level
    ggmean <- worldMaps(data, world_data, input = list(ui_level = 50, year = input$year))
    ggadjust <- worldMaps(data, world_data, input)
    legend_g <- get_legend(worldMaps(data, world_data, input, "right"))
    girafe(ggobj = plot_grid(plot_grid(ggmean, ggadjust), legend_g, ncol = 2, rel_widths = c(1, .1)), width_svg = 12, height_svg = 4)
  })
  
}