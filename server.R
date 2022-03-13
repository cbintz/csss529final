# Define server logic required to draw a scatt plot ----
pacman::p_load(ggplot2, readr, ggformula, shiny, ggiraph, RColorBrewer, data.table, cowplot, googlesheets4)

# Load in data from Google Sheets
gs4_deauth() # Don't require authentication, it is a public sheet
data <- read_sheet("https://docs.google.com/spreadsheets/d/1odaGK89U5gNiO-GtyTWY6rfhq3WZ1fGteVhDabPAKl4/edit?usp=sharing")
setDT(data)

# Multiply rates by 1000 to be per 1000
cols_transform <- c("mean_value_lri", "upper_value_lri", "lower_value_lri")
data[, (cols_transform) := lapply(.SD, "*", 1000), .SDcols = cols_transform]

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
    # subset to year of interest
    data <- data[year_id == input$year_scatter]    
    
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
      legend.position = "bottom",
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
    req(input$ui_level_scatter)
 if(input$covariate == "Hib3 vaccination") {
      column <- "mean_value_hib"
    } else if (input$covariate == "PCV3 vaccination"){
      column <- "mean_value_pcv3"
    }

    if(input$fit == "Linear"){
      ggplot(data, aes(x=get(column), y=mean_value_lri)) +
        geom_point(size=2, shape=21, alpha = 0.5, aes(fill = super_region_name))+
        scale_fill_manual(values = c(brewer.pal(7, "RdBu")))+
        geom_smooth(aes(x=get(column), y=mean_value_lri), method = 'lm',level=input$ui_level_scatter/100, color = "black") +
        xlab(paste(input$covariate, "proportion")) + ylab("LRI incidence rate per 1000")+
        guides(fill = guide_legend(nrow = 3))+
        goldenScatterCAtheme
    }
  
    else if (input$fit == "GAM"){
      ggplot(data, aes(x=get(column), y=mean_value_lri)) +
        geom_point(size=2, shape=21, alpha = 0.5, aes(fill = super_region_name))+
        scale_fill_manual(values = c(brewer.pal(7, "RdBu")))+
        geom_smooth(aes(x=get(column), y=mean_value_lri), method = 'gam',level=input$ui_level_scatter/100, color = "black")+
        xlab(paste(input$covariate, "proportion")) + ylab("LRI incidence rate per 1000")+
        guides(fill = guide_legend(nrow = 3))+
        goldenScatterCAtheme
    }
    
    else if (input$fit == "Loess"){
      ggplot(data, aes(x=get(column), y=mean_value_lri)) +
        geom_point(size=2, shape=21, alpha = 0.5, aes(fill = super_region_name))+
        scale_fill_manual(values = c(brewer.pal(7, "RdBu")))+
        geom_smooth(aes(x=get(column), y=mean_value_lri), method = 'loess',level=input$ui_level_scatter/100, color = "black")+
        xlab(paste(input$covariate, "proportion")) + ylab("LRI incidence rate per 1000")+
        guides(fill = guide_legend(nrow = 3))+
        goldenScatterCAtheme
    }
    
    else if (input$fit == "Quadratic"){
      ggplot(data, aes(x=get(column), y=mean_value_lri)) +
        geom_point(size=2, shape=21, alpha = 0.5, aes(fill = super_region_name))+
        scale_fill_manual(values = c(brewer.pal(7, "RdBu")))+
        geom_smooth(aes(x=get(column), y=mean_value_lri), method = 'lm', formula = y~x+I(x^2),level=input$ui_level_scatter/100, color = "black")+
        xlab(paste(input$covariate, "proportion")) + ylab("LRI incidence rate per 1000")+
        guides(fill = guide_legend(nrow = 3))+
        goldenScatterCAtheme
    }
    
    else if (input$fit == "5th order polynomial"){
      ggplot(data, aes(x=get(column), y=mean_value_lri)) +
        geom_point(size=2, shape=21, alpha = 0.5, aes(fill = super_region_name))+
        scale_fill_manual(values = c(brewer.pal(7, "RdBu")))+
        geom_smooth(aes(x=get(column), y=mean_value_lri), method = 'lm', formula = y ~ poly(x, 5),level=input$ui_level_scatter/100, color = "black")+
        xlab(paste(input$covariate, "proportion")) + ylab("LRI incidence rate per 1000")+
        guides(fill = guide_legend(nrow = 3))+
        goldenScatterCAtheme
    }
    
  })
  output$hover_info <- renderUI({
    if(input$covariate == "Hib3 vaccination") {
      column <- "mean_value_hib"
      label <- "HIB3 Vaccination proportion"
    } else if (input$covariate == "PCV3 vaccination"){
      column <- "mean_value_pcv3"
      label <- "PCV3 Vaccination proportion"
    }
    
    hover <- input$plot_hover
    point <- nearPoints(data, hover, threshold = 5, maxpoints = 1, addDist = TRUE, xvar = column, yvar = "mean_value_lri")
    
     if (nrow(point) == 0) return(NULL)
     
     left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
     top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
     
     left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)-300
     top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
     style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                   "left:", 400, "px; top:", 200, "px; wdith:", 200, "px;")
      
     wellPanel(
       style = style,
       p(HTML(paste0("<br> <b>", point$location_name, "</b> </br>",  "<b> <br> Incidence of LRI: </b>", sprintf("%.1f", point$mean_value_lri), "</br>","<br> <b>",label, ": </b>", sprintf("%.1f",point[[column]]), "</br>")))
     )
  })
  
  # Define the function for building world map
  # Inputs: world data, input data with LRI incidence
  # Confidence interval level selected in R Shiny app
  
  worldMaps <- function(df, world_data, input, legend.p = "none"){
  
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
      capt <- paste0("Mean LRI incidence estimate")
    } else if (input$ui_level == 95){
      column <- "upper_value_lri"
      capt <- paste0("95th percentile LRI incidence estimate")
    }
        
    # Specify the plot for the world map
    g <- ggplot() + geom_map_interactive(data = plotdf, map = world_data, color = 'gray70', 
                                         (aes(long, lat, map_id = region, fill = get(column), 
                                              tooltip = sprintf("%s<br/>%s", region, round(get(column), 1))))) +
      scale_fill_gradientn(colours = rev(brewer.pal(7, "RdBu")), na.value = 'white', trans = "log", label = function(x) round(x, 0), 
        limits = c(min(df$lower_value_lri),max(df$upper_value_lri))) +
      labs(title = NULL, x = NULL, y = NULL, caption = capt) +
      my_theme()
    
    return(g)
  }
  
  # Create the interactive world map
  output$distPlot <- renderGirafe({
    # Subset to year of interest
    data <- data[year_id == input$year_map]
    # Create the plots with each uncertainty level
    ggmean <- worldMaps(data, world_data, input = list(ui_level = 50, year = input$year))
    ggadjust <- worldMaps(data, world_data, input)
    legend_g <- get_legend(worldMaps(data, world_data, input, "right"))
    girafe(ggobj = plot_grid(plot_grid(ggmean, ggadjust), legend_g, ncol = 2, rel_widths = c(1, .1)), width_svg = 12, height_svg = 3)
  })

  
}

