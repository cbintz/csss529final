# Define server logic required to draw a histogram ----
library(ggplot2,readr,ggformula)
data <- read.csv("~/Desktop/final_shiny_bintzc_rbender/final_shiny_df.csv") # this needs to change
server <- function(input, output) {

  
  output$scatterPlot <- renderPlot({
    
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

    if(input$covariate == "Hib vaccination") {
    
    if(input$fit == "LM"){
      ggplot(data, aes(x=mean_value_hib, y=mean_value_lri)) +
        geom_point(size=2, shape=24)+
        geom_smooth(aes(x=mean_value_hib, y=mean_value_lri), method = 'lm',level=as.numeric(input$ci)) +
        xlab("HIB vaccination rate") + ylab("LRI incidence rate")+
        goldenScatterCAtheme
      
    }
  
    else if (input$fit == "GAM"){
      ggplot(data, aes(x=mean_value_hib, y=mean_value_lri)) +
        geom_point(size=2, shape=24) +
        geom_smooth(aes(x=mean_value_hib, y=mean_value_lri), method = 'gam',level=as.numeric(input$ci))+
        xlab("HIB vaccination rate") + ylab("LRI incidence rate")+
        goldenScatterCAtheme
    }
    
    else if (input$fit == "Loess"){
      ggplot(data, aes(x=mean_value_hib, y=mean_value_lri)) +
        geom_point(size=2, shape=24) +
        geom_smooth(aes(x=mean_value_hib, y=mean_value_lri), method = 'loess',level=as.numeric(input$ci))+
        xlab("HIB vaccination rate") + ylab("LRI incidence rate")+
        goldenScatterCAtheme
    }
    
    else if (input$fit == "Quadratic"){
      ggplot(data, aes(x=mean_value_hib, y=mean_value_lri)) +
        geom_point(size=2, shape=24) +
        geom_smooth(aes(x=mean_value_hib, y=mean_value_lri), method = 'lm', formula = y~x+I(x^2),level=as.numeric(input$ci))+
        xlab("HIB vaccination rate") + ylab("LRI incidence rate")+
        goldenScatterCAtheme
    }
    
    else if (input$fit == "5th order polynomial"){
      ggplot(data, aes(x=mean_value_hib, y=mean_value_lri)) +
        geom_point(size=2, shape=24) +
        geom_smooth(aes(x=mean_value_hib, y=mean_value_lri), method = 'lm', formula = y ~ poly(x, 5),level=as.numeric(input$ci))+
        xlab("HIB vaccination rate") + ylab("LRI incidence rate")+
        goldenScatterCAtheme
    }
   
    
    }
    
    else if(input$covariate == "Pcv3 vaccination") {
      
      if(input$fit == "LM"){
        ggplot(data, aes(x=mean_value_pcv3, y=mean_value_lri)) +
          geom_point(size=2, shape=24)+
          geom_smooth(aes(x=mean_value_pcv3, y=mean_value_lri), method = 'lm', level=as.numeric(input$ci))+
          xlab("PCV3 vaccination rate") + ylab("LRI incidence rate")+
          goldenScatterCAtheme
        
      }
      
      else if (input$fit == "GAM"){
        ggplot(data, aes(x=mean_value_pcv3, y=mean_value_lri)) +
          geom_point(size=2, shape=24) +
          geom_smooth(aes(x=mean_value_pcv3, y=mean_value_lri), method = 'gam',level=as.numeric(input$ci))+
          xlab("PCV3 vaccination rate") + ylab("LRI incidence rate")+
          goldenScatterCAtheme
      }
      
      else if (input$fit == "Loess"){
        ggplot(data, aes(x=mean_value_pcv3, y=mean_value_lri)) +
          geom_point(size=2, shape=24) +
          geom_smooth(aes(x=mean_value_pcv3, y=mean_value_lri), method = 'loess',level=as.numeric(input$ci))+
          xlab("PCV3 vaccination rate") + ylab("LRI incidence rate")+
          goldenScatterCAtheme
      }
      
      else if (input$fit == "Quadratic"){
        ggplot(data, aes(x=mean_value_pcv3, y=mean_value_lri)) +
          geom_point(size=2, shape=24) +
          geom_smooth(aes(x=mean_value_pcv3, y=mean_value_lri), method = 'lm', formula = y~x+I(x^2),level=as.numeric(input$ci))+
          xlab("PCV3 vaccination rate") + ylab("LRI incidence rate")+
          goldenScatterCAtheme
      }
      
      else if (input$fit == "5th order polynomial"){
        ggplot(data, aes(x=mean_value_pcv3, y=mean_value_lri)) +
          geom_point(size=2, shape=24) +
          geom_smooth(aes(x=mean_value_pcv3, y=mean_value_lri), method = 'lm', formula = y ~ poly(x, 5),level=as.numeric(input$ci))+
          xlab("PCV3 vaccination rate") + ylab("LRI incidence rate")+
          goldenScatterCAtheme
      }
      
      
    }
    
    
  })
  
}