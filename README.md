# Lower Respiratory Infections Incidence Shiny

This repository contains the scripts needed to create a R Shiny exploring the incidence of Lower Respiratory Infections (LRI) in children under 5. The overall goal of this Shiny is to allow the user to visualize trends in LRI incidence estimates as they relate to geography, time, and vaccination coverage, as well as the uncertainty around these estimates. The data shown in these visualizations come from: Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Disease and Injury Burden 1990-2019. Seattle, United States of America: Institute for Health Metrics and Evaluation (IHME), 2020.

## Sections
### Map
The objective of this map is to show spatial patterns of LRI in children under 5, with user-specified information on uncertainty and time. The fill color is GBD estimates of incident cases of LRI per 1000 population per year in this age group. The lefthand map is the mean estimate. The righthand map shows the bounds of the uncertainty interval, and can be toggled to the 5% or the 95% bound, to show the potential best and worst scenarios. The top sliders change the year and the uncertainty bound. The scales stay constant with changing settings, to facilitate comparison. Hovering with the mouse shows the country name and incidence estimate.

### Scatter
The objective of this scatter is to show the relationship between LRI incidence and selected covariates, PCV3 vaccination and Hib3 vaccination, which protect against certain etiologies of LRI. The x-axis is the GBD estimate of proportion of the infant population covered by these vaccines, and the y-axis is GBD estimates of incident cases of LRI per 1000 population per year. Each point represents a single country, colored by super-region, with a hover feature to allow for easier visualization of select countries or outliers. The sidebar allows the user to explore these relationships with different degrees of polynomial fit, including an option for robust fitting. Like for the map, the top sliders change the year and the uncertainty bound while scales stay constant.

