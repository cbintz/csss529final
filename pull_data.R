#####################################################################
## Author: Corinne Bintz & Rose Bender                             ##
## Description: Pull data for Final shiny project CS&SS 569        ##
##                                                                 ##
#####################################################################
## clean working environment
rm(list=ls())
## runtime configuration
if (Sys.info()["sysname"] == "Linux") {
  j <- "/home/j/"
  h <- "~/"
} else {
  j <- "J:/"
  h <- "H:/"
}

# Directories
repo_dir <- file.path(h, "00_repos", "csss539final")

## Source Functions
invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))

locs <- get_location_metadata(gbd_round_id = 6, decomp_step = 'step5', location_set_id = 35)
locs <- locs[level==3]
location_ids <- locs$location_id
years <- c(1990, 1995, 2000, 2005, 2010, 2015, 2019)
# rate and incidence
# under 5
# level 3 locations
lri_df <- get_outputs('cause', cause_id = 322, gbd_round_id = 6, year_id = years, metric_id = 3, measure_id = 6, age_group_id = 1, location_id = location_ids, decomp_step = 'step5')[,-c('age_group_id', 'age_group_name')]
setnames(lri_df, c("val", "upper", "lower"), c("mean_value_lri", "upper_value_lri", "lower_value_lri"))

hib_df <- get_covariate_estimates(covariate_id = 47, year_id = years, location_id = location_ids, gbd_round_id = 6, decomp_step = 'step4')[,-c('age_group_id', 'age_group_name', 'covariate_id', 'covariate_name_short', 'model_version_id')]
setnames(hib_df, c("mean_value", "upper_value", "lower_value"), c("mean_value_hib", "upper_value_hib", "lower_value_hib"))

PCV3_df <- get_covariate_estimates(covariate_id = 210, year_id = years, location_id = location_ids, gbd_round_id = 6, decomp_step = 'step4')[,-c('age_group_id', 'age_group_name', 'covariate_id', 'covariate_name_short', 'model_version_id')]
setnames(PCV3_df, c("mean_value", "upper_value", "lower_value"), c("mean_value_pcv3", "upper_value_pcv3", "lower_value_pcv3"))

lri_hib <- merge(lri_df, hib_df, by = c('location_id', 'sex_id', 'year_id', 'location_name', 'sex'))
lri_hib_pcv3_df <- merge(lri_hib, PCV3_df, by = c('location_id', 'sex_id', 'year_id', 'location_name', 'sex'))
lri_hib_pcv3_df <- merge(lri_hib_pcv3_df, locs[,.(location_id, ihme_loc_id, super_region_name)], by= 'location_id')

write.csv(lri_hib_pcv3_df, paste0(repo_dir, "/final_shiny_df.csv"), row.names = FALSE)
