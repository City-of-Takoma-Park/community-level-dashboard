library(httr)
library(jsonlite)
library(tidyverse)
library(cdccovidplotting)

zip_direct <- fromJSON(txt = "https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MDCOVID19_MASTER_ZIP_CODE_CASES/FeatureServer/0/query?where=ZIP_CODE%20%3D%20%2720912%27&outFields=*&outSR=4326&f=json")

zip_fields <- zip_direct$features$attributes

write_rds(zip_fields, "./data/zip_fields.rds")

zip_vax <- jsonlite::fromJSON("https://services.arcgis.com/njFNhDsUCentVYJW/arcgis/rest/services/MD_COVID_ZIPCODES_Vaccinations/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")

zip_vax_data <- zip_vax$features$attributes

write_rds(zip_vax_data, "./data/zip_vax.rds")

state.name

chng_vals <- function(df, colname, chngstr){
  
  pct_chng <- paste0(chngstr, "_pct")
  
  df %>%
    mutate(!!sym(chngstr) := {{colname}} - lag({{colname}}),
           !!sym(pct_chng) := round(!!sym(chngstr) * 100 / lag({{colname}}), 2))
}

# read in community level data

read_process_save <- function(statename){
  community_level <- read.csv(paste0("https://data.cdc.gov/resource/3nnm-4jni.csv?state=", statename))
  
  community_level <- community_level %>%
    mutate(date_updated = as.Date(date_updated),
           date_format = format(date_updated, "%B %d, %Y"),
           date_updated = format(date_updated, "%m-%d")) %>%
    group_by(county) %>%
    arrange(date_updated) %>%
    chng_vals(colname = covid_hospital_admissions_per_100k, "chng_hospadmit") %>%
    chng_vals(colname = covid_inpatient_bed_utilization, "chng_inpatient") %>%
    chng_vals(covid_cases_per_100k, "chng_covid")
  
  saveRDS(community_level, paste0("./data/community_level_", statename, ".rds"))
}

walk(state.name, read_process_save)

bind_all <- map_dfr(grep("community_level_", dir("./data"), value = T), ~ readRDS(paste0("./data/", .x)))

saveRDS(bind_all, "./data/all_community_level.rds")

# md_covid <- cdccovidplotting::read_process_commlevels("Maryland")