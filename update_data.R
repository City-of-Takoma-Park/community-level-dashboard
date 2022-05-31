library(httr)
library(jsonlite)
library(tidyverse)
library(cdccovidplotting)

state.name

state.name <- c(state.name,
                "Puerto Rico",
                "District of Columbia",
                "United States Virgin Islands",
                "Commonwealth of the Northern Mariana Islands",
                "Guam",
                "American Samoa")

# read in community level data
read_process_save <- function(statename){
  community_level <- cdccovidplotting::read_process_commlevels(statename)
  
  saveRDS(community_level, paste0("./data/community_level_", statename, ".rds"))
}

walk(state.name, read_process_save)

bind_all <- map_dfr(grep("community_level_", dir("./data"), value = T), ~ readRDS(paste0("./data/", .x)))

saveRDS(bind_all, "./data/all_community_level.rds")


state_process <- function(statedf){
  statedf %>%
    dplyr::filter(grepl("Maryland", state)) %>%
    dplyr::mutate(countyleg = gsub(" County", "", county)) %>%
    dplyr::group_by(county) %>%
    dplyr::arrange(desc(date_format)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    pctile_rank("covid_hospital_admissions_per_100k") %>%
    pctile_rank("covid_inpatient_bed_utilization") %>%
    pctile_rank("covid_cases_per_100k")
}

pctile_rank <- function(df, pctilevar){
  
  newvar <- paste0(pctilevar, "_pctile")
  rank <- paste0(pctilevar, "_rank")
  
  df %>%
    dplyr::mutate(!!dplyr::sym(newvar) := dplyr::percent_rank(!!dplyr::sym(pctilevar)),
                  !!dplyr::sym(rank) := dplyr::dense_rank(!!dplyr::sym(pctilevar)))
}

state_test_rank <- state_test_recent %>%
  state_process()

plot_state <- function(df, varplot, subtitle, showleg = F){
  
  # browser()
  var <- as.character(expr(!!varplot))[2]
  
  varpct <- paste0(var, "_pctile")
  varrank <- paste0(var, "_rank")
  maxrank <- max(df[[varrank]])
  # varpct <- sym(varpct)
  # varpct <- enquo(varpct)
  # 
  # varrank <- sym(varrank)
  # varrank <- enquo(varrank)
  
  plotly::plot_ly(df,
                  x = ~ countyleg,
                  y = varplot,
                  name = ~ countyleg,
                  type = "bar",
                  text = ~ glue::glue("Percentile-ranking: {round(df[[varpct]]*100, 1)}
                                      Ranking among counties: {df[[varrank]]}/{maxrank}"),
                  legendgroup = ~ countyleg,
                  showlegend = showleg,
                  color = ~ countyleg) %>%
    plotly::layout(xaxis = list(title = "County"),
           yaxis = list(title = ""),
           title = "") %>%
    plotlywrappers::subplot_title(subtitle)
  
}

subplots <- function(df, statename, dateformat){
  plotly::subplot(purrr::map2(rlang::list2(~ covid_cases_per_100k,
                                           ~ covid_inpatient_bed_utilization,
                                           ~ covid_hospital_admissions_per_100k), 
                              c("Cases per 100k",
                                "Percent hospital\nbeds due to\nCOVID",
                                "COVID hospitalizations\nper 100k"),
                              ~ {
                                
                                showleg <- dplyr::if_else(grepl("Cases", .y), T, F)
                                
                                plot_state(state_test_rank, .x, .y, showleg = showleg)
                              }), 
                  nrows = 3, shareX = T) %>%
    plotly::layout(title = glue::glue("{statename} counties by community-level components as of {dateformat}")) 
  
}


# 
# # md_covid <- cdccovidplotting::read_process_commlevels("Maryland")
# 
# subplots_keyvars <- function(countydf, varevals = list2(~ covid_cases_per_100k, ~ covid_hospital_admissions_per_100k, ~ covid_hospital_admissions_per_100k), subtitles = c("COVID cases\nper 100k", "Percent of beds\nfilled by\nCOVID patients", "COVID hospital\nadmits per\n100k"), basechngs = c("chng_covid", "chng_inpatient", "chng_hospadmit"), countyfilt= NULL, subrows = 2, ...){
#   
#   browser()
#   
#   # cdccovidplotting::plot_commlevels_lines(countydf = countydf, vareval = varevals[[1]], subtitle = subtitles[1], basechng = basechngs[1], countyfilt = countyfilt)
#   
#   
#   # funenviron <- environment()
#   
#   plots <- purrr::map(seq_along(subtitles), function(pos){
# 
#     # browser()
# 
#     plot_commlevels_lines(countydf = countydf, vareval = varevals[[pos]], subtitle = subtitles[pos], basechng = basechngs[pos], countyfilt = countyfilt, ...)
# 
#   })
#   
#   plotly::subplot(plots, nrows = subrows, shareX = T, shareY = F)
#   
#   # assign(x = paste0(graph, pos),value =  cdccovidplotting::plot_commlevels_lines(countydf = countydf, vareval = var, subtitle = sub, basechng = chng, countyfilt = countyfilt, .covidvar = .covidvar, .countycol = .countycol), envir = funenviron)
#   
# }
# 
# plot_commlevels_lines <- function(countydf, vareval, subtitle, basechng, countyfilt = NULL, .covidvar = covid_cases_per_100k, .countycol = county){
#   
#   # browser()
#   
#   if (!is.null(countyfilt)){
#     countydf <- countydf %>%
#       dplyr::filter(grepl(countyfilt, {{.countycol}}, ignore.case = T))
#   }
#   
#   if (nrow(countydf) == 0 | length(dplyr::pull(countydf, {{.countycol}}) %>% unique()) != 1){
#     stop("More or less than 1 county column present in countydf after countyfilt applied")
#   }
#   
#   cdccovidplotting::plot_commlevels(countydf = countydf, vareval = vareval, subtitle = subtitle, basechng = basechng) %>%
#     cdccovidplotting::lines_approp(varstring = gsub("~", "", dplyr::as_label(dplyr::quo(!!vareval))), countydf = countydf, .covidvar = {{.covidvar}}, .countycol = {{.countycol}})
#   
# }
# 
# plot_commlevels_lines(mc_data, ~ covid_cases_per_100k, subtitle = "Test", basechng = "chng_covid")


mc_data <- bind_all %>%
  filter(grepl("Maryland", state) & grepl("Montgomery", county))

color_bg <- function(plot, df, x_col, shapebot, shapetop, fillcol, opacity = 0.3, .linecol = NULL, .shapeadjust = 0.5){
  
  x_length <- length(df[[x_col]] %>% unique())
  plot %>%
    plotly::layout(
      shapes = list(
        list(type = "rect",
             x0 = 0 - .shapeadjust,
             x1 = x_length - .shapeadjust,
             xref = "x",
             y0 = shapebot,
             y1 = shapetop,
             yref = "y")
      )
    )
}

function (plot, df, x_col, line_val, gluetext, .yshift = 5, .xshift = 2, 
          .showarrow = T, .lineadjust = 0.5) 
{
  x_length <- length(df[[x_col]] %>% unique())
  plot %>% plotly::layout(shapes = list(list(type = "line", 
                                             y0 = line_val, y1 = line_val, x0 = 0 - .lineadjust, x1 = x_length - 
                                               .lineadjust, xref = "x", line = list(dash = "dash")))) %>% 
    plotly::add_annotations(text = glue::glue(gluetext), 
                            x = x_length - x_length/2, yshift = .yshift, xshift = .xshift, 
                            y = line_val, showarrow = .showarrow)
}


shapes_approp <- function(baseplot, varstring, countydf, .covidvar = "covid_cases_per_100k", .countycol = "county"){
  
  # pull most recent data/case level
  curr_level_cases <- countydf %>%
    dplyr::group_by(!!dplyr::sym(.countycol)) %>%
    dplyr::arrange(desc(date_updated)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(!!dplyr::sym(.covidvar))
  
  if (length(curr_level_cases) > 1){
    stop("Error; multiple counties present in dataframe")
  }
  
  if (varstring == "covid_cases_per_100k"){
    
    returnplot <- baseplot %>%
      plotlywrappers::dotted_line(df = countydf, x_col = "date_updated", line_val = 200, gluetext = "", .showarrow = F)
    
  }
  
  else{
    if (curr_level_cases < 200){
      
      var_med <- dplyr::case_when(varstring == "covid_hospital_admissions_per_100k" ~ 10,
                                  varstring == "covid_inpatient_bed_utilization" ~ 10)
      
      var_high <- dplyr::case_when(varstring == "covid_hospital_admissions_per_100k" ~ 20,
                                   varstring == "covid_inpatient_bed_utilization" ~ 15)
      
      returnplot <- baseplot %>%
        plotlywrappers::multi_line(df = countydf, x_col = "date_updated", yvec = c(var_med, var_high), c("Medium", "High"))
      
    }
    
    if (curr_level_cases >= 200){
      var_high <- dplyr::case_when(varstring == "covid_hospital_admissions_per_100k" ~ 10,
                                   varstring == "covid_inpatient_bed_utilization" ~ 10)
      
      returnplot <- baseplot %>%
        plotlywrappers::dotted_line(df = countydf, x_col = "date_updated", line_val = var_high, gluetext = "High", .showarrow = T)
      
    }
    
  }
  
  return(returnplot)
  
}


# mc_data %>%
#   layout(shapes = list(
#     list(type = "rect",
#          fillcolor = "green",
#          line = list(color = "green"),
#          opacity = 0.3,
#          x0 = )
#   ))
