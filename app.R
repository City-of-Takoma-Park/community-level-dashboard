#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotlywrappers)
library(plotly)
library(cdccovidplotting)
library(bslib)

# rsconnect::deployApp(appName = "cdccommlevel", appFiles = c("data", "app.R", "update_data.R"), account = "takomapark")

comm_level_all <- readRDS("./data/all_community_level.rds")
state_list <- comm_level_all$state %>% unique()

state_process <- function(statedf){
  colors <- data.frame(covid_19_community_level = c("Low", "Medium", "High"), colorcomm = c("#72DE8F", "#ffcc00", "#fa8484"))
  
  statedf %>%
    dplyr::mutate(countyleg = gsub(" County", "", county)) %>%
    dplyr::group_by(county) %>%
    dplyr::arrange(desc(date_format)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::left_join(colors) %>%
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
                  name = ~ covid_19_community_level,
                  type = "bar",
                  text = ~ glue::glue("Current community level: {covid_19_community_level}
                  Percentile-ranking: {round(df[[varpct]]*100, 1)}
                  Ranking among counties: {df[[varrank]]}/{maxrank}"),
                  legendgroup = ~ covid_19_community_level,
                  showlegend = showleg,
                  color = ~ I(colorcomm)) %>%
    plotly::layout(xaxis = list(title = "County"),
                   yaxis = list(title = ""),
                   title = "") %>%
    plotlywrappers::subplot_title(subtitle)
  
}

subplots_state <- function(df, statename, dateformat){
  
  # print(glue::glue("{statename} counties by community-level components as of {dateformat}"))
  plotly::subplot(purrr::map2(rlang::list2(~ covid_cases_per_100k,
                                           ~ covid_inpatient_bed_utilization,
                                           ~ covid_hospital_admissions_per_100k), 
                              c("Cases per 100k",
                                "Percent hospital\nbeds due to\nCOVID",
                                "COVID hospitalizations\nper 100k"),
                              ~ {
                                
                                showleg <- dplyr::if_else(grepl("Cases", .y), T, F)
                                
                                plot_state(df, .x, .y, showleg = showleg)
                              }), 
                  nrows = 3, shareX = T) %>%
    plotly::layout(title = glue::glue("{statename} counties by community-level components as of {dateformat}"),
                   legend = list(title = list(text = "<b>Community level</b>"))) 
  
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("County Community-level Explorer"),
  
  p(strong('Background:'), 'this application allows you to generate graphs of CDC Community Level data for counties in your state. The ', tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/your-health/covid-by-county.html", "CDC's Community Level"), " is a measure of COVID-19's current impact on communities based on 1) case-levels (normalized to 100,000 residents), 2) the percent of hospital beds in the area filled by COVID-19 patients, and 3) hospital admissions due to COVID-19 in the area (normalized to 100,000 residents). The CDC's guidance for how individuals and communities should respond to COVID-19 is based off the area's Community Level. To generate graphs of one county's community level and comparing all counties in the state, select or search for a state using the state select box and hit the 'View Counties in state' button; and then select a county in the state and click the 'View county's Community Level button.' This app was created by staff at the City of Takoma Park, MD; for any questions or concerns, email", a(href = 'mailto:danielp@takomaparkmd.gov', "danielp@takomaparkmd.gov.")),
  
  theme = bslib::bs_theme(bootswatch = "spacelab"),
  
  # bslib::bs_theme_preview(),
  
  fluidRow(
    column(width = 5,
           offset = 0.5, 
           shiny::selectInput("statepick", "Select which state you want community level information on", choices = state_list),
           shiny::actionButton(inputId = "submitstate", label = "View counties in state")),
    column(width = 5,
           offset = 0.5,
           
           shiny::uiOutput(outputId = "county_info"),
           
           shiny::uiOutput(outputId = "countybutton")
    )
  ),
  
  br(),
  
  p(shiny::textOutput("countydesc")),
  br(),
  fluidRow(
    plotly::plotlyOutput("county_comlevel")
  ),
  
  br(),
  
  p(textOutput("statedesc")),
  
  br(),
  
  fluidRow(
    plotly::plotlyOutput("statecomp")
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  state_commlevel <- reactive({
    req(input$submitstate)
    
    comm_level_state <- comm_level_all %>%
      filter(state == input$statepick)
  })
  
  state_process_df <- reactive({
    state_process(state_commlevel())
  })
  
  select_county_list <- reactive({
    state_commlevel() %>%
      pull(county) %>%
      unique()
  })
  
  output$county_info <- shiny::renderUI({
    shiny::selectInput(input = "countyselect", label = "Select a County to see its community level and the measures that make up the community level",  choices = select_county_list() %>% sort())
    
  })
  
  output$countybutton <- shiny::renderUI({
    req(input$submitstate)
    
    shiny::actionButton(inputId = "submitcounty", label = "View county's Community Level")
  })
  
  selected_county <- reactive({
    req(input$submitcounty)
    
    state_commlevel() %>%
      dplyr::filter(county == input$countyselect)
  })
  
  output$countydesc <- shiny::renderText({
    req(input$submitcounty)
    
    "The graph below shows the components that make up the selected County's community level"
  })
  
  output$statedesc <- shiny::renderText({
    req(input$submitcounty)
    
    "The graph below compares counties in the state by community-levels. Move your mouse over a bar to see the percentile and raw-ranking of the County on each metric compared to other counties."
  })
  
  current_comm_level <- reactive({
    
    selected_county() %>%
      arrange(desc(date_updated)) %>%
      slice_head(n = 1) %>%
      pull(covid_19_community_level)
    
  })
  
  output$county_comlevel <- plotly::renderPlotly({
    
    title <- glue::glue("{input$countyselect} community level: {current_comm_level()}")
    # browser()
    
    cdccovidplotting::subplots_keyvars(selected_county(), subrows = 1) %>%
      plotly::layout(title = title, xaxis = list(title = "Date", yaxis = list(title = "")))
  })
  
  output$statecomp <- plotly::renderPlotly({
    
    req(input$submitcounty)
    
    date <- state_process_df()[["date_format"]][1]
    
    state <- state_process_df()[["state"]][[1]]
    
    subplots_state(df = state_process_df(), statename = state, dateformat = date)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
