
library(shiny)
library(tidyverse)
library(plotlywrappers)
library(plotly)
library(cdccovidplotting)
library(bslib)

# rsconnect::deployApp(appName = "cdccommlevel", appFiles = c("data", "app.R", "update_data.R"), account = "takomapark")

comm_level_all <- readRDS("./data/all_community_level.rds")
state_list <- comm_level_all$state %>% unique()
county_all <- comm_level_all$statecounty %>% unique()

state_process <- function(statedf, countycol = county){
  colors <- data.frame(covid_19_community_level = c("Low", "Medium", "High"), colorcomm = c("#72DE8F", "#ffcc00", "#fa8484"))
  
  statedf %>%
    dplyr::mutate(countyleg = gsub(" County", "", {{countycol}})) %>%
    dplyr::group_by({{countycol}}) %>%
    dplyr::arrange(desc(date_updated)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::left_join(colors) %>%
    dplyr::ungroup()
}

calc_all_pctile <- function(df, suffix){
  df %>%
    pctile_rank("covid_hospital_admissions_per_100k", suffix = suffix) %>%
    pctile_rank("covid_inpatient_bed_utilization", suffix = suffix) %>%
    pctile_rank("covid_cases_per_100k", suffix = suffix)
}


pctile_rank <- function(df, pctilevar, suffix = ""){
  
  newvar <- paste0(pctilevar, "_pctile", suffix = suffix)
  rank <- paste0(pctilevar, "_rank", suffix = suffix)
  
  df %>%
    dplyr::mutate(!!dplyr::sym(newvar) := dplyr::percent_rank(!!dplyr::sym(pctilevar)),
                  !!dplyr::sym(rank) := dplyr::dense_rank(!!dplyr::sym(pctilevar)))
}

plot_state <- function(df, varplot, subtitle, multistate = F, showleg = F){
  
  # browser()
  
  # browser()
  var <- as.character(expr(!!varplot))[2]
  
  varpct_state <- paste0(var, "_pctile", "_state")
  varrank_state <- paste0(var, "_rank", "_state")
  maxrank_state <- max(df[[varrank_state]])
  
  varpct_nation <- paste0(var, "_pctile", "_nation")
  varrank_nation <- paste0(var, "_rank", "_nation")
  maxrank_nation <- max(df[[varrank_nation]])
  
  if (multistate){
    varpct_comp <- paste0(var, "_pctile", "_comp")
    varrank_comp <- paste0(var, "_rank", "_comp")
    maxrank_comp <- max(df[[varrank_comp]])
  }
  
  gluetext <- dplyr::case_when(
    multistate ~ "Current community level: {covid_19_community_level}
    Last date updated: {date_format}
    Ranking among selected counties: {df[[varrank_comp]]}/{maxrank_comp}
    State percentile-ranking: {round(df[[varpct_state]]*100, 1)}
    Nationwide percentile-ranking: {round(df[[varpct_nation]]*100, 1)}",
    T ~ "Current community level: {covid_19_community_level}
    Last date updated: {date_format}
    Percentile-ranking among state counties: {round(df[[varpct_state]]*100, 1)}
    Ranking among state counties: {df[[varrank_state]]}/{maxrank_state}
    Nationwide percentile-ranking: {round(df[[varpct_nation]]*100, 1)}"
  )
  
  
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
                  hovertext = ~ glue::glue(gluetext),
                  # texposition = "inside",
                  # textfont = list(color = "black"),
                  legendgroup = ~ covid_19_community_level,
                  showlegend = showleg,
                  color = ~ I(colorcomm)) %>%
    plotly::layout(xaxis = list(title = "County"),
                   yaxis = list(title = ""),
                   title = "") %>%
    plotlywrappers::subplot_title(subtitle)
  
}

subplots_state <- function(df, statename, dateformat, multistate = F){
  # browser()
  
  # print(glue::glue("{statename} counties by community-level components as of {dateformat}"))
  plotly::subplot(purrr::map2(rlang::list2(~ covid_cases_per_100k,
                                           ~ covid_inpatient_bed_utilization,
                                           ~ covid_hospital_admissions_per_100k), 
                              c("Cases per 100k",
                                "Percent hospital\nbeds due to\nCOVID",
                                "COVID hospitalizations\nper 100k"),
                              ~ {
                                
                                showleg <- dplyr::if_else(grepl("Cases", .y), T, F)
                                
                                plot_state(df, .x, .y, showleg = showleg, multistate = multistate)
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
  
  shiny::tabsetPanel(
    # type = "tabs",
    selected = "statecomp",
    shiny::tabPanel(
      title = "Compare state counties",
      value = "statecomp",
      br(),
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
      # fluidRow(
        plotly::plotlyOutput("county_comlevel"),
      # ),
      
      br(),
      
      p(textOutput("statedesc")),
      
      br(),
      
      # fluidRow(
      
      plotly::plotlyOutput("statecomp")
      # ),
    ),
    shiny::tabPanel(
      title = "Compare all counties",
      value = "countycomp",
      
      br(),
      # fluidRow(
              shiny::selectizeInput("countypick", 
                            choices = NULL,
                            label = "Select which counties you want to compare",
                            multiple = T),
      
      shiny::actionButton(inputId =  "submitcountylist", label = "Create graph comparing counties"),
      
      br(),
      
      br(),
      
      plotly::plotlyOutput("countycomp")
      # )
    )
    
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  updateSelectizeInput(session, inputId = "countypick", choices = county_all %>% sort(), server = T, label = "Select which counties you want to compare")
  
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
      ungroup %>%
      arrange(desc(date_updated)) %>%
      slice_head(n = 1) %>%
      pull(covid_19_community_level)
    
  })
  
  
  # https://stackoverflow.com/questions/57242792/update-plot-output-on-actionbutton-click-event-in-r-shiny
  
  reactiveplots <- reactiveValues(
    countyind = NULL,
    stateplot = NULL,
    countyplot = NULL
  )
  
  observeEvent(input$submitcounty, {
    
    title <- glue::glue("{input$countyselect} community level: {current_comm_level()}")
    # browser()
    
    reactiveplots$countyind <- cdccovidplotting::subplots_keyvars(selected_county(), subrows = 1) %>%
      plotly::layout(title = title, xaxis = list(title = "Date", yaxis = list(title = "")))
    
    
    date <- state_process_df()[["date_format"]][1]
    
    state <- state_process_df()[["state"]][[1]]
    
    reactiveplots$stateplot <- subplots_state(df = state_process_df(), statename = state, dateformat = date)
  })
  
  output$county_comlevel <- plotly::renderPlotly({
    if (is.null(reactiveplots$stateplot)) return()
    reactiveplots$countyind
    
  })
  
  output$statecomp <- plotly::renderPlotly({
    
    if (is.null(reactiveplots$stateplot)) return()
    reactiveplots$stateplot
    
  })
  
  observeEvent(input$submitcountylist, {
    outdf <- comm_level_all %>%
      dplyr::filter(statecounty %in% input$countypick) %>%
      state_process(countycol = statecounty) %>%
      dplyr::ungroup() %>%
      calc_all_pctile("_comp")
    
    reactiveplots$countyplot <- subplots_state(df = outdf, "Comparison-group", dateformat = " most recent updates", multistate = T)
    
  })
  
  output$countycomp <- plotly::renderPlotly({
    
    # browser()
    
    if (is.null(reactiveplots$countyplot)) return()
    
    reactiveplots$countyplot

    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
