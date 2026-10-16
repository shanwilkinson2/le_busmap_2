# load packages

library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(leaflet)
library(RColorBrewer)
library(glue)
library(shinydashboard)
library(shiny)
library(shinyjs)
library(DT)

# datasets
  full_data <- readRDS("bus_le_hle.RDS")
  la_boundaries <- readRDS("la_boundaries.RDS")
  route_summaries <- readRDS("route_summaries.RDS")
  route_boroughs <- readRDS("route_boroughs.RDS")

# CSS for styling of download button
my_css <- "
  #bttn_data {
  background: grey;
  color: white;
  font-size: 16px;
  }
  "

ui <- dashboardPage(
  dashboardHeader(title = "Life expectancy along bus routes",
                  titleWidth = 330),
  dashboardSidebar(
    sidebarMenu(id="tabs",
                sidebarMenuOutput("menu")
    ),
    radioButtons("select_gender", 
                 "Select gender", 
                 c("Male", "Female"), 
                 selected = "Female"),
    radioButtons("select_metric", 
                 "Select measure", 
                 c("Life Expectancy",
                   "Healthy Life Expectancy",
                   "Years not in good health"), 
                 selected = "Life Expectancy"),
    # selectize = TRUE makes it so you can type in
    selectInput(inputId = "select_route_id", 
                label = "Select route:", 
                choices = sort(unique(full_data$route_shortname_description)),
                selectize = TRUE
    ),
    em("(delete selected route to type & search)")
  ),
  dashboardBody(
    tags$style(my_css),
    tabItems(
      # map tab
      tabItem(tabName = "map",
              # adjusts height of map to be full window minus some for header + box at bottom
              tags$style(type = "text/css", "#bus_map {height: calc(100vh - 250px) !important;}"),
              box(title = textOutput("selected_stat"), #"on this route",
                  status = "primary",
                  width = 12,
                  solidHeader = TRUE,
                  infoBoxOutput("max_le"),
                  infoBoxOutput("min_le"),
                  infoBoxOutput("diff_le")
              ),
              leafletOutput("bus_map")
      ),
      # tab with table of selected data
      tabItem(tabName = "data_selected",
              DT::DTOutput("table_selected")
      ),
      # tab with table of all routes summary data
      tabItem(tabName = "data_all",
              # tab with table of all data
              # selectize = TRUE makes it so you can type in
              selectizeInput(inputId = "select_borough", 
                          label = "Select borough:", 
                          choices = sort(unique(route_boroughs$la)),
                          multiple = TRUE,
                          options = list(plugins = list("remove_button"),
                                         placeholder = "All boroughs")
              ),
              DT::DTOutput("table_all_summary")
      ),
      # downloads tab
      tabItem(tabName = "downloads",
              h2("Download data"),
              downloadButton("dl_full_data", "Download stop & route data (csv)"),
              br(),
              downloadButton("dl_route_summaries", "Download summary route data (csv)"),
              br(),
              downloadButton("dl_route_boroughs", "Download routes per borough (csv)"),
              ),
      # details & sources tab
      tabItem(tabName = "details",
              h2("Further information"),
              br(),
              h3("What does this app show?"),
              p("This app is intended to visualise the differences in life expectancy and healthy life expectancy that can be seen over relatively short distances."),
              p("Life expectancy is an important indicator of overall population health, and inequalities in health."),
              p("Life expectancy at birth is the average number of years a baby born would be expected to live for if they experienced today’s age-specific death rates for the rest of their life. However it is not a prediction as these factors would be unlikely to remain constant."),
              p("We can't live for ever, but is it fair that people from some areas can expect so many years more life than people from other areas? We would want to narrow the gap from what it is today."),
              p("Healthy life expectancy is the number of years a baby born today in a particular area can expect to live in good health, again if today's situation remained the same for the rest of their life. This refers to self rated health, but self rated health is a good predictor of service usage."),
              p("Years not in good health is the difference between the two."),
              p("We would want to increase the proportion of people's lives that is spent in good health so they have the health to do what they want to do as long as possible, and also to reduce the cost of intensive health and social care that is often necessary where people experience a prolonged period of very poor health right at the end of their lives."),
              p("To find out more about the reasons behind the inequalities in life expectancy, take a look at the Office for Health Improvement & Disparities's Segment Tool."),
              a("OHID Segment Tool", href = "https://analytics.phe.gov.uk/apps/segment-tool/", 
                target = "_blank"),
              br(),
              br(),
              h2("Data sources"),
              p("Bus data from Department for Transport Bus open data service, last downloaded October 2026"),
              p("Routes with at least one stop within Greater Manchester are included here."),
              a("Find bus open data", href = "https://data.bus-data.dft.gov.uk/timetable/download/",
                 target = "_blank"),
              br(),
              p("Life expectancy and Healthy life expectancy data are from the Office for Health Improvement & Disparities' Fingertips local health profile. This gives Middle Super Output Area (MSOA - a medium sized administrative area) data on a range of indicators."),
              p("Life expectancy data is the 2025 issue which is based on data relating to the period 2019-2023."),
              p("Healthy Life expectancy data is the 2026 issue which is based on deaths relating to the period 2013-2017, and self-reported health status from Census 2021."),
              p("Each bus stop is given the life expectancy at birth & healthy life expectancy of the MSOA in which they are located. Values are not available for some MSOAs."),
              p("Years not in good health is calculated as the difference between the two."),
              a("OHID Fingertips, local health profile", href = "https://fingertips.phe.org.uk/profile/local-health/data#page/0/gid/1938133185/pat/402/par/E08000001/ati/3/are/E02000988/yrr/1/cid/4/tbm/1", 
                target = "_blank"),
              br(),
              br(),
              p("Code for this app is on my github"),
              a("Github", href="https://github.com/shanwilkinson2/le_busmap_2", target = "_blank")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # render sidebar menu - needed if reactive stuff on page
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Map selected route", tabName = "map", icon = icon("map-location-dot")),
      menuItem("Explore selected route", tabName = "data_selected", icon = icon("magnifying-glass-location")),
      menuItem("Explore all routes", tabName = "data_all", icon = icon("magnifying-glass")),
      menuItem("Download all data", tabName = "downloads", icon = icon("download")),
      menuItem("Details", tabName = "details", icon = icon("circle-info"))
    )
  })
  # makes the menu not update itself when other reactive stuff changes
  isolate({updateTabItems(session, "menu", "map")})
  
  # reactive dataset for map  
  selected_data <- reactive({
    full_data %>%
      filter(route_shortname_description == input$select_route_id & 
               sex == input$select_gender &
               metric == input$select_metric) %>%
      select(route_short_name, agency_name, route_description, route_shortname_description, stop_name, 
             stop_sequence, sex, metric, value)
  })
  
  # reactive dataset for selected data table
  selected_route_data <- reactive({
    full_data %>%
      filter(route_shortname_description == input$select_route_id) %>%
      st_drop_geometry() %>%
      select("Route" = route_shortname_description, 
             "Stop order" = stop_sequence, "Stop name" = stop_name, "Metric" = metric,
             "Gender" = sex, "Area name" = MSOA21NM, "Value" = value)
  })
  
  # reactive dataset for all routes (to filter for just certain borough(s))
  summary_data <- reactive ({
    if(length(input$select_borough) == 0) return(route_summaries)
    
    routes_in <- route_boroughs %>%
      filter(la %in% input$select_borough) %>%
      pull(route_shortname_description) %>%
      unique() 
    
    route_summaries %>%
      filter(route_shortname_description %in% routes_in)
    
  })
  
  # name of selected stat
  output$selected_stat <- renderText({input$select_metric})
  
  # interactive map ###########################################################
  
  # domain = range of values  
  life_exp_pal <- reactive({
    colorNumeric(palette = "BuPu", 
                 domain = c(min(selected_data()$value, na.rm = TRUE),
                            max(selected_data()$value, na.rm = TRUE)), 
                 reverse = ifelse(input$select_metric == "Years not in good health", FALSE, TRUE)
    )
  })
  
  padded_bounds <- function(d) { 
    bb <- st_bbox(d)
    dx <- max((bb[["xmax"]] - bb[["xmin"]]) * 0.10, 0.005)
    dy <- max((bb[["ymax"]] - bb[["ymin"]]) * 0.10, 0.005)
    c(lng1 = bb[["xmin"]] - dx, lat1 = bb[["ymin"]] - dy,
      lng2 = bb[["xmax"]] + dx, lat2 = bb[["ymax"]] + dy)
  }
  
  
  # ---- helper: draws stops, legend and zoom onto any leaflet map ----
  add_stops <- function(map, d) {
    
    labels <- glue(
      "<b>Route:</b> {d$route_short_name} {d$route_description}<br>
       <b>Stop:</b> {d$stop_name}<br>
       <b>{str_to_sentence(d$sex)} {input$select_metric}:</b> {round(d$value, 1)}"
    ) %>% as.list() %>% lapply(HTML)
    
    bb <- st_bbox(d)
    dx <- max((bb[["xmax"]] - bb[["xmin"]]) * 0.10, 0.005)
    dy <- max((bb[["ymax"]] - bb[["ymin"]]) * 0.10, 0.005)
    
    map %>%
      clearGroup("stops") %>%
      removeControl("legend") %>%
      addCircleMarkers(data = d, group = "stops", radius = 8,
                       fillColor = ~life_exp_pal()(value),
                       label = labels,
                       weight = 2, fillOpacity = 0.8, color = "black") %>%
      addLegend("bottomleft", layerId = "legend", data = d,
                pal = life_exp_pal(), values = ~value,
                labFormat = labelFormat(digits = 0),
                title = input$select_metric, opacity = 1) %>%
      fitBounds(bb[["xmin"]] - dx, bb[["ymin"]] - dy,
                bb[["xmax"]] + dx, bb[["ymax"]] + dy)
  }
  
  # ---- the map: first draw, includes boundaries and initial route ----
  output$bus_map <- renderLeaflet({
    
    base <- leaflet() %>%
      addProviderTiles("Stadia.AlidadeSmooth") %>%
      addPolylines(data = la_boundaries, fill = TRUE, fillOpacity = 0, color = "#444444",
                  weight = 1.5, popup = ~la) %>%
      addControl("Hover over a bus stop to find out more",
                 position = "topright")
    
    d <- isolate(selected_data())
    b <- padded_bounds(d)
    
    isolate(add_stops(base, d)) %>%
      htmlwidgets::onRender(sprintf(
        "function(el, x) {
         var m = this;
         setTimeout(function() {
           m.invalidateSize();
           m.fitBounds([[%f, %f], [%f, %f]]);
         }, 300);
       }", b[["lat1"]], b[["lng1"]], b[["lat2"]], b[["lng2"]]))
  })
  
  
  # ---- updates after the first draw (dropdowns / radio buttons) ----
  observeEvent(
    list(selected_data(), life_exp_pal(), input$select_metric),
    {
      req(nrow(selected_data()) > 0)
      add_stops(leafletProxy("bus_map"), selected_data())
    },
    ignoreInit = TRUE
  )
  
  #############################################################################

  # info box caption 
  output$infobox_caption <- reactive({
    paste(input$select_metric, "on this route")
  })
  
  # max life expectancy info box
  output$max_le <- renderInfoBox({
    infoBox(
      "HIGHEST", 
      paste(round(
        max(selected_data()$value, na.rm = TRUE)), 
        "years"), 
      icon = icon("arrow-circle-up"),
      color = "light-blue"
    )
  })
  
  # min life expectancy info box
  output$min_le <- renderInfoBox({
    infoBox(
      "LOWEST", 
      paste(round(
        min(selected_data()$value, na.rm = TRUE)),
        "years"), 
      icon = icon("arrow-circle-down"),
      color = "light-blue"
    )
  })
  
  # difference in life expectancy info box
  output$diff_le <- renderInfoBox({
    infoBox(
      "DIFFERENCE", 
      paste(round(
        max(selected_data()$value, na.rm = TRUE) - min(selected_data()$value, na.rm = TRUE) 
      ), 
      "years"), 
      icon = icon("exchange-alt"),
      color = "yellow"
    )
  })
  
  # generate data for download buttons
  output$dl_full_data <- downloadHandler(
    filename = "bus_le_hle_full_route_data.csv",
    # create file for downloading
    content = function(file){
      out <- full_data %>%
        mutate(longitude = st_coordinates(.)[,1],
               latitude = st_coordinates(.)[,2]) %>%
        st_drop_geometry()
      write.csv(out, file, row.names = FALSE)
      })
  
  output$dl_route_summaries <- downloadHandler(
    filename = "bus_le_hle_route_summaries.csv",
    # create file for downloading
    content = function(file) write.csv(route_summaries, file, row.names = FALSE))
  
  output$dl_route_boroughs <- downloadHandler(
    filename = "bus_le_hle_route_boroughs.csv",
    # create file for downloading
    content = function(file) write.csv(route_boroughs, file, row.names = FALSE))
  
  # selected route table
  output$table_selected <- DT::renderDT({
    datatable(selected_route_data(), 
  filter = "top",
  rownames = FALSE,
  extensions = "Buttons", 
  options = list(
    dom = "lBrtip",
    pageLength = 25,
    lengthMenu = c(10, 25, 50, 100),
    buttons = c("copy", "csv", "excel")
  )) %>%
  formatRound("Value", digits = 1)
  })


# all route summary table
  output$table_all_summary <- DT::renderDT({
    dat <- summary_data() %>%
      select("Route number" = route_short_name, "Agency name" = agency_name, "Route description" = route_description,
             "Metric" = metric, "Gender" = sex, "Number of stops" = num_stops,
             Min = min, Max = max, "Average (median)" = median, Range = range)

    datatable(
      dat, 
      filter = "top",
      rownames = FALSE,
      extensions = "Buttons", 
      options = list(
        dom = "lBrtip",
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        buttons = c("copy", "csv", "excel")
        )
      ) %>%
      formatRound(c("Min", "Average (median)", "Max", "Range"), digits = 1)
  })
  
# end bracket VV
}

shinyApp(ui, server)
