## load packages
library(shiny);library(shinycssloaders);library(shinydashboard);library(shinyWidgets)
library(plotly);library(DT)
library(leaflet);library(leaflet.extras)
library(dplyr); library(tidyr); library(tibble)
library(sp);library(rgdal)
library(maps)
library(raster)
# library(viscover)
require(sf)
## CDL legend URL
legenduri <- "https://nassgeodata.gmu.edu/CropScapeService/wms_cdlall.cgi?version=1.1.1&service=wms&request=getlegendgraphic&layer=cdl_2009&format=image/png"
## the most recent available CDL year
lastyr <- as.numeric(format(Sys.Date(), "%Y")) - 1
setwd("~/GitHub/VISCOVER_V2")
source("VISCOVER_basefun.R")

county_fips <- readRDS("county_fips.RDS")  # the first 49 denote the whole state

CDL_NRI_crosswalk <- readxl::read_xlsx("NRI_CDL_Codes.xlsx", col_names =T, sheet = 3) %>% 
  "["( ,c(1,2,4)) %>% 
  "names<-"(c("code", "CDL_categ", "NRI_categ")) %>% dplyr::arrange(., code)

counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE)) %>% 
  separate(., ID, into = c('state', 'county'), sep =',')
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>% 
  "names<-"(c("state", "geom"))




################################################################################
# Ui 
################################################################################
ui <- dashboardPage(
  dashboardHeader(title = "viscover: VIsualize Soil and Crop data and their OVERlay", titleWidth = 550, disable = FALSE),
  dashboardSidebar(
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem(text = "VISCOVER", tabName = "main", icon = icon("list-alt"),
               badgeColor = "yellow"),
      menuItem(text = "Extension Functions", icon = icon("th"), tabName = "extension", # badgeLabel = "new",
               badgeColor = "green"),
        menuSubItem("Point Search", tabName = "ext_sub1"),
        menuSubItem("County/State Search", tabName = "ext_sub2")

    )
  ),

########################################
  dashboardBody(
    tabItems(
      tabItem(tabName = "main",
              fluidRow(
                box(title = "Info", status = "success", solidHeader = TRUE, width = 12,
                    collapsible = TRUE, collapsed = TRUE,
                    tabBox(
                      width = 12,
                      tabPanel(
                        "Introduction",
                        p("This is a shiny app tool designed for interacting with",
                          tags$a(href = "https://nassgeodata.gmu.edu/CropScape/", "Cropland Data Layer (CDL)"),
                          "and",
                          tags$a(href = "https://sdmdataaccess.sc.egov.usda.gov", "Soil Data Layer (SDL).")),
                        tags$ul(
                          tags$li(
                            "The layer control at the bottom left corner allows you to preview CDL data across different years.
              Press the top-left calendar button to confirm your choice of CDL year."),
                          tags$li(
                            "By selecting two tiles (Map unit polygon & Survey area polygon), you will
               see the soil mapunits polygons within the current extent of the map."),
                          tags$li(
                            "Click the toggle switches at the top left corner to show information about CDL ('C') or SDL ('S').",
                            tags$ul(
                              tags$li("Clicking the map would prompt to draw a marker (labelled as a yelow 'C' or a blue 'S') on that location.
                        To see the CDL/SDL info for that location, hover over the marker.
                        Click the marker again to remove it."),
                              tags$li("The top-left download button would let you download a .csv file containing
                        the CDL/SDL features at the existing markers on the map."))
                          )
                        ),
                        p("This tool can be used to obtain an intersection of CDL and SDL for any small area in the contiguous United States." ),
                        tags$ul(
                          tags$li("Firstly, use the searchOSM or locateMe widget to zoom in to the area you're interested in."),
                          tags$li("Secondly, use the draw tools on the left to draw a polygon/rectangle/circle on the map."),
                          tags$li("Thirdly, unfold the 'Soil Data Layer' box under the map panel and click the 'update' button, then you'll be able to see the SDL information."),
                          tags$li("Lastly, unfold the 'Cropland Data Layer' box and then you'll be able to see the tabulated CDL pixels which fall within the red bounding box or the selected map units.")
                        ),
                        p("About the author:", tags$a(href = "http://annielyu.com", "Xiaodan Lyu")),
                        p("Acknowledgement: special thanks to", tags$a(href = "http://hofmann.public.iastate.edu/", "Dr. Heike Hofmann"), 
                          "and Dr. Emily Berg for their valuable advice.")
                      ),
                      tabPanel(
                        "Demo",
                        tags$div(
                          HTML('<iframe src="https://player.vimeo.com/video/321794430" width="640" height="348" frameborder="0" allow="autoplay; fullscreen" allowfullscreen></iframe>')
                        ))
                    )
                ),
                hr(),
                box(width = 12,
                    leafletOutput("map", width = "100%", height = 750),
                    absolutePanel(
                      top = 20, left = 50,
                      # column(width = 2, actionBttn("go_cdl", "", icon = icon("calendar"), style = "bordered", size = "sm")),
                      column(width = 2, actionButton("go_cdl", "", icon = icon("calendar"))),
                      column(offset = 1, width = 2, downloadButton("download_cdl_markers", "")),
                      column(offset = 1, width = 2, switchInput("is_mu", size = "mini", value = TRUE, onLabel = "S", offLabel = "C",
                                                                onStatus = "primary", offStatus = "warning")),
                      style = "opacity: 1; z-index: 1000;"
                    )
                ),
                hr(),
                box(title = "Soil Data Layer", status = "primary", solidHeader = TRUE,
                    width = 12, collapsible = TRUE, collapsed = TRUE,
                    p("Please define your AOI (area of interest) using the drawing tools."),
                    # radioButtons("mu_type", "Select Scope of the Mapunits:",
                    #              choices = c("Pieces" = 1, "Whole" = 2),
                    #              selected = 1, inline = TRUE),
                    DT::dataTableOutput("tbl_mu") %>% withSpinner(),
                    br(),
                    fluidRow(width = 12,
                             # column(width = 1, actionButton("go_mu", HTML("<b>Update</b>"), class = "btn btn-primary", icon = icon("refresh"))),
                             column(width = 1, actionBttn("go_mu", "Update", style = "jelly", color = "primary", size = "sm", icon = icon("refresh"))),
                             column(width = 1, downloadBttn("download_mu", HTML("<font color='#ffffff'>Download</font>"), color = "primary", size = "sm", style = "jelly")))
                    # column(width = 1, downloadButton("download_mu", HTML("<b>Download</b>"), class = "btn btn-primary")))
                ),
                hr(),
                box(title = "Cropland Data Layer", status = "warning", solidHeader = TRUE,
                    width = 12, collapsible = TRUE, collapsed = TRUE,
                    radioButtons("cdl_type", "Select Cropping Area:",
                                 choices = c("Within Red Bounding Box" = 1, "Within Selected Mapunit Polygons" = 2),
                                 selected = 1, inline = TRUE),
                    fluidRow(width = 12,
                             column(width = 6, DT::dataTableOutput("tbl_cdl") %>% withSpinner()),
                             column(width = 6, br(), br(), br(),
                                    plotlyOutput("plot_cdl", height = "100%") %>% withSpinner())
                    ),
                    br(),
                    fluidRow(width = 12,
                             # column(width = 1, actionButton("go_cdl", HTML("<b>Update</b>"), class = "btn btn-warning", icon = icon("refresh"))),
                             column(width = 1, downloadBttn("download_cdl", HTML("<font color='#ffffff'>Download</font>"), color = "warning", size = "sm", style = "jelly"))
                    )),
                hr()
              )

      ),

##################################
      # NRI extensions 
      tabItem(tabName = "extension", 
              fluidRow(
                box(title = "Extension Functions Info", status = "success", solidHeader = TRUE, width = 12,
                   # collapsible = TRUE, collapsed = TRUE,
                    tabBox(
                      width = 12,
                      tabPanel(
                        "Point Search",
                        p("This is a shiny app tool designed for interacting with"),
                        p("This tool can be used to obtain an intersection of CDL and SDL for any small area in the contiguous United States." ),
                        tags$ul(
                          tags$li("Firstly, use the searchOSM or locateMe widget to zoom in to the area you're interested in."),
                          tags$li("Secondly, use the draw tools on the left to draw a polygon/rectangle/circle on the map."),
                          tags$li("Thirdly, unfold the 'Soil Data Layer' box under the map panel and click the 'update' button, then you'll be able to see the SDL information."),
                          tags$li("Lastly, unfold the 'Cropland Data Layer' box and then you'll be able to see the tabulated CDL pixels which fall within the red bounding box or the selected map units.")
                        ),
                        p("About the author:", tags$a(href = "http://annielyu.com", "Xiaodan Lyu")),
                        p("Acknowledgement: special thanks to", tags$a(href = "http://hofmann.public.iastate.edu/", "Dr. Heike Hofmann"), 
                          "and Dr. Emily Berg for their valuable advice.")
                      ),
                      tabPanel(
                        "County/State Search",
                        )
                    )
                )
                )
      ),

      # Point level search 
      tabItem(tabName = "ext_sub1",
              fluidRow(
                box(title = "Search A List of NRI Points", status = "warning", solidHeader = TRUE, width = 12,
                    fileInput("file1", "Choose xlsx File",
                              multiple = TRUE,
                              accept = c("text/xlsx",
                                         "text/comma-separated-values,text/plain",
                                         ".xlsx")),
                    column(width = 5,
                           selectInput("choose_year", "Choose the years:",
                                       choices = lastyr:1997, multiple = TRUE)),
                    fluidRow(width = 12, 
                             DT::dataTableOutput("tbl_NRI_listp") %>% withSpinner()),
                    column(width = 1, actionBttn("getNRIp", "Update", style = "jelly", 
                                                 color = "primary", size = "sm", icon = icon("refresh")))
                )
                )
              ),

      tabItem(tabName = "ext_sub2",
        fluidRow(
                box(title = "Check Existence of Cropland Type for Given County/State", status = "danger", 
                    solidHeader = TRUE, width = 12,
                    fluidRow(width = 10,
                             column(width = 5,
                                    selectInput("NRI_state_search", "Choose one state:",
                                                choices = states$state)),
                             column(width = 5, 
                                    uiOutput("secondsearch"))
                             # column(width = 5,
                             #        selectInput("NRI_county_search", "Choose County:",
                             #                    choices = NULL))
                    ),
                    fluidRow(width = 10,
                             column(width = 5,
                                    selectInput("NRI_year_search", "Choose the years:",
                                                choices = NULL, multiple = TRUE)),
                             column(width = 5, 
                                    selectInput("NRI_category_search", "Choose cropland types: ", 
                                                choices = NULL, multiple = TRUE)),

                             # column(width = 4,
                             #        selectInput("CDL_category_search", "Choose the CDL cateogories you want to check: ",
                             #                    choices = NULL,
                             #                    multiple = TRUE))),
                    ),
                    fluidRow(width = 12, 
                             DT::dataTableOutput("tbl_NRI_categ_county") %>% withSpinner()),
                    column(width = 1, actionBttn("getcateg_county", "Update", style = "jelly", 
                                                 color = "primary", size = "sm", icon = icon("refresh"))))
              )
      )
  )
)
)

###########################################################################################
###########################################################################################
# Server 
###########################################################################################
###########################################################################################



server <- function(input, output, session) {
  
  #req(input$file1)
  
###############################################################################################
  # show the CDL categories of a list of NRI points
  NRIlistp <- eventReactive(input$getNRIp, {
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    
    NRIlistp <- readxl::read_excel(input$file1$datapath)
    df_var <- names(NRIlistp)
    year <- gsub("[^0-9.-]", " ", input$choose_year) %>% strsplit(., " ") %>% unlist %>% as.numeric()
    year <- year[is.na(year)== FALSE & year >=1997 & year <=lastyr] %>% sort(., decreasing = TRUE)
    lat <- NRIlistp$lat
    lon <- NRIlistp$lon
    
    for(j in year)
    {
      categ <- NULL
      for(i in 1:dim(NRIlistp)[1])
        categ <- c(categ, GetCDLValue(year = j, lon = lon[i], lat = lat[i])$category)
      NRIlistp <- cbind(NRIlistp, categ)
    }
    NRIlistp %>% 
      "names<-"(c(df_var, year)) -> NRIlistp
    NRIlistp
  })
  
  output$tbl_NRI_listp <- DT::renderDataTable({
    if(input$getNRIp)
    {
      NRIlistp <- NRIlistp()
      org_var = dim(NRIlistp)[2]
      if(dim(NRIlistp)[2] > 5)
        org_var <- names(NRIlistp) %>% gsub("[^0-9.-]", " ", .) %>% 
          as.numeric() %>% is.na() %>% sum()
      NRIlistp %>%
        DT::datatable(., caption = "CDL categories of NRI points",
                      extensions = c('Buttons',
                                     'FixedColumns'),
                      options = list(
                        dom = 'Bfrtip',    
                        buttons = c('csv', 'excel', 'pdf'),
                        scrollX = TRUE,    fixedColumns = list(leftColumns = org_var)
                      ))
        
    }

  })
  
 ############################################################################################## 
  # search specific crop categories for counties in some years
  categ_county_list <- eventReactive(input$getcateg_county, {
    state <- input$NRI_state_search
    county <- input$NRI_county_search
    year <- input$NRI_year_search 

    categ_summary <- GetCDLcountysmy(state, county, year, county_fips, counties, states)
    categ_county_list <- 
      lapply(1:dim(categ_summary)[1], FUN = function(x) categ_summary$data[[x]] %>% 
               right_join(.,  CDL_NRI_crosswalk) %>% 
               cbind(year = categ_summary$year[x], 
                     state = categ_summary$state[x], 
                     county = categ_summary$county[x],
                     fips = categ_summary$fips[x], .)) %>% 
      do.call(rbind,.)

    categ_county_list
  })  
  
  updateSelectizeInput(session, 'NRI_state_search',
                       choices = county_fips$state %>% unique() %>%
                         "["(is.na(.) == FALSE), selected = "iowa",
                       server = TRUE
  )
  output$secondsearch <- renderUI({
    selectInput("NRI_county_search", "Choose the counties:", county_fips%>% filter(state == input$NRI_state_search) %>% 
                  dplyr::select(county), multiple = TRUE)
  })

  updateSelectizeInput(session, 'NRI_category_search',
                       choices = CDL_NRI_crosswalk$NRI_categ %>% unique() %>%
                         "["(is.na(.) == FALSE) %>% sort,
                       server = TRUE
  )
  updateSelectizeInput(session, 'NRI_year_search',
                       choices = 2019:1997,
                       server = TRUE
  )
  # updateSelectizeInput(session, 'CDL_category_search',
  #                      choices = CDL_NRI_crosswalk$CDL_categ,
  #                      server = TRUE
  # )
  
  
  output$tbl_NRI_categ_county <- DT::renderDataTable({
    if(input$getcateg_county)
    {
      categ_county_list <- categ_county_list()
      categ_county_list$Freq[is.na(categ_county_list$Freq)] <- 0
      categ_county_list %>% filter(NRI_categ %in% input$NRI_category_search) %>% 
        dplyr::group_by(year, state, county, fips, NRI_categ) %>% 
        summarize(infor = paste(CDL_categ, Freq, sep = "="), 
                  Freq = sum(Freq)) %>% 
        dplyr::group_by(year, state, county, fips, Freq, NRI_categ) %>% 
        summarize(detail = paste(infor, collapse = "; ")) %>% arrange(NRI_categ, fips, year) %>% 
        "["(,c(1:4, 6, 5, 7)) %>% 
        DT::datatable(.,
                   extension = c('Scroller', 'Select', 'SearchPanes'), 
                   selection = 'none',
                   options =list(
                     deferRender = TRUE,  scrollY = 200,  scroller = TRUE,
                     pageLength = 20,
                     dom = 'Pfrtip', 
                     columnDefs = list(list(searchPanes = list(show = FALSE), targets = c(2,4,6,7))),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                       "}")
                     )
        ) %>% 
        formatStyle('Freq',  backgroundColor = styleInterval(0, c('yellow', 'grey')) )
    }
  }, server = FALSE)
  
  
  ############################################################################################
  
  ## Base Map ####
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(options = tileOptions(opacity = 0.8)) %>%
      addWMSTiles(
        baseUrl = "https://sdmdataaccess.nrcs.usda.gov/Spatial/SDM.wms",
        layers = "MapunitPoly", group = "SDL - Map Unit Polygon",
        options = WMSTileOptions(format = "image/png", transparent = TRUE,
                                 crs = leafletCRS(crsClass = "L.CRS.EPSG4326")),
        attribution = sprintf('<a href="https://sdmdataaccess.sc.egov.usda.gov">USDA NRCS Soil Data Mart database</a>. Accessed [%s]', Sys.Date())
      ) %>%
      addWMSTiles(
        baseUrl = "https://sdmdataaccess.nrcs.usda.gov/Spatial/SDM.wms",
        layers = "surveyareapolyoutline", group = "SDL - Survey Area Polygon",
        options = WMSTileOptions(format = "image/png", transparent = TRUE,
                                 crs = leafletCRS(crsClass = "L.CRS.EPSG4326")),
        attribution = sprintf('<a href="https://sdmdataaccess.sc.egov.usda.gov">USDA NRCS Soil Data Mart database</a>. Accessed [%s]', Sys.Date())
      ) -> p
    cdl.yr <- 1997:lastyr
    iter <- 0
    repeat{
      iter <- iter + 1
      p <- p %>%
        addWMSTiles(
          baseUrl = "https://nassgeodata.gmu.edu/CropScapeService/wms_cdlall.cgi",
          layers = paste("cdl", cdl.yr[iter], sep = "_"),
          group = paste("Cropland Data Layer", cdl.yr[iter], sep = " - "),
          options = WMSTileOptions(format = "image/png", transparent = TRUE,
                                   crs = leafletCRS(crsClass = "L.CRS.EPSG4326")),
          attribution = sprintf('<a href="https://nassgeodata.gmu.edu/CropScape/">USDA NASS Cropland Data Layer</a>. {%s}', cdl.yr[iter])
        )
      if(iter >= length(cdl.yr)) break
    }
    p <- p %>%
      addWMSLegend(uri = legenduri, position = "topright", layerId = "CDL legend") %>%
      addLayersControl(
        overlayGroups = c("SDL - Map Unit Polygon", "SDL - Survey Area Polygon",
                          "Map Unit Polygon", "Bounding Box"),
        baseGroups = c("None", paste0("Cropland Data Layer - ", cdl.yr)),
        options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE),
        position = "bottomleft"
      ) %>%
      hideGroup(c("SDL - Map Unit Polygon", "SDL - Survey Area Polygon")) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")) ) %>%
      addSearchOSM() %>%
      setView(-93.65, 42.0285, zoom = 12) %>%
      addDrawToolbar(
        polylineOptions = FALSE,
        polygonOptions = TRUE,
        rectangleOptions = TRUE,
        circleOptions = TRUE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        # markerOptions = drawMarkerOptions(markerIcon = myMarkerIcon(2)),
        singleFeature = TRUE,
        editOptions = editToolbarOptions()
      )
    p
  })
  
  ## CDL Markers ####
  cdl_markers <- reactiveValues(lng = NULL, lat = NULL, Id = NULL,
                                year = NULL, category = NULL, value = NULL)
  mu_markers <- reactiveValues(lng = NULL, lat = NULL, Id = NULL, areasym = NULL,
                               musym = NULL, mukey = NULL, muname = NULL, muacres = NULL)
  
  observeEvent(input$map_click,{
    click <- input$map_click
    clng <- click$lng
    clat <- click$lat
    Id <- Sys.time() %>% as.character()
    if(!input$is_mu){
      year <- cdl.year()
      CDLValue <- GetCDLValue(year, clng, clat)
      cdl_markers$lng <- c(cdl_markers$lng, clng)
      cdl_markers$lat <- c(cdl_markers$lat, clat)
      cdl_markers$Id <- c(cdl_markers$Id, Id)
      cdl_markers$year <- c(cdl_markers$year, year)
      cdl_markers$value <- c(cdl_markers$value, CDLValue[[1]])
      cdl_markers$category <- c(cdl_markers$category, CDLValue[[2]])
      leafletProxy("map") %>%
        # leaflet() %>% addTiles() %>%
        addAwesomeMarkers(
          lng = clng, lat = clat, icon = viscover:::myMarkerIcon(1), layerId = Id, clusterId = "C",
          label = HTML(
            sprintf("<style>td{padding: 5px} </style>
                   <table style = 'background:rgb(255,255,255)' border>
                   <tr><td>Location</td><td>(%.4f, %.4f)</td></tr>
                   <tr><td>Year</td><td>%s</td></tr>
                   <tr><td>Category</td><td>%s</td></tr>
                   <tr><td>Value</td><td>%s</td></tr>
                   <tr><td>Color</td><td><span style='background:%s;width:100%%;float:left'>&nbsp;</span></td></tr>
                   </table>", clng, clat, year, CDLValue[[2]], CDLValue[[1]], CDLValue[[3]])),
          labelOptions = labelOptions(
            offset = c(-80, -100), opacity = 1,
            textOnly = T, textsize = "12px",
            style = list("font-weight" = "bold",
                         "float" = "left",
                         "width" = "100%"))
        )
    }
    if(input$is_mu){
      qres <- GetSDLValue(clng, clat)
      mu_markers$lng <- c(mu_markers$lng, clng)
      mu_markers$lat <- c(mu_markers$lat, clat)
      mu_markers$Id <- c(mu_markers$Id, Id)
      mu_markers$areasym <- c(mu_markers$areasym, qres[1])
      mu_markers$musym <- c(mu_markers$musym, qres[2])
      mu_markers$mukey <- c(mu_markers$mukey, qres[3])
      mu_markers$muname <- c(mu_markers$muname, qres[4])
      mu_markers$muacres <- c(mu_markers$muacres, qres[5])
      leafletProxy("map") %>%
        addAwesomeMarkers(
          lng = clng, lat = clat, icon = viscover:::myMarkerIcon(2), layerId = Id, clusterId = "P",
          label = HTML(
            sprintf("<style>td{padding: 5px} </style>
                     <table style = 'background:rgb(255,255,255)' border>
                     <tr><td>Location</td><td>(%.4f, %.4f)</td></tr>
                     <tr><td>areasym</td><td>%s</td></tr>
                     <tr><td>musym</td><td>%s</td></tr>
                     <tr><td>mukey</td><td>%s</td></tr>
                     <tr><td>muname</td><td>%s</td></tr>
                     <tr><td>muacres</td><td>%s</td></tr>
                     </table>", clng, clat,
                    qres[1], qres[2], qres[3], qres[4], qres[5])),
          labelOptions = labelOptions(
            offset = c(-100, -120), opacity = 1,
            textOnly = TRUE, textsize = "12px",
            style = list("font-weight" = "bold",
                         "float" = "left",
                         "width" = "100%")))
    }
  })
  
  observeEvent(input$map_marker_click,{
    # browser()
    Id.rm_c <- which(cdl_markers$Id == input$map_marker_click$id)
    if(length(Id.rm_c)){
      cdl_markers <- lapply(cdl_markers, function(x) x[-Id.rm_c])
    }
    
    Id.rm_s <- which(mu_markers$Id == input$map_marker_click$id)
    if(length(Id.rm_s)){
      mu_markers <- lapply(mu_markers, function(x) x[-Id.rm_s])
    }
    
    leafletProxy("map") %>% removeMarker(layerId = input$map_marker_click$id)
  })
  
  ## Soil Mapunit Polygons ####
  bb.poly <- eventReactive(input$map_draw_new_feature,{
    coords <- input$map_draw_new_feature$geometry$coordinates %>%
      unlist %>% matrix(nc = 2, byrow = T)
    if(nrow(coords) >= 4) {
      bb.poly <- sp::Polygon(coords) %>% list %>% sp::Polygons(ID=1) %>% list %>% sp::SpatialPolygons()
    }
    else {
      d <- ifelse(input$map_draw_start$feature_type == "circle",
                  input$map_draw_new_feature$properties$radius,
                  500)
      circ <- dismo::circles(coords, d = d, lonlat = TRUE)
      bb.poly <- circ@polygons
    }
  })
  
  mu.polys <- reactive({
    input$go_mu
    bb.poly <- isolate(bb.poly())
    b2 <- as.vector(bbox(bb.poly))
    withProgress(message = "Scraping SDL data", value = 0.5, {
      mu.polys <- soilDB::mapunit_geom_by_ll_bbox(b2)
      incProgress(0.5)
    })
    mu.polys@data <- mu.polys@data %>% group_by(mukey) %>%
      mutate(muacres = sum(as.numeric(muareaacres))) %>% ungroup
    mu.polys
  })
  
  observeEvent(input$go_mu,{
    bb.poly <- bb.poly()
    mu.polys <- mu.polys()
    b2 <- as.vector(sp::bbox(mu.polys))
    
    leafletProxy("map", session) %>%
      clearShapes() %>%
      addPolygons(data = as(raster::extent(sp::bbox(bb.poly)), "SpatialPolygons"),
                  group = "Bounding Box", fill = FALSE,
                  stroke = TRUE, color = "red", weight = 4) %>%
      addSearchFeatures(
        targetGroups  = 'Map Unit Polygon',
        options = searchFeaturesOptions(
          propertyName = "popup",
          zoom = 15, autoCollapse = TRUE,
          textPlaceholder = "Search map units",
          openPopup = TRUE)) %>%
      fitBounds(lng1 = b2[1], lng2 = b2[3], lat1 = b2[2], lat2 = b2[4])
  })
  
  observe({
    if(length(input$tbl_mu_rows_selected)){
      tb_s <- input$tbl_mu_rows_selected
      tb_mapunit <- tb_mapunit()
      mu.polys <- mu.polys()
      mu.polys.sub <- mu.polys %>% subset(mukey %in% unlist(tb_mapunit[tb_s,"mukey"]))
      leafletProxy("map", session) %>%
        clearGroup("Map Unit Polygon") %>%
        addPolygons(data = mu.polys.sub, group = "Map Unit Polygon",
                    fillColor = ~colorNumeric("YlOrBr", mu.polys@data$muacres)(muacres),
                    stroke = TRUE, color = "black", weight = 3,  fillOpacity = 0.4,
                    popup = ~paste0("Survey area symbol: ", areasymbol, "<br>",
                                    "Map unit symbol: ", musym, "<br>",
                                    "Map unit key: ", mukey),
                    highlightOptions = highlightOptions(color = "red", bringToFront = TRUE,
                                                        weight = 5, fillOpacity = 0))
    }
    if(length(input$tbl_mu_rows_selected)==0){
      mu.polys <- mu.polys()
      leafletProxy("map", session) %>%
        clearGroup("Map Unit Polygon") %>%
        addPolygons(data = mu.polys, group = "Map Unit Polygon",
                    fillColor = ~colorNumeric("YlOrBr", muacres)(muacres),
                    stroke = TRUE, color = "black", weight = 3, fillOpacity = 0.4,
                    popup = ~paste0("Survey area symbol: ", areasymbol, "<br>",
                                    "Map unit symbol: ", musym, "<br>",
                                    "Map unit key: ", mukey),
                    highlightOptions = highlightOptions(color = "red", bringToFront = TRUE,
                                                        weight = 5, fillOpacity = 0))
    }
  })
  
  ## Soil Box ####
  tb_mapunit <- reactive({
    mu.polys <- mu.polys()
    mu.polys@data %>% dplyr::select(-muareaacres, -mupolygonkey) %>%
      unique %>% arrange(desc(muacres)) %>% dplyr::select(mukey, everything())
  })
  
  tb_mapunit_area <- reactive({
    p <- rgeos::writeWKT(bb.poly())
    q <- paste0(
      "SELECT areasymbol, musym, mukey, muname, muacres, mukind, farmlndcl
         FROM mapunit mu
         INNER JOIN legend on legend.lkey = mu.lkey       
         WHERE mukey IN (
         SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('", p, "')
         )")
    qres <- soilDB::SDA_query(q)
    qres %>%
      mutate(mukey = as.factor(mukey)) %>%
      arrange(desc(muacres)) %>%
      dplyr::select(mukey, everything())
  })
  
  output$tbl_mu <- DT::renderDataTable({
    # if(input$mu_type == 1) {
    tb <- tb_mapunit()
    # }
    # if(input$mu_type == 2){
    #   tb <- tb_mapunit_area()
    # }
    tb_ref <- tb_mapunit()
    DT::datatable(tb, caption = "Mapunit Polygons intersecting with the area bounded by the red box.") %>%
      formatRound("muacres", 0) %>%
      formatStyle("mukey", fontWeight = "bold",
                  backgroundColor = styleEqual(
                    tb_ref$mukey, colorNumeric("YlOrBr", tb_ref$muacres)(tb_ref$muacres)))
  })
  
  ## CDL Box ####
  cdl.year <- reactive({
    input$go_cdl
    group <- grep("Cropland", isolate(input$map_groups), value = TRUE)
    return(ifelse(length(group), gsub("Cropland Data Layer - ", "", group), lastyr))
  })
  
  cdl.tile <- reactive({
    return(GetCDLFile(cdl.year(), mu.polys()))
  })
  
  cdl.tile.bb <- reactive({
    return(GetCDLFile(cdl.year(), bb.poly()))
  })
  
  cdl_tibble <- reactive({
    if(input$cdl_type == 1){
      cdl.tile <- cdl.tile.bb()
      tb <- cdl.tile %>% raster::getValues() %>% table %>% as.data.frame
    }
    if(input$cdl_type == 2){
      cdl.tile <- cdl.tile()
      mu.polys <- mu.polys()
      tb_mapunit <- tb_mapunit()
      tb_s <- input$tbl_mu_rows_selected
      if(length(tb_s)) {
        mu.polys.sub <- mu.polys %>% subset(mukey %in% unlist(tb_mapunit[tb_s,"mukey"]))
        tb <- TileinPoly(cdl.tile, mu.polys.sub)
      }
      if(length(tb_s)==0) {
        tb <- TileinPoly(cdl.tile, mu.polys)
      }
    }
    names(tb) <- c("VALUE", "COUNT")
    tb2 <- tb %>% mutate(VALUE = VALUE %>% as.character %>% as.integer) %>%
      left_join(cdl.dbf[,1:2], by = "VALUE")
    tb2 %>% dplyr::select(VALUE, CATEGORY = CLASS_NAME, COUNT) %>% arrange(desc(COUNT))
  })
  
  output$tbl_cdl <- DT::renderDataTable({
    tb <- cdl_tibble()
    DT::datatable(tb, caption = paste("CDL Year", cdl.year())) %>%
      formatStyle("CATEGORY", fontWeight = "bold",
                  backgroundColor = styleEqual(tb$CATEGORY, cdlpal(tb$VALUE)))
  })
  
  output$plot_cdl <- renderPlotly({
    d <- cdl_tibble()
    plot_ly(data = d %>% filter(!is.na(CATEGORY)),
            x = ~COUNT, y = ~reorder(CATEGORY, COUNT),
            type = 'bar', orientation = 'h',
            marker = list(color = ~cdlpal(VALUE))) %>%
      layout(margin = list(l = 160, b = 25),
             xaxis = list(showgrid = TRUE, title = "", zeroline = FALSE),
             yaxis = list(title = "")) -> p
    p$elementId <- NULL
    p
  })
  
  ## Download Handlers ####
  output$download_mu <- downloadHandler(
    filename = function(){
      paste0('soil_mapunit-', Sys.Date(), '.csv')
    },
    content = function(con){
      # if(input$mu_type == 1)
      write.csv(tb_mapunit(), con)
      #   if(input$mu_type == 2)
      #     write.csv(tb_mapunit_area(), con)
    }
  )
  
  output$download_cdl <- downloadHandler(
    filename = function(){
      paste0('cdl_', cdl.year(), '-', Sys.Date(), '.csv')
    },
    content = function(con){
      write.csv(cdl_tibble(), con)
    }
  )
  
  out_markers <- reactive({
    # browser()
    if(input$is_mu){
      marker_ls <- reactiveValuesToList(mu_markers)
      marker_ls$muname <- gsub(",", ";", marker_ls$muname)
    }
    if(!input$is_mu){
      marker_ls <- reactiveValuesToList(cdl_markers)
    }
    marker_tb <- do.call("cbind", marker_ls) %>% as.data.frame() %>%
      dplyr::select(lng, lat, everything()) %>% dplyr::select(-Id) %>%
      sapply(unlist)
    return(marker_tb)
  })
  
  output$download_cdl_markers <- downloadHandler(
    filename = function(){
      sprintf('%s_markers-%s.csv', ifelse(input$is_mu, "mu", "cdl"), Sys.Date())
    },
    content = function(con){
      write.csv(out_markers(), con, row.names = FALSE, quote = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)
