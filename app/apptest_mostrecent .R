library(dplyr)
library(tidyr)
library(magrittr)
library(dygraphs)
library(xts)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(scales)
library(leaflet)
library(sf)
library(stringr)
library(viridis)

#setwd("app")

#still to do
#make map gray on places where there is not data

`%not_in%` <- function (x, table) match(x, table, nomatch = 0L) == 0L

inflation <- readr::read_csv("cpi.txt", skip = 6, col_types = "nn")

inflation %<>%
  transmute(year = Year,
            cpi = first(Annual) / Annual)

# Pre-App Data Work ----
# if("glpdata_exports.RData" %not_in% list.files()) {
#   
#   zip_map <- glptools::map_zip
#   
#   HPI_zip <- glpdata::HPI_zip_forapp
#   
#   save(zip_map, HPI_zip, file  = "glpdata_exports.RData")
# }

load("glpdata_exports.RData")

HPI_zip %<>%
  ungroup() %>%
  mutate(zip = as.character(zip)) %>%
  complete(zip, year = 1975:2000) %>%
  left_join(inflation, by = "year") %>%
  mutate(
    HPI_old = HPI_2000,
    HPI_2000 = HPI_2000 * cpi)

load("HPI_lou.RData")

HPI_lou %<>%
  left_join(inflation, by = "year") %>%
  mutate(
    HPI_old = HPI,
    HPI = HPI * cpi)

addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins   
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}


temp_df <- filter(HPI_zip, year == 2020) %>%
  mutate(HPI_value = HPI_2000 * 100000 / 100)

temp_pal <- colorNumeric(
  palette = "viridis",
  domain = temp_df$HPI_value)
  
starting_map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.HOT) %>%
  addPolygons(data=left_join(zip_map, temp_df, by = "zip"),
              fillColor = ~temp_pal(HPI_value), 
              fillOpacity = 0.5,
              opacity = 1, 
              color = "#FFFFFF",
              weight = 2,
              label = zip_map$zip) %>%
  #ok so add poloygons is what we are going to change
  setView(lng = -85.63, lat = 38.20, zoom = 10) %>%
  addLegend(position = "bottomright", 
            values = c(range(temp_df$HPI_value, na.rm = T), NA_real_),
            pal = temp_pal)

#glp_colors <- c("#d63631", "#eaab21", "#8699ac", "#63b692") #a7bfd7", "#7CE3B6")
glp_colors <- c("#0e4a99", "#f58021", "#00a9b7", 
                "#800055", "#356E39", "#CFB94C", 
                "#7E9C80")

#UI ----
#Inputs ----
ui <- fluidPage(

  useShinyjs(),

  theme = shinythemes::shinytheme("lumen"),

  tags$head(includeCSS("www/styles.css")),
  tags$head(tags$style('h2 {color:#00a0af;}')),

  fluidRow(
    div(style="text-align: center;",
        h2("Housing Price Changes over Time Across Louisville Zipcodes"),
        h5("The median price of an owner-occupied home in Jefferson County increased from $55,500 in 1990 to $185,700 in 2019. 
            Rising home prices in Louisville and across the country are the result of several factors: inflation has increased the price of houses alongside other goods, 
            construction of new houses and the renovation of older houses has increased the size and quality of homes, and the value of homes has simply gone up.
            Homeownership provides a source of wealth-generation and stability, but most data on housing prices does not focus on the value of existing homes; instead
            it reflects the combined impact of construction, inflation, and rising values. While that data is important for understanding housing affordability for new buyers,
            it does not describe changes in home equity for current homeowners."),
        h5("This tool shows data from the HUD Housing Price Index. It measures the change in value of single-family homes, and it controls for the impact of
            construction and inflation on home prices. In other words, it compared the real value of homes to their value in the past."),
        h5("To use the tool, select zip codes from the dropdown menu or by clicking on the map. You can change the starting year, ending year,
            and purchase price of a hypothetical home. The data you see on the graphs are controlled for inflation, and the dollar values shown reflect the first year on the slider."))
  ),#close title

  hr(),

   fluidRow(

    column(4,
           wellPanel(
           div(style="text-align: center;",
               selectInput("zipcode",
                           h4("Select zipcodes to compare"),
                           choices = unique(HPI_zip$zip),
                           selected = NULL,
                           multiple=TRUE)))),
    column(4,
           wellPanel(
             div(style="text-align: center;",
                 sliderInput("years",
                             h4("Select years to compare"),
                             min = 1975,
                             max = 2020,
                             value = c(2000,2020),
                             sep = "")))),
    column(4,
           wellPanel(
           div(style="text-align: center;",
               shinyWidgets::autonumericInput(
                            inputId = "price",
                            label= h4("Select a starting home value"),
                            value=100000,
                            min=0,
                            max=1000000,
                            currencySymbol = "$",
                            currencySymbolPlacement = "p",
                            step=100))))

  ), #closes input fluid row

#Outputs----

#text outputs (2nd row)

  hr(),
  fluidRow(
    column(12,
         div(style="text-align: center;",
             h4(uiOutput("zips")))
    ), #close column1
  ), #close fluid row

#sentence above map
  hr(),

  fluidRow(
    
    uiOutput("map_title"),
  ),#close this small statement

#map and graph
fluidRow(

  column(width = 6,

         leafletOutput("map", height = "400px"),
  ),

  column(width = 6,
         dygraphOutput(outputId = "plot", height = "400px")
  )

) #closes output fluid row

) #closes fluid page

#Server ----
server <- function(input, output, session) {
# 
#   zipcode <- reactive({as.vector(input$zipcode)})
#   year1 <- reactive({input$years[1]})
#   year2 <- reactive({input$years[2]})
#   homevaluedollar <- reactive({dollar(input$price)})
#   homevalue <- reactive({input$price})
  
#Statements ----
  zip_rebase <- reactive({
    
    HPI_zip %<>%
      filter(year >= input$years[1],
             year <= input$years[2]) %>%
      group_by(zip) %>%
      mutate(HPI_new = HPI_2000 / HPI_2000[year == input$years[1]] * 100,
             HPI_value = round(HPI_new * input$price / 100, 0),
             zip_color = "#FFFFFF")
    
    if(length(input$zipcode) > 0) {
      for(i in 1:length(input$zipcode)) {
        this_color <- glp_colors[(i - 1) %% length(glp_colors) + 1]
        
        HPI_zip %<>%
          mutate(zip_color = if_else(zip == input$zipcode[i] , this_color, zip_color))
        
      }
    }
    
    HPI_zip
    
  })
  
  city_rebase <- reactive({
    
    HPI_lou %>%
      filter(year >= input$years[1],
             year <= input$years[2]) %>%
      mutate(HPI_new = HPI / HPI[year == min(year)] * 100)
    
  })
  
  this_map <- reactive({
    
    zip_rebase() %>%
      filter(year == input$years[2]) %>%
      select(zip, HPI_new, HPI_value, zip_color) %>%
      left_join(zip_map, ., by = "zip")
    
  })
  
  output$map_title <- renderUI({
    h3(paste0("Average home values in ", input$years[2], " for homes worth ", dollar(input$price), " in ", input$years[1]), align = "center")
  })
  
  observe({
    
    zipcode <- as.vector(input$zipcode)
    year1 <- input$years[1]
    year2 <- input$years[2]
    homevaluedollar <- dollar(input$price)
    homevalue <- input$price
    original_data <- zip_rebase()
    
    output$zips <- renderUI({
      if (length(zipcode) == 0) {
        HTML("Select a zipcode to begin.")
      } else {
        temp <- ""
        for(i in 1:length(zipcode)){
          this_zip <- original_data %>% filter(zip %in% zipcode[i])
          this_color <- this_zip %>% pull(zip_color) %>% unique()
          
          format_pre <- paste0('<font color=\"', this_color, '\"><b><u>')
          format_suf <- paste0('</u></b></font>')
          
          this_hpi <- this_zip %>% filter(year == year2) %>% pull(HPI_new)
          this_hpi_old <- this_zip %>% filter(year == year2) %>% pull(HPI_old)
          
          if (is.na(this_hpi)){
            
            min_year <- original_data %>%
              filter(zip %in% zipcode[i], !is.na(HPI_2000)) %>%
              pull(year) %>%
              min(na.rm = TRUE)
            
            temp <- paste0(temp,"Data is not available for zip code ", format_pre, zipcode[i], format_suf, " until ", min_year, ".", br(),br())
            
          } else {

            this_percent <- percent(abs(this_hpi - 100), scale = 1, accuracy = 0.1)
            newprice <- dollar(homevalue * this_hpi / 100, accuracy = 1)
            newprice2 <- dollar(homevalue * this_hpi_old / 100, accuracy = 1)
            
            this_word <- if_else(this_hpi - 100 > 0, "increased", "decreased")
            
            temp <- paste0(temp, "In ", format_pre, zipcode[i], format_suf, ", home values ", format_pre, this_word, format_suf, 
                           " by ", format_pre, this_percent, format_suf, " from ", 
                           year1, " to ", year2, ". An average home that was worth ", homevaluedollar, " in ", year1, " would be worth ", 
                           format_pre, newprice, format_suf, " in ", year2, 
                           ". Without controlling for inflation, that would be ", format_pre, newprice2, format_suf, " in today's dollars.", br(),br())
            rm(this_zip)
          }
        }
        HTML(temp)
      }
      
    }) #close renderUI
  }) #close observe

  #Create basemap ----
  #we need a dataframe with a zip column and a percent change column

  #first reset base year to the proper year

  #make data frame that adds all years to every zip, NA if no data (currently only years for which data exist appear)
  #^can do this with expand.grid
  #filter where year == NA
  # then create index_year1 column
  #when this dataframe is eventually joined to zip_map it will report NAs for missing zipcodes
  
  output$map <- renderLeaflet(starting_map)
    
  observeEvent(this_map(), {
    
    map_obj <- this_map()
    
    this_pal <- colorNumeric(
      palette = "viridis",
      domain = map_obj$HPI_value) 
    
    map_obj %<>%
      mutate(
        these_words = if_else(HPI_new - 100 > 0, "increased", "decreased"),
        this_label_text = if_else(
          is.na(map_obj$HPI_new),
          paste0("Data is not available for ", input$years[1], " in <b>", map_obj$zip, "</b>. Please choose a more recent year."),
          paste0("Prices <b>", these_words, "</b> by an average of ", 
                 "<b>", scales::percent(abs(map_obj$HPI_new - 100), scale = 1, accuracy = 0.1), "</b>",
                 " between ", input$years[1], " and ", input$years[2], ".")),
        this_label_text = if_else(zip == "40209",
                                  "This zip code is mostly the airport and fairgrounds. Housing price data is not available for it.",
                                  this_label_text),
        this_label_text = if_else(zip == "40225",
                                  "This zip code is GE Appliance Park. Housing price data is not available for it.",
                                  this_label_text),
        these_labels = paste0("<center><b>", zip, "</b></center>", this_label_text) %>%
          lapply(htmltools::HTML),
        bg_weights = if_else(zip_color == "#FFFFFF", 1, 6),
        weights = if_else(zip_color == "#FFFFFF", 1, 3))
    
    
    # Organize object so most recently-clicked polygon is loaded last (on top)
    if(length(input$zipcode) > 0) {
      
      map_obj <-
        bind_rows(
          map_obj[map_obj$zip %not_in% input$zipcode,],
          map_obj[map_obj$zip %in% input$zipcode,]
        )
      
    }
    
    leafletProxy("map", data = map_obj) %>%
      clearControls() %>%
      clearShapes() %>%
      # addMapPane("top", 442) %>%
      # addMapPane("bottom", 441) %>%
      addMapPane("polygons", zIndex = 420) %>% # shown below ames_circles
      addMapPane("lines", zIndex = 430) %>% # shown above ames_lines
      
      # color
      addPolygons(fillColor = ~this_pal(HPI_value),
                  fillOpacity = 0.5,
                  stroke = F,
                  opacity = 1,
                  smoothFactor = 1.5) %>%
      
      # white
      addPolygons(fillOpacity = 0,
                  opacity = 1,
                  color = "#FFFFFF",
                  weight = map_obj$bg_weights,
                  smoothFactor = 1.5) %>%
      
      addPolygons(fillOpacity = 0,
                  opacity = 1,
                  color = map_obj$zip_color,
                  weight = map_obj$weights,
                  label = map_obj$these_labels,
                  layerId = map_obj$zip,
                  smoothFactor = 1.5) %>%
      
      
      # addPolygons(fillColor = ~this_pal(HPI_value), 
      #             fillOpacity = 0.5,
      #             #stroke = F,
      #             opacity = 1,
      #             color = map_obj$zip_color,
      #             weight = map_obj$weights,
      #             label = map_obj$these_labels,
      #             layerId = map_obj$zip,
      #             options = pathOptions(pane = "polygons")) %>%

      # addPolygons(fillOpacity = 0,
      #             opacity = 1, 
      #             color = "#FFFFFF",
      #             weight = map_obj$bg_weights) %>%
      addLegend_decreasing(position = "bottomright", decreasing = T,
                values = c(range(map_obj$HPI_value, na.rm = T), NA_real_),
                pal = this_pal)
    
  })
  
  output$plot <- renderDygraph({
    
    zip_df <- zip_rebase()
    city_df <- city_rebase()
    
    zip_df %<>%
      filter(zip %in% input$zipcode) %>%
      transmute(geog = zip, year, HPI_new)
    
    city_df %<>%
      transmute(geog = "Louisville", year, HPI_new)
    
    this_df <- bind_rows(zip_df, city_df) %>%
      mutate(HPI_new = round(HPI_new * input$price / 100, 0))
    
    this_df %<>%
      mutate(year = paste0("1/1/", year) %>% as.POSIXct(format = "%m/%d/%Y")) %>%
      pivot_wider(id_cols = "year", names_from = "geog", values_from = "HPI_new")
    
    max_value <- max(this_df[names(this_df) != "year"], na.rm = TRUE)
    
    ts <- xts::xts(x = this_df, 
                   order.by = this_df$year)
    
    output_graph <- dygraph(ts)
    #main = paste0("Housing Price Changes since ", input$years[1]))
    
    output_graph %<>%
      dySeries("Louisville", label = "City Average", color = "#323844", strokeWidth = 3)
    
    if(length(input$zipcode) > 0) {
      for(z in 1:length(input$zipcode)) {
        output_graph %<>%
          dySeries(input$zipcode[z], 
                   color = glp_colors[(z - 1) %% length(glp_colors) + 1], # starts at 1 and goes 1 through 4
                   strokeWidth = 3)
      }
    }
    
    output_graph %<>% dyLegend(show = "always")
    
    output_graph %<>%
      dyLimit(input$price, strokePattern = "solid") %>%
      dyLegend(width = 500) %>%
      dyAxis("x",
             "Year",
             axisLabelFormatter = "function(d) { return d.getFullYear() }",
             pixelsPerLabel = 40,
             axisLabelWidth = 40,
             rangePad = 10) %>%
             # ticker = "function(a, b, pixels, opts, dygraph, vals) {
             #         return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)
             #       }") %>%
      dyAxis("y",
             "Estimated Value" ,
             axisLabelFormatter = "function(v){return '$' + v.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');}",
             valueFormatter = "function(v){return '$' + v.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');}",
             valueRange = c(0, max_value * 1.10),
             rangePad = 10,
             axisLabelWidth = 85)
    
    output_graph
    
  })
  
  observeEvent( input$map_shape_click, {
    current_zips <- input$zipcode
    
    map_click <- input$map_shape_click$id
    
    if (map_click %in% current_zips) {
      new_zips <- setdiff(current_zips, map_click)
    } else {
      new_zips <- c(current_zips, map_click)
    }
    
    updateSelectInput(
      session,
      "zipcode",
      selected = new_zips)
  })

} #closes server

shinyApp(ui,server)

