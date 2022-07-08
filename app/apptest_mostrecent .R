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

library(profvis)

#setwd("app")

#still to do
#make map gray on places where there is not data

`%not_in%` <- function (x, table) match(x, table, nomatch = 0L) == 0L

inflation <- readr::read_csv("cpi.txt", skip = 6, col_types = "nn")

inflation %<>%
  transmute(year = Year,
            cpi = Annual)

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
    HPI_unadjusted = HPI_2000)

load("HPI_lou.RData")

HPI_lou %<>%
  left_join(inflation, by = "year") %>%
  mutate(
    HPI_unadjusted = HPI)

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
  mutate(HPI_value = HPI_unadjusted * 100000 / 100)

temp_pal <- colorNumeric(
  palette = "viridis",
  domain = temp_df$HPI_unadjusted)
  
starting_map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.HOT) %>%
  addPolygons(data=left_join(zip_map, temp_df, by = "zip"),
              fillColor = ~temp_pal(HPI_unadjusted), 
              fillOpacity = 0.5,
              opacity = 1, 
              color = "#FFFFFF",
              weight = 2) %>%
  #ok so add poloygons is what we are going to change
  setView(lng = -85.63, lat = 38.20, zoom = 10) %>%
  addLegend(position = "bottomright", 
            values = c(range(temp_df$HPI_unadjusted, na.rm = T), NA_real_),
            pal = temp_pal,
            title = "Price in 2020",
            labFormat = labelFormat(prefix = "$"))

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

  # The median price of an owner-occupied home in Jefferson County increased from $55,500 in 1990 to $185,700 in 2019. 
  # Rising home prices in Louisville and across the country are the result of several factors: inflation has increased the price of houses alongside other goods, 
  # construction of new houses and the renovation of older houses has increased the size and quality of homes, and the value of homes has simply gone up.
  
  fluidRow(
    div(style="text-align: center;",
        h2("Housing Price Growth Across Louisville Zipcodes"),
        h5("Homeownership provides a source of wealth-generation and stability, but most data on housing prices does not focus on the value of existing homes; instead
            it reflects the combined impact of construction, inflation, and rising values. While that data is important for understanding housing affordability for new buyers,
            it doesn't describe changes in home equity for current homeowners. This tool shows data from the ", tags$a(href='https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index.aspx', "HUD Housing Price Index"), ". 
            It measures the change in value of single-family homes, and it controls for the impact of construction and renovation on home prices. 
            In other words, it compares the real value of homes to their value in the past.", style='font-size:16px'),
        h5("To use the tool, select zip codes from the dropdown menu or by clicking on the map. You can change the starting year, ending year,
            and purchase price of a hypothetical home. You have the option of controlling for inflation, which can tell you more about the value of homes as an investment but can make the data more difficult to interpret.
            When adjusting for inflation, the dollar values shown reflect the first year on the slider.", style='font-size:16px'))
  ),#close title

  hr(),

   fluidRow(

    column(3,
           wellPanel(
           div(style="text-align: center;",
               selectInput("zipcode",
                           h4("Select zipcodes to compare"),
                           choices = unique(HPI_zip$zip),
                           selected = NULL,
                           multiple=TRUE)))),
    column(3,
           wellPanel(
             div(style="text-align: center;",
                 sliderInput("years",
                             h4("Select years to compare"),
                             min = 1975,
                             max = 2020,
                             value = c(2000,2020),
                             sep = "")))),
    column(3,
           wellPanel(
           div(style="text-align: center;",
               shinyWidgets::autonumericInput(
                            inputId = "price",
                            label= h4("Select a starting home price"),
                            value=100000,
                            min=0,
                            max=1000000,
                            currencySymbol = "$",
                            currencySymbolPlacement = "p",
                            step=100)))),
    # column(3,
    #        wellPanel(
    #          div(style="text-align: center;",
    #              awesomeCheckbox(
    #                inputId = "inflation",
    #                label = h4("Adjust all data for inflation?"),
    #                value = FALSE))))
    # 
    column(3,
           wellPanel(
             div(style="text-align: center;",
                 HTML(
                 '<div class="form-group shiny-input-container">
                    <label style="cursor: pointer;" for="inflation">
                      <h4>Adjust all data for inflation?</h4>
                    </label>
                    <div class="awesome-checkbox checkbox-primary">
                      <input id="inflation" type="checkbox" data-shinyjs-resettable-id="inflation" data-shinyjs-resettable-type="Checkbox" data-shinyjs-resettable-value="false" class="shinyjs-resettable shiny-bound-input">
                    </div>
                  </div>'))))
                  # conditionalPanel(
                  #   "input.inflation == true",
                  #   uiOutput("inflation_choices")))))
                  
    
    # column(4,
    #        wellPanel(
    #          div(style="text-align: center;",
                 # awesomeCheckbox(
                 #   inputId = "inflation",
                 #   label = "Adjust all data for inflation?",
                 #   value = FALSE))))

  ), #closes input fluid row


  
#Outputs----

#text outputs (2nd row)

  hr(),
  fluidRow(
    column(12,
         div(style="text-align: center;",
             h4(uiOutput("zips"), style='font-size:16px'))
    ), #close column1
  ), #close fluid row

#sentence above map
  hr(),

  fluidRow(
    
    uiOutput("map_title"),
  ),#close this small statement

#statement over graph
  fluidRow(
    column(6, p("(Hover over the map to see data for each zipcode.)", align = "center", style='color:#808080')),
    column(6, p("(Hover over the graph to see prices for each selected zipcode and the city average.)", align = "center", style='color:#808080'))
  ),


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
  
  # output$inflation_choices <- renderUI(
  #   radioGroupButtons(
  #     inputId = "inflation_year",
  #     label = "Use dollar values from:",
  #     choices = input$years))
  
#Statements ----
  zip_rebase <- reactive({
    
    HPI_zip %<>%
      filter(year >= input$years[1],
             year <= input$years[2]) %>%
      group_by(zip) %>%
      mutate(
        HPI_raw = HPI_unadjusted, # keep track of years for which data is available
        zip_color = "#FFFFFF")
  
    print(input$inflation_year)
    
    if (!is.null(input$inflation_year)) {
      inflation_year <- input$inflation_year
    } else {
      inflation_year <- input$years[1]
    }
    
    HPI_zip %<>%
      mutate(
        HPI_raw = HPI_unadjusted, # keep track of years for which data is available
        HPI_unadjusted = HPI_unadjusted / HPI_unadjusted[year == input$years[1]] * 100,
        HPI_inflation = HPI_unadjusted / cpi * cpi[year == inflation_year],
        price_unadjusted = round(HPI_unadjusted * input$price / 100, 0),
        price_inflation = round(HPI_inflation * input$price / 100, 0))
    
    if (input$inflation) {
      HPI_zip$HPI <- HPI_zip$HPI_inflation
      HPI_zip$price <- HPI_zip$price_inflation
    } else {
      HPI_zip$HPI <- HPI_zip$HPI_unadjusted
      HPI_zip$price <- HPI_zip$price_unadjusted
    }
    
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
    
    HPI_lou %<>%
      filter(year >= input$years[1],
             year <= input$years[2])
    
    HPI_lou %<>%
      mutate(
        HPI_unadjusted = HPI_unadjusted / HPI_unadjusted[year == input$years[1]] * 100,
        HPI_inflation = HPI_unadjusted / cpi * cpi[year == input$years[1]],
        price_unadjusted = round(HPI_unadjusted * input$price / 100, 0),
        price_inflation = round(HPI_inflation * input$price / 100, 0))
    
    if (input$inflation) {
      HPI_lou$HPI <- HPI_lou$HPI_inflation
      HPI_lou$price <- HPI_lou$price_inflation
    } else {
      HPI_lou$HPI <- HPI_lou$HPI_unadjusted
      HPI_lou$price <- HPI_lou$price_unadjusted
    }
    
    HPI_lou

  })
  
  this_map <- reactive({
    
    zip_rebase() %>%
      filter(year == input$years[2]) %>%
      select(zip, HPI_unadjusted, HPI_inflation, price_unadjusted, price_inflation, HPI, price, zip_color) %>%
      left_join(zip_map, ., by = "zip")
    
  })
  
  output$map_title <- renderUI({
    
    inflation_note <- if_else(input$inflation, paste0(" (in ", input$years[1], " dollars)"), "")
    
    h3(paste0("Average home prices in ", input$years[2], " for homes worth ", dollar(input$price), 
              " in ", input$years[1], inflation_note), align = "center")
  })
    
  observe({
    
    zipcode <- as.vector(input$zipcode)
    year1 <- input$years[1]
    year2 <- input$years[2]
    homevaluedollar <- dollar(input$price)
    original_data <- zip_rebase()
    
    output$zips <- renderUI({
      if (length(zipcode) == 0) {
        HTML("To begin, select a zipcode from the dropdown above or by clicking the map below.")
      } else {
        temp <- ""
        for(i in 1:length(zipcode)){
          this_zip <- original_data %>% filter(zip %in% zipcode[i])
          this_color <- this_zip %>% pull(zip_color) %>% unique()
          
          format_pre <- paste0('<font color=\"', this_color, '\"><b>')
          format_suf <- paste0('</b></font>')
          
          this_zip %<>% filter(year == year2)
          this_HPI <- this_zip %>% pull(HPI_unadjusted)
          
          if (is.na(this_HPI)){
            
            min_year <- original_data %>%
              filter(zip == zipcode[i], !is.na(HPI_raw)) %>%
              pull(year) %>%
              min(na.rm = TRUE)
            
            temp <- paste0(temp,"Data is not available for zip code ", format_pre, zipcode[i], format_suf, " until ", min_year, ".", br(),br())
            
          } else {

            this_price <- this_zip %>% pull(price_unadjusted) %>% dollar(accuracy = 1)
            this_percent  <- percent(abs(this_HPI - 100), scale = 1, accuracy = 0.1)
            this_word <- if_else(this_HPI - 100 > 0, "increased", "decreased")
            
            temp <- paste0(temp, "In ", format_pre, zipcode[i], format_suf, ", home values ", format_pre, this_word, format_suf, 
                           " by ", format_pre, this_percent, format_suf, " from ", 
                           year1, " to ", year2, ". An average home that was worth ", homevaluedollar, " in ", year1, " would be worth ", 
                           format_pre, this_price, format_suf, " in ", year2, ".", br())
            
            if (input$inflation) {
              this_HPI_inflation <- this_zip %>% pull(HPI_inflation)
              this_price_inflation <- this_zip %>% pull(price_inflation) %>% dollar(accuracy = 1)
              this_percent_inflation  <- percent(abs(this_HPI_inflation - 100), scale = 1, accuracy = 0.1)
              this_word_inflation <- if_else(this_HPI_inflation - 100 > 0, "increased", "decreased")
              inflation_note <- if_else(input$inflation, paste0(" (in ", input$years[1], " dollars)"), "")
              
              temp <- paste0(temp, "Adjusting for inflation, home values in ", format_pre, zipcode[i], " ", this_word_inflation, format_suf, 
                             " by ", format_pre, this_percent_inflation, format_suf, " from ", 
                             year1, " to ", year2, ". An average home that was worth ", homevaluedollar, " in ", year1, " would be worth ", 
                             format_pre, this_price_inflation, format_suf, " in ", year2, " (in ", input$years[1], " dollars).", br())
            }
            
            temp <- paste0(temp, br())
            
            #" In ", year1, " dollars, that would be ", format_pre, price_inflation, format_suf, ".", br(),br())
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
      domain = map_obj$price) 
    
    inflation_note <- if_else(input$inflation, paste0(" (in ", input$years[1], " dollars)"), "")
    
    map_obj %<>%
      mutate(
        these_words = if_else(HPI - 100 > 0, "increased", "decreased"),
        this_label_text = if_else(
          is.na(HPI),
          paste0("Data is not available for ", input$years[1], " in <b>", zip, "</b>. Please choose a more recent year."),
          paste0("<p style='margin-bottom: 0.1em;'>From ", input$years[1], " to ", input$years[2], ", prices <b>", these_words, "</b><br>", 
                 " by an average of <b>", scales::percent(abs(HPI - 100), scale = 1, accuracy = 0.1), "</b>. </p>",
                 "An average home that was worth ", dollar(input$price, accuracy = 1), " in ", input$years[1], "<br>", 
                 " would be worth <b>", dollar(price, accuracy = 1), "</b> in ", input$years[2], inflation_note, ".")),
        this_label_text = if_else(zip == "40209",
                                  "This zip code is mostly the airport and fairgrounds. Housing price data is not available for 40209.",
                                  this_label_text),
        this_label_text = if_else(zip == "40225",
                                  "This zip code is GE Appliance Park. Housing price data is not available for 40225.",
                                  this_label_text),
        these_labels = paste0("<p style='margin-bottom: 0.1em;'><font size='2'><center><b>", zip, "</b></p>", this_label_text, "</center></font>") %>%
          lapply(htmltools::HTML),
        bg_weights = if_else(zip_color == "#FFFFFF", 1, 6),
        weights = if_else(zip_color == "#FFFFFF", 1, 3))
    
    
    # Organize object so most recently-clicked polygon is loaded last (on top)
    # if(length(input$zipcode) > 0) {
    #   
    #   map_obj <-
    #     bind_rows(
    #       map_obj[map_obj$zip %not_in% input$zipcode,],
    #       map_obj[map_obj$zip %in% input$zipcode,]
    #     )
    #   
    # }
    
    selected_zips <- map_obj[map_obj$zip %in% input$zipcode,]
    unselected_zips <- map_obj[map_obj$zip %not_in% input$zipcode,]

    inflation_note <- if_else(input$inflation, 
                              paste0("<br>(", input$years[1], " dollars)"),
                              "")
    
    leafletProxy("map", data = map_obj) %>%
      clearControls() %>%
      clearShapes() %>%
      
      # add shape color and white lines for all polygons
      addPolygons(fillColor = ~this_pal(price),
                  fillOpacity = 0.5,
                  opacity = 1,
                  color = "#FFFFFF",
                  weight = map_obj$bg_weights,
                  label = map_obj$these_labels,
                  labelOptions = labelOptions(opacity = 0.9),
                  layerId = map_obj$zip,
                  smoothFactor = 1.5) %>%
      
      # add colored lines for selected zipcodes
      addPolylines(data = selected_zips,
                   opacity = 1,
                   color = selected_zips$zip_color,
                   weight = selected_zips$weights,
                   smoothFactor = 1.5) %>%

      addLegend_decreasing(position = "bottomright", decreasing = T,
                values = c(range(map_obj$price, na.rm = T), NA_real_),
                pal = this_pal,
                title = paste0("Price in ", input$years[2], inflation_note),
                labFormat = labelFormat(prefix = "$"))
    
  })
  
  output$plot <- renderDygraph({
    
    zip_df <- zip_rebase()
    city_df <- city_rebase()
    
    zip_df %<>%
      filter(zip %in% input$zipcode) %>%
      transmute(geog = zip, year, price)
    
    city_df %<>%
      transmute(geog = "Louisville", year, price)
    
    this_df <- bind_rows(zip_df, city_df)
    
    this_df %<>%
      mutate(year = paste0("1/1/", year) %>% as.POSIXct(format = "%m/%d/%Y")) %>%
      pivot_wider(id_cols = "year", names_from = "geog", values_from = "price")
    
    max_value <- max(this_df[names(this_df) != "year"], na.rm = TRUE)
    
    ts <- xts::xts(x = this_df, 
                   order.by = this_df$year)
    
    output_graph <- dygraph(ts)
    #main = paste0("Housing Price Changes since ", input$years[1]))
    
    output_graph %<>%
      dySeries("Louisville", label = "Jefferson County Average", color = "#323844", strokeWidth = 3)
    
    if(length(input$zipcode) > 0) {
      for(z in 1:length(input$zipcode)) {
        output_graph %<>%
          dySeries(input$zipcode[z], 
                   color = glp_colors[(z - 1) %% length(glp_colors) + 1], # starts at 1 and goes 1 through length(glp_colors)
                   strokeWidth = 3)
      }
    }
    
    output_graph %<>% dyLegend(show = "always")
    
    inflation_note <- if_else(input$inflation, paste0(" (", input$years[1], " dollars)"), "")
    
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
             paste0("Estimated Value", inflation_note),
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
    
    if(is.null(map_click)) return()
    
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

