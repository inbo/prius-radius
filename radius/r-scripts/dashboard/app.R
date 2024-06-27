# Load library
library(shiny)
library(leaflet)
library(plotly)
library(sf)
library(tidyverse)
library(magrittr)
library(bslib)
library(bsicons)
library(htmlwidgets)
library(htmltools)
library(gtools)
library(RColorBrewer)
library(crosstalk)
library(webshot)
library(highcharter)

# Runtime settings
wd <- "~/Github/prius-radius/" 

# Data inlezen
## Spatial data
Vlaanderen_grenzen <- st_read(paste0(wd,"prius/data/spatial/flanders_wgs84.geojson"))
Provincies_grenzen <- st_read(paste0(wd, "prius/data/spatial/Provincies.geojson"))

# Ik geef alle datasets eenzelfde kolomnaam voor kolommen waarin de naam van de gebieden ('naam') en kolommen waarin de code van de gebieden ('code') staat, zodat ik deze makkelijk kan oproepen in mijn reactieve elementen
ps_hbtrl_deel <- st_read(paste0(wd, "radius/data/spatial/ps_hbtrl_deel.shp")) %>%
  rename(code = gebcode)

# Samenvatting van ps_hbtrl_deel zodat ik één rij/1 MULTIPOLYGON krijg per gebied
ps_hbtrl_wgs84 <- ps_hbtrl_deel %>%
  group_by(code, naam, gebopp_ha) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  st_transform(4326) %>%
  sf::st_cast("MULTIPOLYGON")

ps_vglrl_wgs84 <- st_read(paste0(wd,"radius/data/spatial/WGS84/ps_vglrl_wgs84.shp")) %>%
  rename(code = na2000code, naam = gebnaam)

n2khab_wgs84 <- st_read(paste0(wd,"radius/data/spatial/WGS84/n2khab_wgs84.shp"))  %>%
  #st_drop_geometry() %>%
  rename(code = type, naam = name) 

ps_nbhp_wgs84 <- st_read(paste0(wd,"radius/data/spatial/WGS84/ps_nbhp_wgs84.shp"))  %>%
  #st_drop_geometry() %>%
  rename(code = egndmty, naam = ntrbhrp)

am_patdat_wgs84 <- st_read(paste0(wd,"radius/data/spatial/WGS84/am_patdat_wgs84.shp")) %>%
  #st_drop_geometry() %>%
  rename(code = regio, naam = domennm)

list_wfs <- list("Habitatrichtlijngebieden (SBZ-H)" = ps_hbtrl_wgs84, "Vogelrichtlijngebieden (SBZ-V)" = ps_vglrl_wgs84, "Natura 2000 Habitattypes" = n2khab_wgs84, "Natuurbeheerplannen" = ps_nbhp_wgs84, "ANB patrimonium" = am_patdat_wgs84)

## Metric data
HBTRL <- ps_hbtrl_wgs84 %>%
  right_join(read.csv(paste0(wd,"radius/data/output/HBTRL_long.csv")), by = c("code" = "gebied"), keep = TRUE) 
VGLRL <- ps_vglrl_wgs84 %>%
  right_join(read.csv(paste0(wd,"radius/data/output/VGLRL_long.csv")), by = c("code" = "gebied"), keep = TRUE) 
N2KHAB <- n2khab_wgs84 %>%
  right_join(read.csv(paste0(wd,"radius/data/output/N2KHAB_long.csv")), by = c("code" = "gebied"), keep = TRUE) 
NBHP <- ps_nbhp_wgs84 %>%
  right_join(read.csv(paste0(wd,"radius/data/output/NBHP_long.csv")), by = c("code" = "gebied"), keep = TRUE) 
PATDAT <- am_patdat_wgs84 %>%
  right_join(read.csv(paste0(wd,"radius/data/output/PATDAT_long.csv")), by = c("code" = "gebied"), keep = TRUE) 

list_metrics <- list("Habitatrichtlijngebieden (SBZ-H)" = HBTRL, "Vogelrichtlijngebieden (SBZ-V)" = VGLRL, "Natura 2000 Habitattypes" = N2KHAB, "Natuurbeheerplannen" = NBHP, "ANB patrimonium" = PATDAT)

## Species data
species_list <- read_csv(paste0(wd, "radius/data/input/radius_species_list.csv")) 

occ_flanders <- read.csv(paste0(wd, "radius/data/input/gbif_occ_flanders.csv")) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = "+proj=longlat +datum=WGS84") %>%
  arrange(Soort, .locale = "en")

############################
########### APP ############
############################

# Custom CSS lay-out 
custom_css <- "
  .custom-header {
      font-size: 3.5em;
      font-weight: bold;
      color: black;
  }
  
  .custom-subheader {
      font-size: 1.5em;
      font-weight: normal;
      color: grey;
      margin-bottom: 5px;
  }
  
  .custom-valuebox-text {
      font-size: 1rem;
      font-weight: bold;
      color: grey;
      margin-top: 5px;
  }
  
  .p {
    font-size: 1rem;
    font-weight: normal;
    color: black;
  }
  
  .h1 {
    font-size: 0.7em;
    font-weight: bold;
    color: black;
    margin-bottom: 10
  }
  
  .h2 {
    font-size: 0.3em;
    font-weight: normal;
    color: black;
  }
  
  .custom-container {
    display: flex;
    flex-direction: row;
    height: 100vh;
    overflow: hidden;
  }
  
  .custom-sidebar {
    width: 320px;
    height: 100%;
    flex-shrink: 0;
    padding: 20px;
    background-color: #f8f9fa; 
    box-shadow: 0 4px 8px rgba(0,0,0,0.1);
    border-radius: 10px;
  }
  
  .main-content {
    flex: 1;
    padding-left: 10px;
    padding-right: 10px;
    margin-left: 10px;
    margin-right: 10px;
    overflow-y: auto;
  }
  
  custom-download-button {
      margin: 2px;
      border-color: #c04384;
      height: 30px; 
      padding: 5px 10px; 
    }

  .custom-header-row {
      flex-direction: column;
      height: 200px; 
      margin-bottom: 20px;
      padding-top: 20px;
      margin-left: 10px;
      margin-right: 10px;
      background-color: #f8f9fa;
      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      border-radius: 10px;
  }
  
  .custom-header2-row {
      height: 70px;
  }

   .custom-value-box {
    display: flex;
    flex-direction: column;
    text-align: left;
    height: 180px;
    box-shadow: none; 
    border: none; 
    background-color: #f8f9fa !important;
    border-radius: 0;
   }
   
   .custom-kaart-row {
    height: 600px; /* Adjust this value to the desired height */
    overflow: hidden; /* Optional: Prevents overflow */
  }
"

########### UI ############

ui <- page_navbar(
  title = "RadIUS dashboard",
  bg = "#c04384",
  inverse = TRUE,
  tags$head(tags$style(HTML(custom_css)), tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")),

  ## PAGINA 1 - GEBIEDSFICHES
  nav_panel(
    title = "Gebiedsfiches",
    div(class = "custom-container",
        div(class = "custom-sidebar",
            p("eventueel buttons toevoegen om alleen planten, dieren of unielijstsoorten te selecteren?"),
            selectInput("kaart2", label = "Kaarttype:", choices = names(list_metrics)),
            selectInput("gebied2", label = "Gebied:", choices = c("All"))),
        
        div(class = "main-content",
            # fluidRow(
            #   class = "custom-header-row",
            #   div(textOutput("kaartnaam"), class = "custom-header")
            # ),
            fluidRow(
              h1(textOutput("gebiedsnaam"))
            ),
            fluidRow(
              layout_column_wrap(
                column(
                  width = 12,
                  plotlyOutput("in_gebied")
                ),
                column(
                  width = 12,
                  plotlyOutput("of_gebied")
                ))
              ),
            fluidRow(
              layout_column_wrap(
                column(
                  width = 12,
                  plotlyOutput("in_vs_of")
                ),
                column(
                  width = 12
                )
              )
            )
        )
    )
  ),
  
  ## PAGINA 2 - SOORTENFICHES
  nav_panel(
    title = "Soortenfiches",
    div(class = "custom-container",
        div(class = "custom-sidebar",
            p("Selecteer de", strong("soort"), "en", strong("kaart"), " waarin je geïnteresseerd bent."),
            selectizeInput(inputId = "soort", label = "Soort:", 
                           choices = sort(unique(occ_flanders$Soort)), selected = 1),
            selectInput('kaart', label = 'Kaarttype:', choices = names(list_metrics))
        ),
        div(class = "main-content",
            fluidRow(
              class = "custom-header-row",
              column(
                width = 9,
                div(textOutput("soort"), class = "custom-header"),
                div(textOutput("species"), class = "custom-subheader"),
                div(uiOutput("union_list"))
              ),
              column(
                width = 3,
                value_box(
                  class = "custom-value-box",
                  color = "#f8f9fa",
                  title = "",
                  value = div(textOutput("tot_obs"), class = "custom-valuebox-text"),
                  showcase = plotlyOutput("aantal"),
                  showcase_layout = "bottom",
                  div("WAARNEMING(EN)", class = "custom-valuebox-text"),
                  div("SINDS 2015", class = "custom-valuebox-text"),
                  full_screen = TRUE
                )
              )
            ),
            # fluidRow(
            #   class = "custom-header2-row",
            #   h1(textOutput("kaarttitel")),
            # ),
            navset_card_tab(
              nav_panel(
                title = "Aandeel in",
                fluidRow(
                  column(
                    width = 3,
                    highchartOutput("piechart_in"),
                    full_screen = TRUE
                  ),
                  column(
                    width = 9,
                    highchartOutput("barchart_in")
                  )
                ),
                fluidRow(
                  class = "custom-kaart-row", 
                  leafletOutput("kaart_in", height = "600px"),
                  div(downloadButton("download_kaart", "Download PNG", class = "custom-download-button"))
                  )
                ),
              nav_panel(
                title = "Aandeel van",
                fluidRow(
                  highchartOutput("barchart_of"),
                  full_screen = TRUE
                ),
                fluidRow(
                  class = "custom-kaart-row", 
                  leafletOutput("kaart_of", height = "600px")
                ))
            )
        )
    )
  ),
  
  ## PAGINA 3 - OVER DASHBOARD
  nav_panel(
    title = "Over dashboard",
    p("Third page content.")
  ),
  nav_spacer()
)


########### SERVER ############
server <- function(input, output, session) {
  
  # PAGINA 1 - GEBIEDSFICHES
  
  ## REACTIEVE ELEMENTEN
  observeEvent(input$kaart2, {
    freezeReactiveValue(input, "gebied2")
    updateSelectInput(inputId = "gebied2", choice = c("All", unique(na.omit(list_metrics[[input$kaart2]]$code))))
  })
  
  metrics_wide <- reactive({
    list_metrics[[input$kaart2]] %>%
      filter(is.na(code) & type %in% c("in", "of")) %>%
      mutate(type = paste(type, gebied, sep = "")) %>%
      pivot_wider(names_from = type, values_from = overlap)
  })
  
  metrics_in_gebied <- reactive({
    list_metrics[[input$kaart2]] %>%
      filter(is.na(code) & type == "in")
  })
  
  metrics_of_gebied <- reactive({
    list_metrics[[input$kaart2]] %>%
      filter(is.na(code) & type == "of")
  })
  
  ## OUTPUTS
  output$gebiedsnaam <- renderText({
    input$kaart2
  })
  
  output$in_gebied <- renderPlotly({
    ggplotly(
      ggplot() +
        geom_bar(data = metrics_in_gebied(), aes(x = reorder(soort, overlap), y = overlap, text = paste0(soort, " (", round(overlap * 100, 1), "%)")), stat = "identity", colour = "white", linewidth = 0.01, fill = "#c04384") +
        #geom_hline(yintercept = 0.07705856, color = "red") + # proportie van totaal opp Vlaanderen dat ingenomen wordt door HBTRL
        xlab("Soort") + ylab("% overlap") +
        theme_bw() +
        theme(axis.text = element_text(size = 9),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              axis.title = element_text(size = 9)), tooltip = "text")
  })
  
  output$of_gebied <- renderPlotly({
    ggplotly(
      ggplot() +
        geom_bar(data = metrics_of_gebied(), aes(x = reorder(soort, overlap), y = overlap, text = paste0(soort, " (", round(overlap * 100, 1), "%)")), stat = "identity", colour = "white", linewidth = 0.01, fill = "#c04384") +
        #geom_hline(yintercept = 0.07705856, color = "red") + # proportie van totaal opp Vlaanderen dat ingenomen wordt door HBTRL
        xlab("Soort") + ylab("% overlap") +
        theme_bw() +
        theme(axis.text = element_text(size = 9),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              axis.title = element_text(size = 9)), tooltip = "text")
  })
  
  output$in_vs_of <- renderPlotly({
    ggplotly(
      ggplot(data = metrics_wide(), aes(x = metrics_wide()[[9]], y = metrics_wide()[[10]], label = abbr)) +
        geom_text() +
        theme(legend.position="none") +
        xlim(0,1) +
        xlab("Aandeel totale verspreiding dat overlapt met habitatrichtlijngebied") + ylab("aandeel totaal oppervlak habitatrichtlijngebied dat bezet is") +
        theme_bw()
    )
        
  })
  
  # output$in-vs-of <- renderPlotly({
  #   ggplotly(
  #     ggplot() +
  #       geom_bar(data = metrics_of_gebied(), aes(x = reorder(soort, overlap), y = overlap, text = paste0(soort, " (", round(overlap * 100, 1), "%)")), stat = "identity", colour = "white", linewidth = 0.01, fill = "#c04384") +
  #       #geom_hline(yintercept = 0.07705856, color = "red") + # proportie van totaal opp Vlaanderen dat ingenomen wordt door HBTRL
  #       xlab("Soort") + ylab("% overlap") +
  #       theme_bw() +
  #       theme(axis.text = element_text(size = 9),
  #             axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
  #             axis.title = element_text(size = 9)), tooltip = "text")
  # })
  
  
  # PAGINA 2 - SOORTENFICHES
  
  ## REACTIVE ELEMENTS
  
  ### Filter soortgegevens obv input soort
  occ_species <- reactive({
    occ_flanders %>%
      filter(Soort == input$soort) 
  })
  
  ### Filter metrics data obv input soort en kaarttype
  metrics <- reactive({
    list_metrics[[input$kaart]] %>%
      filter(soort == input$soort)
  })
  
  ### Overzicht waarnemingen per jaar
  occ_sum <- reactive({
    occ_species() %>%
      st_drop_geometry() %>%
      count(year, name = "n") %>%
      right_join(data.frame(year = 2015:2024), by = "year") %>%
      arrange(year) %>%
      na.replace(0)
  })
  
  ### Subset metrics in
  metrics_in <- reactive({
    metrics() %>%
      filter(type == "in" & !is.na(code)) 
  })
  
  # metrics_in_sd <- reactive({
  #   SharedData$new(metrics_in(), key=~gebied)
  # })
  
  ### Subset metrics of
  metrics_of <- reactive({
    metrics() %>%
      filter(type == "of" & !is.na(code))
  })
  
  ### Color palette in
  pal_in <- reactive({
    colors <- c("lightgrey", colorRampPalette(c("lightgrey", "#c04384"))(99))
    
    if (nrow(metrics_in()) != 0) {
      colorNumeric(
        palette = colors,
        domain = c(min(metrics_in()$overlap), max(metrics_in()$overlap))
      )
    }
  })
  
  ### Color palette of
  pal_of <- reactive({
    colors <- c("lightgrey", colorRampPalette(c("lightgrey", "#c04384"))(99))
    
    if (nrow(metrics_of()) != 0) {
      colorNumeric(
        palette = colors,
        domain = c(min(metrics_of()$overlap), max(metrics_of()$overlap))
      )
    }
  })
  
  ## OUTPUTS
  
  ### Soortnaam
  output$soort <- renderText({
    input$soort
  })
  
  ### Wet. soortnaam
  output$species <- renderText({
    unique_species <- occ_flanders %>%
      filter(Soort == input$soort) %>%
      distinct(Species) %>%
      pull(Species)
    
    paste(unique_species, collapse = ", ")
  })
  
  ### Staat de soort op de unielijst? Zo ja, welke uitvoeringsverordening?
  output$union_list <- renderUI({
    eu <- species_list %>%
      filter(Soort == input$soort) %>%
      pull(EU_lijst) %>%
      unique()
    
    if (eu == 1) {
      HTML('<span style="color: #c04384; font-weight: bold;"><i class="fas fa-bell"></i> Op unielijst (uitvoeringsverordening 2016/1141)</span>')
    } else if (eu == 2) {
      HTML('<span style="color: #c04384; font-weight: bold;"><i class="fas fa-bell"></i> Op unielijst (uitvoeringsverordening 2017/1263)</span>')
    } else if (eu == 3) {
      HTML('<span style="color: #c04384; font-weight: bold;"><i class="fas fa-bell"></i> Op unielijst (uitvoeringsverordening 2019/1262)</span>')
    } else if (eu == 4) {
      HTML('<span style="color: #c04384; font-weight: bold;"><i class="fas fa-bell"></i> Op unielijst (uitvoeringsverordening 2022/1203)</span>')
    } else if (eu == 0) {
      HTML("")
    }
  })
  
  ### Totaal aantal waarnemingen
  output$tot_obs <- renderText({
    format(nrow(occ_species()), big.mark = ".")
  })
  
  ### Barchart waarnemingen per jaar
  output$aantal <- renderPlotly({
    plot_ly(occ_sum()) %>%
      add_lines(
        x = ~as.factor(year), y = ~n,
        color = I("grey"), span = I(1),
        fill = 'tozeroy', alpha = 0.2,
        text = ~paste("Jaar: ", as.factor(year), "<br>Waarnemingen: ", n),
        hoverinfo = 'text'
      ) %>%
      layout(
        xaxis = list(visible = F, showgrid = F, title = ""),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "black"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      
      config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
          var ro = new ResizeObserver(function() {
            var visible = el.offsetHeight > 100;
            Plotly.relayout(el, {'xaxis.visible': visible});
          });
          ro.observe(el);
        }"
      )
  })
  
  ### Tussentitel
  output$kaartnaam <- renderText({
    paste(input$kaart)
  })
  
  output$piechart_in <- renderHighchart({
    df <- data.frame(
      label = c("IN", "NIET IN"),
      gebied = c(paste("in", input$kaart), paste("niet in", input$kaart)),
      overlap = c(sum(metrics_in()$overlap), 1 - sum(metrics_in()$overlap)),
      color = c("#c04384", "lightgrey"))
    
    # Preparing the data in the required format
    data_list <- df %>%
      mutate(y = overlap * 100, name = gebied) %>%
      select(name, y, label, color) %>%
      list_parse()
    
    highchart() %>%
      hc_chart(type = "pie", width = NULL) %>%
      hc_plotOptions(pie = list(
        innerSize = '60%',
        dataLabels = list(
          enabled = TRUE,
          distance = -30, 
          format = '<b>{point.label}</b> ',
          style = list(color = 'black', fontSize = '14px', fontWeight = 'bold'),
          backgroundColor = NULL,
          borderColor = NULL,      
          borderWidth = 0      
        ),
        showInLegend = FALSE,
        borderColor = "black",
        borderWidth = 0.5
      )) %>%
      hc_add_series(
        name = "Overlap",
        data = data_list, 
        tooltip = list(
          pointFormat = '<b>{point.y:.1f}</b> %', 
          style = list(color = "black", fontsize = '14px', fontWeight = 'bold')
        )
      ) %>%
      #hc_tooltip(pointFormat = '<b>{point.gebied}</b>') %>%
      hc_legend(enabled = FALSE) %>%
      hc_size(height = NULL) %>%
      hc_chart(backgroundColor = 'rgba(0, 0, 0, 0)') %>%
      hc_add_theme(hc_theme_elementary())
  })
  
  
  output$barchart_in <- renderHighchart({
    if (nrow(metrics_in()) != 0) {
      df <- metrics_in() %>%
        filter(overlap != 0) %>%
        arrange(overlap) %>%
        mutate(gebied = factor(gebied, levels = gebied)) %>%
        mutate(y = overlap * 100)
      
      # data_list <- df %>%
      #   select(gebied, y, naam) %>%
      #   list_parse()
      
      highchart() %>%
        hc_chart(type = 'column') %>%
        hc_xAxis(categories = df$gebied, title = list(text = ""), labels = list(rotation = -90)) %>%
        hc_yAxis(title = list(text = 'Overlap (%)'), labels = list(format = '{value}%')) %>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = TRUE, format = '{point.y:.2f}%'),
          borderColor = "black",
          borderWidth = 0.5,
          pointPadding = 0.1, 
          groupPadding = 0.1
        )) %>%
        hc_add_series(
          name = "Overlap",
          data = df %>% mutate(y = overlap * 100) %>% select(gebied, y, naam),
          colorByPoint = TRUE,
          colors = pal_in()(df$overlap)
        ) %>%
        hc_tooltip(
          headerFormat = '',
          pointFormat = '<b>{point.naam} ({point.gebied}): {point.y:.2f}%</b>'
        ) %>%
        hc_chart(backgroundColor = 'rgba(0, 0, 0, 0)') %>%
        hc_add_theme(hc_theme_elementary()) 
    }
  })
  
  ### Kaart in
  kaart_in <- reactive({
    map <- leaflet() %>%
      addTiles(urlTemplate = "", attribution = NULL, group = "Zonder achtergrond") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addPolygons(data = Vlaanderen_grenzen, color = "black", fillColor = "#f0f0f0", weight = 1, group = "Vlaanderen") %>%
      addPolygons(data = Provincies_grenzen, color = "black", fillColor = "#f0f0f0", weight = 0.5, group = "Provincies") %>%
      addPolygons(data = list_wfs[[input$kaart]], color = "black", fillColor = "lightgrey", opacity = 0.7, weight = 0.5, fillOpacity = 1, label = ~paste0(naam, " (", code, "): 0%"), highlight = highlightOptions(stroke = TRUE, color = "black", weight = 2)) 
      
      if(nrow(metrics_in()) != 0) {
        map <- map %>%
          addPolygons(data = metrics_in(), color = "black", fillColor = ~pal_in()(overlap), opacity = 1, weight = 0.5, fillOpacity = 1, label = ~paste0(naam, " (", gebied, "): ", round(overlap * 100, 1), "%"), highlight = highlightOptions(stroke = TRUE, color = "black", weight = 2))
      }
    
    map <- map %>%
      addCircleMarkers(data = occ_species(), radius = 3, color = "blue", fillOpacity = 0.7, weight = 0.5, label = ~paste0(day, "/", month, "/", year), group = "Waarnemingen") %>%
      addLayersControl(
        baseGroups = c("Geen achtergrond", "OSM (default)", "Satellite"),
        overlayGroups = c("Waarnemingen"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Waarnemingen") %>%
      setView(lng = 4.240556, lat = 51.037778, zoom = 9)
    
    if(is.data.frame(metrics_in()) && nrow(metrics_in()) != 0) {
      map <- map %>%
        addLegend(pal = pal_in(), values = metrics_in()$overlap, opacity = 0.8, position = "bottomright")
    }
    
    map
  })
  
  output$kaart_in <- renderLeaflet({
      kaart_in()
  })
  
  
  output$download_kaart <- downloadHandler(
    filename = function()
      nameFile(soort = input$soort,
               type = "in",
               content = input$kaart, fileExt = "png"),
    content = function(file) {

      tmpFile <- tempfile(fileext = ".html")

      # write map to temp .html file
      htmlwidgets::saveWidget(kaart_in(), file = tmpFile, selfcontained = FALSE)

      # convert temp .html file into .png for download
      webshot::webshot(url = tmpFile, file = file,
                       vwidth = 1000, vheight = 500, cliprect = "viewport")

    }
  )
  
  ### Piechart of
  output$piechart_of <- renderPlotly({
    df <- data.frame(
      gebied = c(metrics_of()$gebied, "Buiten"),
      overlap = c(metrics_of()$overlap, 1 - sum(metrics_of()$overlap))
    )
    
    plot_ly(df, labels = ~gebied, values = ~overlap, type = 'pie', 
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            text = ~paste('Gebied:', gebied, '<br>Overlap:', overlap),
            hoverinfo = 'text',
            marker = list(colors = ~pal()(df$overlap),
                          line = list(color = '#FFFFFF', width = 1)),
            showlegend = FALSE) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  ### Barchart of
  output$barchart_of <- renderHighchart({
    if (nrow(metrics_of()) != 0) {
      df <- metrics_of() %>%
        filter(overlap != 0) %>%
        arrange(overlap) %>%
        mutate(gebied = factor(gebied, levels = gebied)) %>%
        mutate(y = overlap * 100)
      
      highchart() %>%
        hc_chart(type = 'column') %>%
        hc_xAxis(categories = df$gebied, title = list(text = ""), labels = list(rotation = -90)) %>%
        hc_yAxis(title = list(text = 'Overlap (%)'), labels = list(format = '{value}%')) %>%
        hc_plotOptions(column = list(
          dataLabels = list(enabled = TRUE, format = '{point.y:.2f}%'),
          borderColor = "black",
          borderWidth = 0.5,
          pointPadding = 0.1, 
          groupPadding = 0.1
        )) %>%
        hc_add_series(
          name = "Overlap",
          data = df %>% mutate(y = overlap * 100) %>% select(gebied, y, naam),
          colorByPoint = TRUE,
          colors = pal_of()(df$overlap)
        ) %>%
        hc_tooltip(
          headerFormat = '',
          pointFormat = '<b>{point.naam} ({point.gebied}): {point.y:.2f}%</b>'
        ) %>%
        hc_chart(backgroundColor = 'rgba(0, 0, 0, 0)') %>%
        hc_add_theme(hc_theme_elementary()) 
    }
  })
  
  ### Kaart of
  output$kaart_of <- renderLeaflet({
    map <- leaflet() %>%
      addTiles(urlTemplate = "", attribution = NULL, group = "Zonder achtergrond") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addPolygons(data = Vlaanderen_grenzen, color = "black", fillColor = "#f0f0f0", weight = 1, group = "Vlaanderen") %>%
      addPolygons(data = Provincies_grenzen, color = "black", fillColor = "#f0f0f0", weight = 0.5, group = "Provincies") %>%
      addPolygons(data = list_wfs[[input$kaart]], color = "black", fillColor = "lightgrey", opacity = 0.7, weight = 0.5, fillOpacity = 1, label = ~paste0(naam, " (", code, "): 0%"), highlight = highlightOptions(stroke = TRUE, color = "black", weight = 2)) 
    
    if(nrow(metrics_of()) != 0) {
      map <- map %>%
        addPolygons(data = metrics_of(), color = "black", fillColor = ~pal_of()(overlap), opacity = 1, weight = 0.5, fillOpacity = 1, label = ~paste0(naam, " (", gebied, "): ", round(overlap * 100, 1), "%"), highlight = highlightOptions(stroke = TRUE, color = "black", weight = 2))
    }
    
    map <- map %>%
      addCircleMarkers(data = occ_species(), radius = 3, color = "blue", fillOpacity = 0.7, weight = 0.5, label = ~paste0(day, "/", month, "/", year), group = "Waarnemingen") %>%
      addLayersControl(
        baseGroups = c("Geen achtergrond", "OSM (default)", "Satellite"),
        overlayGroups = c("Waarnemingen"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Waarnemingen") %>%
      setView(lng = 4.240556, lat = 51.037778, zoom = 9)
    
    if(is.data.frame(metrics_of()) && nrow(metrics_of()) != 0) {
      map <- map %>%
        addLegend(pal = pal_of(), values = metrics_of()$overlap, opacity = 0.8, position = "bottomright")
    }
    
    map
  })
  
}

shinyApp(ui = ui, server = server)