# ──────────────────────────────────────────────────────────────
# AWV IUS-verkenner
# Invasieve uitheemse planten langs autosnelwegen in Vlaanderen
# ──────────────────────────────────────────────────────────────


# ─────────────────────────────────────────────
# 1. Packages
# ─────────────────────────────────────────────

library(shiny)
library(bslib)
library(bsicons)
library(leaflet)
library(highcharter)
library(sf)
library(tidyverse)
library(here)
library(htmltools)
library(scales)


# ─────────────────────────────────────────────
# 2. Instellingen
# ─────────────────────────────────────────────

buffer_autosnelweg <- 25

# Kleuren
kleur_navbar <- "#343A40"
kleur_awv <- "#F2C94C"
kleur_awv_donker <- "#D99A00"
kleur_awv_licht <- "#FFF3BF"
kleur_grijs <- "#9AA0A6"
kleur_lichtgrijs <- "#F4F5F6"
kleur_donkergrijs <- "#4F565C"

status_kleuren <- c(
  "Reeds in AWV-visie" = kleur_awv_donker,
  "Vermeld in actualisatievraag" = kleur_awv,
  "Overige IUS" = kleur_grijs
)


# ─────────────────────────────────────────────
# 3. Data inlezen
# ─────────────────────────────────────────────

data_dir <- "output"

required_files <- c(
  "awv_autosnelwegen_metrics.csv",
  "awv_district_metrics.csv",
  "awv_hex_totaal.rds",
  "awv_hex_soort.rds",
  "awv_wegsegmenten.rds",
  "awv_autosnelwegen_buffer.rds",
  "awv_provincies.rds",
  "awv_occ_planten.rds"
)

missing_files <- required_files[
  !file.exists(
    file.path(
      data_dir,
      required_files
    )
  )
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "De volgende bestanden ontbreken in ",
      data_dir,
      ":\n",
      paste(
        missing_files,
        collapse = "\n"
      )
    )
  )
}


# Tabellen
autosnelwegen_metrics <- read_csv(
  file.path(
    data_dir,
    "awv_autosnelwegen_metrics.csv"
  ),
  show_col_types = FALSE
)

district_metrics <- read_csv(
  file.path(
    data_dir,
    "awv_district_metrics.csv"
  ),
  show_col_types = FALSE
)


# Ruimtelijke bestanden
hex_totaal_leaflet <- readRDS(
  file.path(
    data_dir,
    "awv_hex_totaal.rds"
  )
)

hex_soort_leaflet <- readRDS(
  file.path(
    data_dir,
    "awv_hex_soort.rds"
  )
)

wegsegmenten_leaflet <- readRDS(
  file.path(
    data_dir,
    "awv_wegsegmenten.rds"
  )
)

autosnelwegen_buffer_leaflet <- readRDS(
  file.path(
    data_dir,
    "awv_autosnelwegen_buffer.rds"
  )
)

Provincies_grenzen_leaflet <- readRDS(
  file.path(
    data_dir,
    "awv_provincies.rds"
  )
)

# Puntwaarnemingen voor optionele weergave op de soortenkaart
occ_planten_leaflet <- readRDS(
  file.path(
    data_dir,
    "awv_occ_planten.rds"
  )
)


# ─────────────────────────────────────────────
# 4. Data voorbereiden
# ─────────────────────────────────────────────

# Oude en nieuwe benamingen voor AWV-status harmoniseren
autosnelwegen_metrics <- autosnelwegen_metrics %>%
  mutate(
    awv_status = case_when(
      awv_status %in% c(
        "Bestaande AWV-focus",
        "Reeds in AWV-visie"
      ) ~ "Reeds in AWV-visie",
      
      awv_status %in% c(
        "Mogelijke uitbreiding",
        "Vermeld in actualisatievraag"
      ) ~ "Vermeld in actualisatievraag",
      
      TRUE ~ "Overige IUS"
    )
  )


# Alleen soorten met overlap met de autosnelwegcorridor
autosnelwegen_metrics_dashboard <- autosnelwegen_metrics %>%
  filter(
    `of` > 0
  ) %>%
  arrange(
    desc(`of`)
  )


# Basisinformatie per soort
species_meta <- autosnelwegen_metrics_dashboard %>%
  select(
    Soort,
    Species,
    awv_status,
    `of`
  ) %>%
  distinct(
    Soort,
    .keep_all = TRUE
  )


# Soortinformatie toevoegen aan districtresultaten
district_metrics <- district_metrics %>%
  left_join(
    species_meta,
    by = "Soort"
  )


# Namen van districten opschonen
wegsegmenten_leaflet <- wegsegmenten_leaflet %>%
  mutate(
    labelWegbeheerder = str_squish(
      labelWegbeheerder
    )
  )


# Districtpolygonen maken voor visualisatie
district_buffers_leaflet <- wegsegmenten_leaflet %>%
  filter(
    !is.na(labelWegbeheerder),
    labelWegbeheerder != ""
  ) %>%
  st_transform(
    31370
  ) %>%
  select(
    labelWegbeheerder
  ) %>%
  st_buffer(
    dist = buffer_autosnelweg
  ) %>%
  group_by(
    labelWegbeheerder
  ) %>%
  summarise(
    .groups = "drop"
  ) %>%
  st_make_valid() %>%
  st_transform(
    4326
  )


# Selectiemogelijkheden
plantsoorten <- autosnelwegen_metrics_dashboard %>%
  pull(
    Soort
  ) %>%
  unique() %>%
  sort()

districten <- district_metrics %>%
  pull(
    labelWegbeheerder
  ) %>%
  na.omit() %>%
  unique() %>%
  sort()


# Standaard geselecteerde soort:
# soort met hoogste globale overlap
standaard_soort <- autosnelwegen_metrics_dashboard %>%
  slice_max(
    order_by = `of`,
    n = 1,
    with_ties = FALSE
  ) %>%
  pull(
    Soort
  )


# Bounding box Vlaanderen
bbox_vlaanderen <- st_bbox(
  Provincies_grenzen_leaflet
)


# ─────────────────────────────────────────────
# 5. Helpers
# ─────────────────────────────────────────────

# Exportopties toevoegen aan Highcharts
add_exporting <- function(chart) {
  
  chart %>%
    hc_exporting(
      enabled = TRUE,
      fallbackToExportServer = FALSE
    ) %>%
    hc_add_dependency(
      "modules/exporting.js"
    ) %>%
    hc_add_dependency(
      "modules/offline-exporting.js"
    ) %>%
    hc_add_dependency(
      "modules/export-data.js"
    )
}


# Basisthema Highcharts
hc_aw_vormgeving <- function(chart) {
  
  chart %>%
    hc_chart(
      backgroundColor = "rgba(0,0,0,0)"
    ) %>%
    hc_legend(
      itemStyle = list(
        fontFamily = "Arial",
        fontSize = "11px"
      )
    ) %>%
    hc_tooltip(
      style = list(
        fontFamily = "Arial",
        fontSize = "12px"
      )
    )
}


# KPI-kaart
kpi_card <- function(
    titel,
    output_id,
    subtitel = NULL
) {
  
  div(
    class = "kpi-card",
    
    div(
      titel,
      class = "kpi-title"
    ),
    
    div(
      textOutput(
        output_id,
        inline = TRUE
      ),
      class = "kpi-value"
    ),
    
    if (!is.null(subtitel)) {
      div(
        subtitel,
        class = "kpi-subtitle"
      )
    }
  )
}


# ─────────────────────────────────────────────
# 6. CSS
# ─────────────────────────────────────────────

custom_css <- paste0(
  "

  body {
    font-family: Arial, sans-serif;
    background-color: #F7F7F5;
    color: #2F3438;
  }

  .navbar {
    background-color: ", kleur_navbar, " !important;
  }

  .navbar-brand {
    font-weight: 600;
    font-size: 17px;
  }

  .navbar .nav-link {
    color: rgba(255,255,255,0.82) !important;
    font-size: 13px;
  }

  .navbar .nav-link:hover {
    color: white !important;
  }

  .navbar .nav-link.active {
    color: ", kleur_awv, " !important;
    font-weight: 600;
  }

  .dashboard-header {
    margin: 5px 0 18px 0;
  }

  .dashboard-title {
    font-size: 23px;
    font-weight: 600;
    color: #2F3438;
    margin-bottom: 3px;
  }

  .dashboard-subtitle {
    font-size: 13px;
    color: #777;
    margin-bottom: 0;
  }

  .dashboard-card {
    background: white;
    border: 1px solid #E4E6E8;
    border-radius: 6px;
    padding: 16px;
    margin-bottom: 15px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.04);
  }

  .card-title-aw {
    font-size: 15px;
    font-weight: 600;
    color: #343A40;
    margin-bottom: 3px;
  }

  .card-subtitle-aw {
    font-size: 11px;
    color: #777;
    margin-bottom: 12px;
  }

  .kpi-card {
    background: white;
    border-top: 4px solid ", kleur_awv, ";
    border-left: 1px solid #E4E6E8;
    border-right: 1px solid #E4E6E8;
    border-bottom: 1px solid #E4E6E8;
    border-radius: 6px;
    padding: 15px 18px;
    margin-bottom: 15px;
    min-height: 105px;
  }

  .kpi-title {
    font-size: 11px;
    color: #777;
    text-transform: uppercase;
    letter-spacing: 0.03em;
    margin-bottom: 5px;
  }

  .kpi-value {
    font-size: 27px;
    font-weight: 600;
    color: #343A40;
  }

  .kpi-subtitle {
    margin-top: 2px;
    color: #777;
    font-size: 11px;
  }

  .species-header {
    background: white;
    border-left: 5px solid ", kleur_awv, ";
    border-top: 1px solid #E4E6E8;
    border-right: 1px solid #E4E6E8;
    border-bottom: 1px solid #E4E6E8;
    border-radius: 6px;
    padding: 15px 18px;
    margin-bottom: 15px;
  }

  .species-name {
    font-size: 20px;
    font-weight: 600;
    color: #343A40;
  }

  .species-scientific {
    font-size: 12px;
    font-style: italic;
    color: #777;
    margin-top: 2px;
  }

  .status-badge {
    display: inline-block;
    padding: 4px 8px;
    margin-top: 8px;
    border-radius: 4px;
    font-size: 10px;
    font-weight: 600;
  }

  .sidebar {
    font-size: 12px;
  }

  .control-label {
    font-size: 12px;
    font-weight: 600;
  }

  .selectize-input {
    font-size: 12px;
  }

  .selectize-dropdown {
    font-size: 11px;
  }

  .analysis-note {
    background: #FFF9E6;
    border-left: 4px solid ", kleur_awv, ";
    padding: 10px 14px;
    margin: 0 0 15px 0;
    font-size: 11px;
    color: #555;
  }

  .about-section {
    max-width: 900px;
    background: white;
    border: 1px solid #E4E6E8;
    border-radius: 6px;
    padding: 20px 24px;
    margin-bottom: 15px;
  }

  .about-section h3 {
    font-size: 15px;
    font-weight: 600;
    margin-top: 0;
    color: #343A40;
  }

  .about-section p,
  .about-section li {
    font-size: 12px;
    line-height: 1.5;
  }

  .table {
    font-size: 11px;
  }

  "
)


# ─────────────────────────────────────────────
# 7. UI
# ─────────────────────────────────────────────

ui <- page_navbar(
  
  title = "AWV IUS-verkenner",
  
  bg = kleur_navbar,
  
  inverse = TRUE,
  
  theme = bs_theme(
    version = 5
  ),
  
  header = tags$head(
    tags$style(
      HTML(
        custom_css
      )
    )
  ),
  
  
  # ───────────────────────────────────────────
  # OVERZICHT
  # ───────────────────────────────────────────
  
  nav_panel(
    
    "Overzicht",
    
    div(
      class = "dashboard-header",
      
      div(
        "Invasieve uitheemse planten langs autosnelwegen in Vlaanderen",
        class = "dashboard-title"
      ),
      
      div(
        "Verkennende screening voor Agentschap Wegen en Verkeer",
        class = "dashboard-subtitle"
      )
    ),
    
    
    div(
      class = "analysis-note",
      
      HTML(
        paste0(
          "<strong>Interpretatie:</strong> ",
          "de resultaten geven een indicatie van het geschatte voorkomen ",
          "langs de autosnelwegcorridor. Ze vormen geen volledige ",
          "beheerprioritering."
        )
      )
    ),
    
    
    fluidRow(
      
      column(
        
        width = 5,
        
        div(
          class = "dashboard-card",
          
          div(
            "Welke soorten komen het sterkst voor?",
            class = "card-title-aw"
          ),
          
          div(
            paste0(
              "Aandeel van de totale autosnelwegcorridor ",
              "met geschat voorkomen."
            ),
            class = "card-subtitle-aw"
          ),
          
          highchartOutput(
            "global_bar",
            height = "650px"
          )
        )
      ),
      
      
      column(
        
        width = 7,
        
        div(
          class = "dashboard-card",
          
          div(
            "Waar liggen ruimtelijke concentraties?",
            class = "card-title-aw"
          ),
          
          div(
            paste0(
              "Aantal invasieve uitheemse plantensoorten ",
              "per hexagoon."
            ),
            class = "card-subtitle-aw"
          ),
          
          leafletOutput(
            "overview_map",
            height = "650px"
          )
        )
      )
    ),
    
    
    fluidRow(
      
      column(
        
        width = 12,
        
        div(
          class = "dashboard-card",
          
          div(
            "Vergelijking tussen districten",
            class = "card-title-aw"
          ),
          
          div(
            paste0(
              "Geschat voorkomen van alle plantensoorten ",
              "per AWV-district."
            ),
            class = "card-subtitle-aw"
          ),
          
          highchartOutput(
            "district_heatmap",
            height = "700px"
          )
        )
      )
    )
  ),
  
  
  # ───────────────────────────────────────────
  # SOORTEN
  # ───────────────────────────────────────────
  
  nav_panel(
    
    "Soorten",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        width = 280,
        
        selectizeInput(
          inputId = "soort",
          label = "Soort:",
          choices = plantsoorten,
          selected = standaard_soort
        ),
        
        selectizeInput(
          inputId = "soort_district",
          label = "District:",
          choices = c(
            "Heel Vlaanderen",
            districten
          ),
          selected = "Heel Vlaanderen"
        )
      ),
      
      
      div(
        
        uiOutput(
          "species_header"
        ),
        
        
        fluidRow(
          
          column(
            width = 4,
            
            kpi_card(
              "Geschat voorkomen",
              "species_of",
              "Aandeel van de totale autosnelwegcorridor"
            )
          ),
          
          column(
            width = 8,
            
            div(
              class = "analysis-note",
              
              paste0(
                "De soortkaart toont per hexagoon welk aandeel van de ",
                "autosnelwegcorridor overlapt met het geschatte ",
                "voorkomen van de geselecteerde soort."
              )
            )
          )
        ),
        
        
        fluidRow(
          
          column(
            
            width = 7,
            
            div(
              class = "dashboard-card",
              
              div(
                "Ruimtelijke verspreiding",
                class = "card-title-aw"
              ),
              
              leafletOutput(
                "species_map",
                height = "570px"
              )
            )
          ),
          
          
          column(
            
            width = 5,
            
            conditionalPanel(
              condition = "input.soort_district == 'Heel Vlaanderen'",
              
              div(
                class = "dashboard-card",
                
                div(
                  "Voorkomen per AWV-district",
                  class = "card-title-aw"
                ),
                
                div(
                  "Aandeel van de corridor binnen ieder district.",
                  class = "card-subtitle-aw"
                ),
                
                highchartOutput(
                  "species_district_bar",
                  height = "570px"
                )
              )
            )
          )
        )
      )
    )
  ),
  
  
  # ───────────────────────────────────────────
  # DISTRICTEN
  # ───────────────────────────────────────────
  
  nav_panel(
    
    "Districten",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        width = 280,
        
        selectizeInput(
          inputId = "district",
          label = "AWV-district:",
          choices = districten,
          selected = districten[1]
        )
      ),
      
      
      div(
        
        div(
          class = "species-header",
          
          div(
            textOutput(
              "district_name"
            ),
            class = "species-name"
          ),
          
          div(
            "Geschat voorkomen van invasieve uitheemse planten",
            class = "species-scientific"
          )
        ),
        
        
        fluidRow(
          
          column(
            
            width = 5,
            
            div(
              class = "dashboard-card",
              
              div(
                "Welke soorten komen hier het sterkst voor?",
                class = "card-title-aw"
              ),
              
              div(
                paste0(
                  "Aandeel van de autosnelwegcorridor ",
                  "binnen het geselecteerde district."
                ),
                class = "card-subtitle-aw"
              ),
              
              highchartOutput(
                "district_species_bar",
                height = "600px"
              )
            )
          ),
          
          
          column(
            
            width = 7,
            
            div(
              class = "dashboard-card",
              
              div(
                "Ruimtelijk overzicht",
                class = "card-title-aw"
              ),
              
              div(
                "Aantal IUS-planten langs de autosnelwegen in het district.",
                class = "card-subtitle-aw"
              ),
              
              leafletOutput(
                "district_map",
                height = "600px"
              )
            )
          )
        )
        
        
      )
    )
  ),
  
  
  # ───────────────────────────────────────────
  # OVER
  # ───────────────────────────────────────────
  
  nav_panel(
    
    "Over",
    
    div(
      class = "dashboard-header",
      
      div(
        "AWV IUS-verkenner",
        class = "dashboard-title"
      ),
      
      div(
        "Verkennende toepassing voor invasieve uitheemse planten",
        class = "dashboard-subtitle"
      )
    ),
    
    
    div(
      class = "about-section",
      
      h3(
        "Doel"
      ),
      
      p(
        paste0(
          "Dit dashboard verkent het geschatte voorkomen van ",
          "invasieve uitheemse planten langs autosnelwegen in Vlaanderen. ",
          "De resultaten kunnen helpen om soorten, ruimtelijke concentraties ",
          "en verschillen tussen AWV-districten te verkennen."
        )
      )
    ),
    
    
    div(
      class = "about-section",
      
      h3(
        "Methode"
      ),
      
      p(
        paste0(
          "Rond gekende soortwaarnemingen werd een buffer van 100 m ",
          "gebruikt als gestandaardiseerde benadering van het voorkomen. ",
          "Autosnelwegen werden gebufferd met 25 m als benadering van ",
          "de autosnelweg en de onmiddellijk aangrenzende bermzone."
        )
      ),
      
      p(
        paste0(
          "De weergegeven overlap geeft aan welk aandeel van de ",
          "onderzochte autosnelwegcorridor overlapt met de geschatte ",
          "verspreiding van een soort."
        )
      )
    ),
    
    
    div(
      class = "about-section",
      
      h3(
        "Interpretatie"
      ),
      
      p(
        paste0(
          "De analyse is een ruimtelijke screening en geen exacte ",
          "populatiekartering of volledige beheerprioritering. ",
          "Ook impact, wettelijke verplichtingen, veiligheid, ",
          "beheersbaarheid en kosten zijn relevant voor beheerkeuzes."
        )
      )
    ),
    
    
    div(
      class = "about-section",
      
      h3(
        "Verdere verfijning"
      ),
      
      p(
        paste0(
          "Gerichtere analyses zijn mogelijk wanneer ruimtelijke lagen ",
          "van het effectieve AWV-beheerareaal beschikbaar zijn, ",
          "bijvoorbeeld van wegbermen, grachten, bufferbekkens en ",
          "andere groenobjecten."
        )
      )
    )
  )
)


# ─────────────────────────────────────────────
# 8. Server
# ─────────────────────────────────────────────

server <- function(
    input,
    output,
    session
) {
  
  
  # ───────────────────────────────────────────
  # OVERZICHT
  # ───────────────────────────────────────────
  
  output$kpi_soorten <- renderText({
    
    nrow(
      autosnelwegen_metrics_dashboard
    )
  })
  
  
  output$kpi_districten <- renderText({
    
    length(
      districten
    )
  })
  
  
  output$kpi_max_overlap <- renderText({
    
    max_of <- max(
      autosnelwegen_metrics_dashboard$`of`,
      na.rm = TRUE
    )
    
    scales::percent(
      max_of,
      accuracy = 0.1
    )
  })
  
  
  output$kpi_max_soort <- renderUI({
    
    top_soort <- autosnelwegen_metrics_dashboard %>%
      slice_max(
        order_by = `of`,
        n = 1,
        with_ties = FALSE
      ) %>%
      pull(
        Soort
      )
    
    tags$span(
      top_soort
    )
  })
  
  
  output$global_bar <- renderHighchart({
    
    df <- autosnelwegen_metrics_dashboard %>%
      arrange(
        desc(`of`)
      ) %>%
      mutate(
        y = `of` * 100,
        kleur = status_kleuren[
          awv_status
        ]
      )
    
    punten <- map2(
      df$y,
      df$kleur,
      function(
    waarde,
    kleur
      ) {
        list(
          y = waarde,
          color = kleur
        )
      }
    )
    
    chart <- highchart() %>%
      
      hc_chart(
        type = "bar"
      ) %>%
      
      hc_plotOptions(
        bar = list(
          grouping = FALSE
        )
      ) %>%
      
      hc_xAxis(
        categories = df$Soort,
        title = list(
          text = ""
        ),
        labels = list(
          style = list(
            fontSize = "10px"
          )
        )
      ) %>%
      
      hc_yAxis(
        title = list(
          text = "Aandeel autosnelwegcorridor (%)"
        ),
        labels = list(
          format = "{value}%"
        ),
        min = 0
      ) %>%
      
      hc_add_series(
        name = "Geschat voorkomen",
        data = punten,
        showInLegend = FALSE
      ) %>%
      
      hc_add_series(
        name = "Reeds in AWV-visie",
        data = list(),
        color = kleur_awv_donker,
        showInLegend = TRUE,
        enableMouseTracking = FALSE
      ) %>%
      
      hc_add_series(
        name = "Vermeld in actualisatievraag",
        data = list(),
        color = kleur_awv,
        showInLegend = TRUE,
        enableMouseTracking = FALSE
      ) %>%
      
      hc_add_series(
        name = "Overige IUS",
        data = list(),
        color = kleur_grijs,
        showInLegend = TRUE,
        enableMouseTracking = FALSE
      ) %>%
      
      hc_legend(
        enabled = TRUE,
        layout = "horizontal",
        align = "center",
        verticalAlign = "top",
        itemStyle = list(
          fontFamily = "Arial",
          fontSize = "11px",
          fontWeight = "normal"
        )
      ) %>%
      
      hc_tooltip(
        headerFormat = "",
        pointFormat = paste0(
          "<b>{point.category}</b><br>",
          "{point.y:.2f}%"
        )
      )
    
    chart %>%
      hc_aw_vormgeving() %>%
      add_exporting()
  })
  
  
  output$overview_map <- renderLeaflet({
    
    max_soorten <- max(
      hex_totaal_leaflet$n_soorten,
      na.rm = TRUE
    )
    
    max_soorten <- max(
      1,
      max_soorten
    )
    
    pal <- colorNumeric(
      palette = c(
        "#FFF7D6",
        kleur_awv_donker
      ),
      domain = c(
        0,
        max_soorten
      ),
      na.color = "transparent"
    )
    
    
    leaflet(
      options = leafletOptions(
        preferCanvas = TRUE
      )
    ) %>%
      
      addProviderTiles(
        providers$Esri.WorldGrayCanvas
      ) %>%
      
      addPolygons(
        data = autosnelwegen_buffer_leaflet,
        fill = FALSE,
        color = "#777777",
        weight = 4,
        opacity = 0.25
      ) %>%
      
      addPolylines(
        data = wegsegmenten_leaflet,
        color = "#555555",
        weight = 1.4,
        opacity = 0.4
      ) %>%
      
      addPolygons(
        data = Provincies_grenzen_leaflet,
        fill = FALSE,
        color = "#555555",
        weight = 1,
        opacity = 0.7
      ) %>%
      
      addPolygons(
        data = hex_totaal_leaflet %>%
          filter(
            n_soorten > 0
          ),
        fillColor = ~pal(
          n_soorten
        ),
        fillOpacity = 0.88,
        color = "#666666",
        weight = 0.5,
        popup = ~paste0(
          "<b>Aantal IUS-planten: ",
          n_soorten,
          "</b><br><br>",
          soorten
        )
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = hex_totaal_leaflet$n_soorten,
        title = "Aantal IUS-planten",
        opacity = 0.8
      ) 
  })
  
  
  # ───────────────────────────────────────────
  # SOORTEN
  # ───────────────────────────────────────────
  
  selected_species_meta <- reactive({
    
    req(
      input$soort
    )
    
    species_meta %>%
      filter(
        Soort == input$soort
      ) %>%
      slice(
        1
      )
  })
  
  
  selected_species_hex <- reactive({
    
    req(
      input$soort
    )
    
    df <- hex_soort_leaflet %>%
      filter(
        Soort == input$soort,
        of_hex > 0
      )
    
    if (!is.null(input$soort_district) &&
        input$soort_district != "Heel Vlaanderen") {
      
      district_polygon <- district_buffers_leaflet %>%
        filter(
          labelWegbeheerder == input$soort_district
        )
      
      district_polygon_31370 <- district_polygon %>%
        st_transform(31370) %>%
        st_make_valid()
      
      df_31370 <- df %>%
        st_transform(31370) %>%
        st_make_valid()
      
      idx <- lengths(
        st_intersects(
          df_31370,
          district_polygon_31370
        )
      ) > 0
      
      df <- df[idx, ]
    }
    
    df
  })
  
  
  selected_species_occ <- reactive({
    
    req(
      input$soort
    )
    
    df <- occ_planten_leaflet %>%
      filter(
        Soort == input$soort
      )
    
    if (!is.null(input$soort_district) &&
        input$soort_district != "Heel Vlaanderen") {
      
      district_polygon <- district_buffers_leaflet %>%
        filter(
          labelWegbeheerder == input$soort_district
        )
      
      district_polygon_31370 <- district_polygon %>%
        st_transform(31370) %>%
        st_make_valid()
      
      df_31370 <- df %>%
        st_transform(31370) %>%
        st_make_valid()
      
      idx <- lengths(
        st_intersects(
          df_31370,
          district_polygon_31370
        )
      ) > 0
      
      df <- df[idx, ]
    }
    
    df
  })
  
  
  selected_species_districts <- reactive({
    
    req(
      input$soort
    )
    
    district_metrics %>%
      filter(
        Soort == input$soort
      ) %>%
      arrange(
        desc(
          of_district
        )
      )
  })
  
  
  output$species_header <- renderUI({
    
    df <- selected_species_meta()
    
    status <- df$awv_status[1]
    
    status_color <- status_kleuren[
      status
    ]
    
    text_color <- if (
      status == "Reeds in AWV-visie"
    ) {
      "white"
    } else {
      "#333333"
    }
    
    
    div(
      class = "species-header",
      
      div(
        df$Soort[1],
        class = "species-name"
      ),
      
      div(
        df$Species[1],
        class = "species-scientific"
      ),
      
      span(
        status,
        class = "status-badge",
        style = paste0(
          "background-color:",
          status_color,
          "; color:",
          text_color,
          ";"
        )
      )
    )
  })
  
  
  output$species_of <- renderText({
    
    df <- selected_species_meta()
    
    scales::percent(
      df$`of`[1],
      accuracy = 0.1
    )
  })
  
  
  output$species_map <- renderLeaflet({
    
    df <- selected_species_hex()
    occ_df <- selected_species_occ()
    
    req(
      nrow(df) > 0
    )
    
    pal <- colorNumeric(
      palette = c(
        "#FFF7D6",
        kleur_awv_donker
      ),
      domain = c(
        0,
        1
      ),
      na.color = "transparent"
    )
    
    bb <- st_bbox(
      df
    )
    
    
    kaart <- leaflet(
      options = leafletOptions(
        preferCanvas = TRUE
      )
    ) %>%
      
      addProviderTiles(
        providers$Esri.WorldGrayCanvas
      ) %>%
      
      addPolygons(
        data = autosnelwegen_buffer_leaflet,
        fill = FALSE,
        color = "#777777",
        weight = 4,
        opacity = 0.25
      ) %>%
      
      addPolylines(
        data = wegsegmenten_leaflet,
        color = "#555555",
        weight = 1.4,
        opacity = 0.4
      ) %>%
      
      addPolygons(
        data = Provincies_grenzen_leaflet,
        fill = FALSE,
        color = "#555555",
        weight = 1,
        opacity = 0.7
      ) %>%
      
      addPolygons(
        data = df,
        fillColor = ~pal(
          of_hex
        ),
        fillOpacity = 0.9,
        color = "#666666",
        weight = 0.5,
        popup = ~paste0(
          "<b>",
          Soort,
          "</b><br><br>",
          "Aandeel autosnelwegcorridor: ",
          scales::percent(
            of_hex,
            accuracy = 0.1
          )
        ),
        group = "Geschat voorkomen"
      )
    
    if (nrow(occ_df) > 0) {
      
      kaart <- kaart %>%
        addCircleMarkers(
          data = occ_df,
          radius = 3,
          stroke = TRUE,
          color = "#333333",
          weight = 0.7,
          fillColor = kleur_awv_donker,
          fillOpacity = 0.8,
          group = "Waarnemingen",
          popup = ~paste0(
            "<b>",
            Soort,
            "</b>"
          )
        )
    }
    
    kaart %>%
      addLayersControl(
        overlayGroups = c(
          "Geschat voorkomen",
          "Waarnemingen"
        ),
        options = layersControlOptions(
          collapsed = TRUE
        )
      ) %>%
      hideGroup(
        "Waarnemingen"
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = c(
          0,
          1
        ),
        title = "Aandeel corridor",
        labFormat = labelFormat(
          transform = function(x) {
            x * 100
          },
          suffix = "%"
        ),
        opacity = 0.8
      ) 
  })
  
  
  output$species_district_bar <- renderHighchart({
    
    df <- selected_species_districts() %>%
      arrange(
        desc(of_district)
      ) %>%
      mutate(
        y = of_district * 100
      )
    
    chart <- highchart() %>%
      
      hc_chart(
        type = "bar"
      ) %>%
      
      hc_xAxis(
        categories = df$labelWegbeheerder,
        title = list(
          text = ""
        ),
        labels = list(
          style = list(
            fontSize = "10px"
          )
        )
      ) %>%
      
      hc_yAxis(
        title = list(
          text = "Aandeel corridor (%)"
        ),
        labels = list(
          format = "{value}%"
        ),
        min = 0
      ) %>%
      
      hc_add_series(
        name = "Geschat voorkomen",
        data = df$y,
        color = kleur_awv_donker,
        showInLegend = FALSE
      ) %>%
      
      hc_tooltip(
        headerFormat = "",
        pointFormat = paste0(
          "<b>{point.category}</b><br>",
          "{point.y:.2f}%"
        )
      )
    
    chart %>%
      hc_aw_vormgeving() %>%
      add_exporting()
  })
  
  
  # ───────────────────────────────────────────
  # DISTRICTEN
  # ───────────────────────────────────────────
  
  selected_district_data <- reactive({
    
    req(
      input$district
    )
    
    district_metrics %>%
      filter(
        labelWegbeheerder == input$district
      ) %>%
      arrange(
        desc(
          of_district
        )
      )
  })
  
  
  selected_district_buffer <- reactive({
    
    req(
      input$district
    )
    
    district_buffers_leaflet %>%
      filter(
        labelWegbeheerder == input$district
      )
  })
  
  
  selected_district_segments <- reactive({
    
    req(
      input$district
    )
    
    wegsegmenten_leaflet %>%
      filter(
        labelWegbeheerder == input$district
      )
  })
  
  
  output$district_name <- renderText({
    
    input$district
  })
  
  
  output$district_species_bar <- renderHighchart({
    
    df <- selected_district_data() %>%
      filter(
        of_district > 0
      ) %>%
      arrange(
        desc(of_district)
      ) %>%
      mutate(
        y = of_district * 100
      )
    
    chart <- highchart() %>%
      
      hc_chart(
        type = "bar"
      ) %>%
      
      hc_xAxis(
        categories = df$Soort,
        title = list(
          text = ""
        ),
        labels = list(
          style = list(
            fontSize = "10px"
          )
        )
      ) %>%
      
      hc_yAxis(
        title = list(
          text = "Aandeel corridor (%)"
        ),
        labels = list(
          format = "{value}%"
        ),
        min = 0
      ) %>%
      
      hc_plotOptions(
        bar = list(
          grouping = FALSE
        )
      ) %>%
      
      hc_add_series(
        name = "Reeds in AWV-visie",
        data = ifelse(
          df$awv_status == "Reeds in AWV-visie",
          df$y,
          NA_real_
        ),
        color = kleur_awv_donker,
        showInLegend = TRUE
      ) %>%
      
      hc_add_series(
        name = "Vermeld in actualisatievraag",
        data = ifelse(
          df$awv_status == "Vermeld in actualisatievraag",
          df$y,
          NA_real_
        ),
        color = kleur_awv,
        showInLegend = TRUE
      ) %>%
      
      hc_add_series(
        name = "Niet vermeld door AWV",
        data = ifelse(
          df$awv_status == "Overige IUS",
          df$y,
          NA_real_
        ),
        color = kleur_grijs,
        showInLegend = TRUE
      ) %>%
      
      hc_tooltip(
        headerFormat = "",
        pointFormat = paste0(
          "<b>{point.category}</b><br>",
          "{series.name}<br>",
          "{point.y:.2f}%"
        )
      )
    
    chart %>%
      hc_aw_vormgeving() %>%
      add_exporting()
  })
  
  
  output$district_map <- renderLeaflet({
    
    district_polygon <- selected_district_buffer()
    
    req(
      nrow(
        district_polygon
      ) > 0
    )
    
    district_polygon_31370 <- district_polygon %>%
      st_transform(31370) %>%
      st_make_valid()
    
    hex_totaal_31370 <- hex_totaal_leaflet %>%
      st_transform(31370) %>%
      st_make_valid()
    
    idx <- lengths(
      st_intersects(
        hex_totaal_31370,
        district_polygon_31370
      )
    ) > 0
    
    district_hex <- hex_totaal_leaflet[idx, ] %>%
      filter(
        n_soorten > 0
      )
    
    max_soorten <- max(
      hex_totaal_leaflet$n_soorten,
      na.rm = TRUE
    )
    
    max_soorten <- max(
      1,
      max_soorten
    )
    
    pal <- colorNumeric(
      palette = c(
        "#FFF7D6",
        kleur_awv_donker
      ),
      domain = c(
        0,
        max_soorten
      ),
      na.color = "transparent"
    )
    
    bb <- st_bbox(
      district_polygon
    )
    
    
    kaart <- leaflet(
      options = leafletOptions(
        preferCanvas = TRUE
      )
    ) %>%
      
      addProviderTiles(
        providers$Esri.WorldGrayCanvas
      ) %>%
      
      addPolygons(
        data = district_polygon,
        fill = FALSE,
        color = kleur_awv_donker,
        weight = 4,
        opacity = 0.8
      ) %>%
      
      addPolylines(
        data = selected_district_segments(),
        color = "#444444",
        weight = 2,
        opacity = 0.7
      )
    
    
    if (nrow(district_hex) > 0) {
      
      kaart <- kaart %>%
        
        addPolygons(
          data = district_hex,
          fillColor = ~pal(
            n_soorten
          ),
          fillOpacity = 0.88,
          color = "#666666",
          weight = 0.5,
          popup = ~paste0(
            "<b>Aantal IUS-planten: ",
            n_soorten,
            "</b><br><br>",
            soorten
          )
        )
    }
    
    
    kaart %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = hex_totaal_leaflet$n_soorten,
        title = "Aantal IUS-planten",
        opacity = 0.8
      ) 
  })
  
  
  output$district_table <- renderTable({
    
    selected_district_data() %>%
      
      filter(
        of_district > 0
      ) %>%
      
      arrange(
        desc(
          of_district
        )
      ) %>%
      
      transmute(
        Soort = Soort,
        `AWV-context` = awv_status,
        `District (%)` = round(
          of_district * 100,
          2
        ),
        `Vlaanderen (%)` = round(
          `of` * 100,
          2
        )
      )
    
  },
  striped = TRUE,
  hover = TRUE,
  bordered = FALSE,
  spacing = "s"
  )
  
  
  # ───────────────────────────────────────────
  # HEATMAP DISTRICTEN
  # ───────────────────────────────────────────
  
  output$district_heatmap <- renderHighchart({
    
    district_order <- sort(
      unique(
        district_metrics$labelWegbeheerder
      )
    )
    
    species_order <- autosnelwegen_metrics_dashboard %>%
      arrange(
        desc(
          `of`
        )
      ) %>%
      pull(
        Soort
      )
    
    
    df <- district_metrics %>%
      filter(
        Soort %in% species_order,
        labelWegbeheerder %in% district_order
      ) %>%
      mutate(
        x = match(
          labelWegbeheerder,
          district_order
        ) - 1,
        y = match(
          Soort,
          species_order
        ) - 1,
        value = of_district * 100
      )
    
    
    heatmap_data <- pmap(
      list(
        df$x,
        df$y,
        df$value,
        df$Soort,
        df$labelWegbeheerder
      ),
      function(
    x,
    y,
    value,
    soort,
    district
      ) {
        
        list(
          x = x,
          y = y,
          value = value,
          soort = soort,
          district = district
        )
      }
    )
    
    
    max_value <- max(
      df$value,
      na.rm = TRUE
    )
    
    
    chart <- highchart() %>%
      
      hc_chart(
        type = "heatmap"
      ) %>%
      
      hc_xAxis(
        categories = district_order,
        title = list(
          text = ""
        ),
        labels = list(
          rotation = -45,
          style = list(
            fontSize = "9px"
          )
        )
      ) %>%
      
      hc_yAxis(
        categories = species_order,
        title = list(
          text = ""
        ),
        reversed = TRUE,
        labels = list(
          style = list(
            fontSize = "9px"
          )
        )
      ) %>%
      
      hc_colorAxis(
        min = 0,
        max = max_value,
        stops = list(
          list(
            0,
            "#FFFDF5"
          ),
          list(
            0.5,
            kleur_awv_licht
          ),
          list(
            1,
            kleur_awv_donker
          )
        ),
        labels = list(
          format = "{value}%"
        )
      ) %>%
      
      hc_add_series(
        data = heatmap_data,
        borderWidth = 0.5,
        borderColor = "white",
        name = "Geschat voorkomen"
      ) %>%
      
      hc_tooltip(
        headerFormat = "",
        pointFormat = paste0(
          "<b>{point.soort}</b><br>",
          "{point.district}<br>",
          "{point.value:.2f}%"
        )
      )
    
    
    chart %>%
      hc_aw_vormgeving() %>%
      add_exporting()
  })
}


# ─────────────────────────────────────────────
# 9. App starten
# ─────────────────────────────────────────────

shinyApp(
  ui = ui,
  server = server
)