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
library(rsconnect)
library(ggtext)
library(ggthemes)

# Custom plot theme

custom_theme <- function() {
  theme_few() +
    theme(
      axis.title.y = element_text(size = 8, family = "Arial"),
      axis.text.y = element_text(size = 8, family = "Arial"),
      axis.title.x = element_text(size = 8, family = "Arial"),
      axis.text.x = element_text(size = 8, family = "Arial"),
      axis.ticks.length = unit(1.5, "pt"),
      axis.ticks = element_line(colour = "grey90"),
      legend.text = element_text(size = 8, family = "Arial"),
      legend.title = element_blank(),
      panel.border = element_rect(color = "grey90", fill = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      panel.background = element_rect(color = "grey", fill = "grey98"),
      strip.text = element_text(size = 9, face = "bold", hjust = -0.01, family = "Arial"),
      #strip.text.x.top = element_text(margin = margin(0,0,3,0), hjust = 0.5, vjust = 0),
      plot.title = element_text(size = 10, family = "Arial", face = "bold", hjust = 0.5),
      plot.title.position = "plot"
    )
}

##### DATA INLEZEN VOOR PUBLISHING
# Data inlezen
## Spatial data
Vlaanderen_grenzen <- st_read("https://raw.githubusercontent.com/inbo/prius-radius/main/prius/data/spatial/flanders_wgs84.geojson")
Provincies_grenzen <- st_read("https://raw.githubusercontent.com/inbo/prius-radius/main/prius/data/spatial/Provincies.geojson")

readRDSfromURL <- function(url) {
  temp <- tempfile(fileext = ".rds")
  download.file(url, temp, mode = "wb")
  readRDS(temp)
}

ps_hbtrl_deel <- readRDSfromURL("https://github.com/inbo/prius-radius/raw/dashboard/radius/data/spatial/ps_hbtrl_deel.rds") %>%
  rename(code = gebcode)

#ps_hbtrl_deel$geom <- st_sfc(ps_hbtrl_deel$geometry)

#Samenvatting van ps_hbtrl_deel zodat ik één rij/1 MULTIPOLYGON krijg per gebied
ps_hbtrl_wgs84 <- ps_hbtrl_deel %>%
  st_set_crs(31370) %>%
  group_by(code, naam, gebopp_ha) %>%
  summarise(geometry = sf::st_union(geom)) %>%
  ungroup() %>%
  st_transform(4326) %>%
  sf::st_cast("MULTIPOLYGON")

ps_vglrl_wgs84 <- readRDSfromURL("https://github.com/inbo/prius-radius/raw/dashboard/radius/data/spatial/WGS84/ps_vglrl_wgs84.rds") %>%
  rename(code = na2000code, naam = gebnaam)

n2khab_wgs84 <- readRDSfromURL("https://github.com/inbo/prius-radius/raw/dashboard/radius/data/spatial/WGS84/n2khab_wgs84.rds") %>%
  rename(code = type, naam = name)

ps_nbhp_wgs84 <- readRDSfromURL("https://github.com/inbo/prius-radius/raw/dashboard/radius/data/spatial/WGS84/ps_nbhp_wgs84.rds") %>%
  rename(code = eigendomtype, naam = natuurbeheerplantype)

am_patdat_wgs84 <- readRDSfromURL("https://github.com/inbo/prius-radius/raw/dashboard/radius/data/spatial/WGS84/am_patdat_wgs84.rds") %>%
  rename(code = regio, naam = domeinnaam)

# lu_sbp_pgs <- readRDSfromURL("https://github.com/inbo/prius-radius/raw/dashboard/radius/data/spatial/lu_sbp_pgs.rds") 
# 
# lu_sbp_pls <- readRDSfromURL("https://github.com/inbo/prius-radius/raw/dashboard/radius/data/spatial/lu_sbp_pls.rds") 

list_wfs <- list("Habitatrichtlijngebieden (SBZ-H)" = ps_hbtrl_wgs84, "Vogelrichtlijngebieden (SBZ-V)" = ps_vglrl_wgs84, "Natura 2000 Habitattypes" = n2khab_wgs84, "Natuurbeheerplannen" = ps_nbhp_wgs84, "ANB patrimonium" = am_patdat_wgs84)

## Metric data
HBTRL <- ps_hbtrl_wgs84 %>%
  right_join(read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/HBTRL_long.csv"), by = c("code" = "gebied"), keep = TRUE)
VGLRL <- ps_vglrl_wgs84 %>%
  right_join(read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/VGLRL_long.csv"), by = c("code" = "gebied"), keep = TRUE)
N2KHAB <- n2khab_wgs84 %>%
  right_join(read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/N2KHAB_long.csv"), by = c("code" = "gebied"), keep = TRUE)
NBHP <- read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/NBHP_long.csv")
PATDAT <- read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/PATDAT_incl_ob_long.csv")

SBP_pgs <- read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/SBP_pgs_long.csv") %>%
  separate(gebied, into = c("gebied", "deelgebied"), sep = "\\s*\\(\\s*", fill = "right") %>%
  mutate(deelgebied = gsub("\\)$", "", deelgebied)) %>%
  mutate(gebied = gsub("\\s{2,}", " ", gebied)) %>%
  arrange(gebied)


SBP_pls <- read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/SBP_pls_long.csv")


list_metrics <- list("Habitatrichtlijngebieden (SBZ-H)" = HBTRL, "Vogelrichtlijngebieden (SBZ-V)" = VGLRL, "Natura 2000 Habitattypes" = N2KHAB, "Natuurbeheerplannen" = NBHP, "ANB patrimonium" = PATDAT, "Soortenbeschermingsprogramma's (SBP's)" = SBP_pgs)

## Species data
species_list <- read_csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/input/radius_species_list.csv")

occ_flanders <- read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/input/gbif_occ_flanders.csv") %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = "+proj=longlat +datum=WGS84") %>%
  arrange(Soort, .locale = "en")

habitats <- read_csv2("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/input/toewijzing_habitats.csv", col_types = cols(code = col_character()))

##### DATA INLEZEN VOOR TESTEN APP

# # Data inlezen
# ## Spatial data
# Vlaanderen_grenzen <- st_read("C:/Users/fleur_petersen/Documents/GitHub/prius-radius/prius/data/spatial/flanders_wgs84.geojson")
# Provincies_grenzen <- st_read("C:/Users/fleur_petersen/Documents/GitHub/prius-radius/prius/data/spatial/Provincies.geojson")
# 
# 
# ps_hbtrl_deel <- readRDS("C:/Users/fleur_petersen/Documents/GitHub/prius-radius/radius/data/spatial/ps_hbtrl_deel.rds") %>%
#   rename(code = gebcode)
# 
# #Samenvatting van ps_hbtrl_deel zodat ik één rij/1 MULTIPOLYGON krijg per gebied
# ps_hbtrl_wgs84 <- ps_hbtrl_deel %>%
#   st_set_crs(31370) %>%
#   group_by(code, naam, gebopp_ha) %>%
#   summarise(geometry = sf::st_union(geom)) %>%
#   ungroup() %>%
#   st_transform(4326) %>%
#   sf::st_cast("MULTIPOLYGON")
# 
# ps_vglrl_wgs84 <- st_read("C:/Users/fleur_petersen/Documents/GitHub/prius-radius/radius/data/spatial/WGS84/ps_vglrl_wgs84.shp") %>%
#   rename(code = na2000code, naam = gebnaam)
# 
# n2khab_wgs84 <- readRDS("C:/Users/fleur_petersen/Documents/GitHub/prius-radius/radius/data/spatial/WGS84/n2khab_wgs84.rds") %>%
#   rename(code = type, naam = name)
# 
# ps_nbhp_wgs84 <- readRDS("C:/Users/fleur_petersen/Documents/GitHub/prius-radius/radius/data/spatial/WGS84/ps_nbhp_wgs84.rds") %>%
#   rename(code = eigendomtype, naam = natuurbeheerplantype)
# 
# am_patdat_wgs84 <- readRDS("C:/Users/fleur_petersen/Documents/GitHub/prius-radius/radius/data/spatial/WGS84/am_patdat_wgs84.rds") %>%
#   rename(code = regio, naam = domeinnaam)
# 
# list_wfs <- list("Habitatrichtlijngebieden (SBZ-H)" = ps_hbtrl_wgs84, "Vogelrichtlijngebieden (SBZ-V)" = ps_vglrl_wgs84, "Natura 2000 Habitattypes" = n2khab_wgs84, "Natuurbeheerplannen" = ps_nbhp_wgs84, "ANB patrimonium" = am_patdat_wgs84)
# 
# ## Metric data
# HBTRL <- ps_hbtrl_wgs84 %>%
#   right_join(read.csv("C:/Users/fleur_petersen/Documents/GitHub/prius-radius/radius/data/output/HBTRL_long.csv"), by = c("code" = "gebied"), keep = TRUE)
# VGLRL <- ps_vglrl_wgs84 %>%
#   right_join(read.csv("C:/Users/fleur_petersen/Documents/GitHub/prius-radius/radius/data/output/VGLRL_long.csv"), by = c("code" = "gebied"), keep = TRUE)
# N2KHAB <- n2khab_wgs84 %>%
#   right_join(read.csv("C:/Users/fleur_petersen/Documents/GitHub/prius-radius/radius/data/output/N2KHAB_long.csv"), by = c("code" = "gebied"), keep = TRUE)
# NBHP <- read.csv("C:/Users/fleur_petersen/Documents/GitHub/prius-radius/radius/data/output/NBHP_long.csv")
# 
# PATDAT <- read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/PATDAT_incl_ob_long.csv")
# 
# SBP_pgs <- read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/SBP_pgs_long.csv") %>%
#   separate(gebied, into = c("gebied", "deelgebied"), sep = "\\s*\\(\\s*", fill = "right") %>%
#   mutate(deelgebied = gsub("\\)$", "", deelgebied)) %>%
#   mutate(gebied = gsub("\\s{2,}", " ", gebied)) %>%
#   arrange(gebied)
# 
# 
# SBP_pls <- read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/SBP_pls_long.csv")
# 
# list_metrics <- list("Habitatrichtlijngebieden (SBZ-H)" = HBTRL, "Vogelrichtlijngebieden (SBZ-V)" = VGLRL, "Natura 2000 Habitattypes" = N2KHAB, "Natuurbeheerplannen" = NBHP, "ANB patrimonium" = PATDAT, "Soortenbeschermingsprogramma's (SBP's)" = SBP_pgs)
# 
# ## Species data
# species_list <- read_csv("C:/Users/fleur_petersen/Documents/GitHub/prius-radius/radius/data/input/radius_species_list.csv")
# 
# occ_flanders <- read.csv("C:/Users/fleur_petersen/Documents/GitHub/prius-radius/radius/data/input/gbif_occ_flanders.csv") %>%
#   st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = "+proj=longlat +datum=WGS84") %>%
#   arrange(Soort, .locale = "en")
# 
# habitats <- read_csv2("C:/Users/fleur_petersen/Documents/GitHub/prius-radius/radius/data/input/toewijzing_habitats.csv", col_types = cols(code = col_character()))

############################
########### APP ############
############################

custom_theme <- function() {
  theme_few() +
    theme(
      axis.title.y = element_text(size = 8, family = "Arial"),
      axis.text.y = element_text(size = 8, family = "Arial"),
      axis.title.x = element_text(size = 8, family = "Arial"),
      axis.text.x = element_text(size = 8, family = "Arial"),
      axis.ticks.length = unit(1.5, "pt"),
      axis.ticks = element_line(colour = "grey90"),
      legend.text = element_text(size = 8, family = "Arial"),
      legend.title = element_blank(),
      panel.border = element_rect(color = "grey90", fill = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      panel.background = element_rect(color = "grey", fill = "grey98"),
      strip.text = element_text(size = 9, face = "bold", hjust = -0.01, family = "Arial"),
      plot.title = element_text(size = 10, family = "Arial", face = "bold", hjust = 0.5),
      plot.title.position = "plot"
    )
}

# Custom CSS lay-out 
custom_css <- "

  .p {
    font-size: 9px;
    font-weight: normal;
    color: black;
  }
  
  .h1 {
    font-size: 12px;
    font-weight: bold;
    color: black;
    margin-bottom: 10
  }
  
  .h2 {
    font-size: 11px;
    font-weight: normal;
    color: black;
  }

  .custom-sidebar {
    width: 250px;
    height: 100%;
    padding: 10px;
    background-color: #f8f9fa; 
    box-shadow: 0 4px 8px rgba(0,0,0.1,0.1);
    border-radius: 0px;
    font-size: 12px;
  }
  
  label.control-label {
    font-size: 12px; 
  }
  
  .selectize-input {
    font-size: 12px; 
  }

  .selectize-dropdown .option {
    font-size: 11px; 
  }
  
  .custom-header-row {
      flex-direction: column;
      height: 100px; 
      margin-bottom: 10px;
      padding-top: 10px;
      margin-left: 5px;
      margin-right: 5px;
      background-color: #f8f9fa;
      box-shadow: 0 4px 8px rgba(0,0,0.1,0.1);
      border-radius: 0px;
  }

  .custom-header {
      font-size: 2em;
      font-weight: bold;
      color: black;
  }
  
  .custom-header-row2 {
      flex-direction: column;
      height: 150px; 
      margin-bottom: 10px;
      padding-top: 10px;
      margin-left: 5px;
      margin-right: 5px;
      background-color: #f8f9fa;
      box-shadow: 0 4px 8px rgba(0,0,0.1,0.1);
      border-radius: 0px;
  }
  
  .custom-subheader {
      font-size: 1em;
      font-weight: normal;
      color: grey;
      margin-bottom: 5px;
  }
  
  .custom-value-box {
    display: flex;
    flex-direction: column;
    text-align: left;
    height: 140px;
    box-shadow: none; 
    border: none; 
    background-color: #f8f9fa !important;
    border-radius: 0;
   }
  
  .custom-valuebox-text {
      font-size: 10px;
      font-weight: bold;
      color: grey;
      margin-top: 5px;
  }
  
  .custom-kaart-row {
    height: 600px;
    overflow: hidden; 
  }
  
  .custom-container {
    display: flex;
    flex-direction: row;
    height: 100%;
  }
  
  .nav-underline .nav-link {
  font-size: 10px;
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

  
  
  .custom-plotly-header {
    height: 150px; /* Adjust this value as needed */
  }
"

########### UI ############

ui <- page_navbar(
  title = "RadIUS dashboard",
  bg = "#c04384",
  inverse = TRUE,
  header = tags$head(
    tags$style(HTML(custom_css)),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
  ),
  
  ## PAGINA 1 - GEBIEDSFICHES
  tabPanel(
    title = "Gebiedsfiches",
    div(
      class = "custom-container",
      
      ### Sidebar
      div(
        class = "custom-sidebar",
        
        selectizeInput("kaart", "Kaarttype:", choices = names(list_metrics)),
        uiOutput("gebiedsfiches_input"), 
        #eventueel hier voor N2KHAB nog een optie toevoegen op per habitatgroep te selecteren (bossen, kustduinen, zoetwater, ...)?
        radioButtons("soort_selectie", "Selecteer soorten:",
                     choices = c("Alle Soorten" = "all", 
                                 "Unielijstsoorten" = "unielijst", 
                                 "Niet-Unielijstsoorten" = "niet_unielijst"),
                     selected = "all"),
      ),
      
      ### Main Content
      div(
        class = "main-content",
        
        #### Header
        fluidRow(
          class = "custom-header-row",
          column(9, 
                 textOutput("gebiedsnaam") %>% tagAppendAttributes(class = "custom-header"),
                 textOutput("deelgebied") %>% tagAppendAttributes(class = "custom-subheader")),
          column(3, plotlyOutput("kaart2", height = "100px"))
        ),
        
        #### Main 
        uiOutput("gebiedsfiches_ui")
      )
    )
  ),
  
  
  ## PAGINA 2 - SOORTENFICHES
  tabPanel(
    title = "Soortenfiches",
    div(class = "custom-container",
        
        ### Sidebar
        div(class = "custom-sidebar",
            selectizeInput(inputId = "soort", label = "Soort:", 
                           choices = sort(unique(occ_flanders$Soort)), selected = 1),
            selectInput('kaart2', label = 'Kaarttype:', choices = names(list_metrics)),
            uiOutput("soortenfiches_input")
        ),
        
        ### Main content
        div(class = "main-content",
            
            #### Header
            fluidRow(
              class = "custom-header-row2",
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
            
            #### Main
            uiOutput("soortenfiches_ui")
        )
    )
  ),
  
  ## PAGINA 3 - OVER DASHBOARD
  tabPanel("Over",
           h1("RadIUS project"),
           p("Het RadIUS-project voert een doorgedreven analyse uit van het voorkomen van invasieve uitheemse soorten (IUS, met name deze van de Unielijst) in voor de natuursector relevante gebieden. Afgaande op gekende puntlocaties van de betrokken soorten, buigen we ons over het voorkomen van Unielijst- en andere soorten in beschermde natuur. We toetsen de verspreiding aan (1) de Vogelrichtlijngebieden, (2) de Habitatrichtlijngebieden, en ook aan (3) de Natura2000-habitattypes. Dezelfde toetsing gebeurt vervolgens voor (4) alle gebieden met een effectief natuurbeheer (natuurbeheerplan), en (5) de domeinen in beheer bij het ANB."),
           h2("Werkwijze"),
           h2("Bronnen"),
           h3("Kaartlagen"),
           h3("Verspreidingsdata"),
           p("alle verspreidingsdata komt van GBIF. Laatste download: "),
           h2("Relevante links"),
           h2("Dashboard gebruik"),
           h2("Contact"),
           p("Het dashboard wordt onderhouden door het Instituut voor Natuur- en Bosonderzoek (INBO), als onderdeel van het RadIUS-project. Bij vragen of onduidelijkheden kunt u steeds terecht bij faunabeheer@inbo.be")
  ),
  nav_spacer()
)


########### SERVER ############
server <- function(input, output, session) {
  
  # PAGINA 1 - GEBIEDSFICHES
  
  ## REACTIEVE ELEMENTEN
  metrics <- reactive({
    data <- list_metrics[[input$kaart]] %>%
      st_drop_geometry() %>%
      left_join(species_list, by = c("soort" = "Soort"), relationship = "many-to-many") 
    
    if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)")) {
      data <- data %>%
        filter(type == "of") %>%
        select(soort, species, abbr, Groep, code, naam, overlap, EU_lijst) %>%
        distinct()
    }
    else if (input$kaart == "Natura 2000 Habitattypes") {
      data <- data %>% 
        filter(type == "of") %>%
        left_join(habitats, by = c("code", "naam" = "habitat")) %>%
        select(soort, species, abbr, Groep, code, naam, groep, overlap, EU_lijst) %>%
        distinct()
    } 
    else if (input$kaart == "Natuurbeheerplannen") {
      data <- data %>%
        filter(!type %in% c("in", "sel")) %>%
        select(soort, species, abbr, Groep, gebied, type, overlap, EU_lijst) %>%
        distinct()
    }
    else if (input$kaart == "ANB patrimonium") {
      data <- data %>%
        filter(!type %in% c("in", "sel")) %>%
        select(soort, species, abbr, Groep, gebied, type, overlap, EU_lijst) %>%
        distinct()
    }
    else {
      data <- data %>% 
        filter(gebied == input$sbp & type == "of") %>%
        select(soort, species, abbr, Groep, gebied, deelgebied, overlap, EU_lijst) %>%
        distinct()
    }
  
    filtered_data <- switch(input$soort_selectie,
                            "all" = data,
                            "unielijst" = data %>% filter(EU_lijst != 0),
                            "niet_unielijst" = data %>% filter(EU_lijst == 0)
    )
    
    return(filtered_data)
  })
  
  metrics_nspec_per_gebied <- reactive({
    if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
      df <- metrics() %>% 
        filter(!is.na(code) & overlap != 0) %>%
        group_by(code, naam) %>%
        summarise(y = n()) %>%
        arrange(desc(y))
    }
    else if (input$kaart == "Natura 2000 Habitattypes") {
      df <- metrics() %>% 
        filter(!is.na(code) & overlap != 0) %>%
        group_by(code, naam) %>%
        summarise(y = n()) %>%
        arrange(desc(y)) %>% 
        left_join(habitats, by = c("code")) %>%
        select(groep, code, naam, y)
    }
    
    else if (input$kaart == "Soortenbeschermingsprogramma's (SBP's)") {
      df <- metrics() %>%
        filter(!is.na(deelgebied) & overlap != 0) %>%
        group_by(deelgebied) %>%
        rename(code = deelgebied) %>%
        summarise(y = n()) %>%
        arrange(desc(y))
    }
    
    else {
      df <- df
    }
  })
  
  ## OUTPUTS
  output$gebiedsnaam <- renderText({
    input$kaart
  })


  output$gebiedsfiches_input <- renderUI({
    if (input$kaart == "Soortenbeschermingsprogramma's (SBP's)") {
      tagList(
        selectizeInput("sbp", 
                       label = "SBP:", 
                       choices = unique(list_metrics[[input$kaart]]$gebied),
                       selected = NULL),
        uiOutput("sbp_deelgebied")
      )
    } else if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
      data <- list_metrics[[input$kaart]]
      
      df <- data.frame(naam = data$naam, code = data$code, stringsAsFactors = FALSE)
      
      df <- na.omit(df)
      df <- df[order(df$naam), ]
      
      choices <- setNames(df$code, paste(df$naam, "(", df$code, ")", sep = " "))
      
      selectizeInput("deelgebied", 
                     label = "Deelgebied:", 
                     choices = c("All" = "All", choices))
    } else if (input$kaart == "ANB patrimonium") {
      selectizeInput("regio", 
                     label = "Beheerregio:", 
                     choices = c("All" = "All", unique(am_patdat_wgs84$code)))
    } else {
      NULL 
    }
  })
  
  output$sbp_deelgebied <- renderUI({
    req(input$sbp)
    deelgebieden <- unique(list_metrics[[input$kaart]]$deelgebied[list_metrics[[input$kaart]]$gebied == input$sbp])
    selectizeInput("sbp_deelgebied", 
                   label = "Deelgebied:", 
                   choices = c("All" = "All", deelgebieden))
  })
  
  output$deelgebied <- renderText({
    
    req(input$kaart, input$deelgebied)
    
    if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes") & input$deelgebied != "All") {
      list_metrics[[input$kaart]] %>%
        filter(code == input$deelgebied) %>%
        st_drop_geometry() -> data
      
      paste0(unique(data$naam), " (", unique(data$code), ")")
    }
    else if (input$kaart == "Soortenbeschermingsprogramma's (SBP's)") {
      input$sbp
    }
    else {
      NULL
    }
  })

  output$gebiedsfiches_ui <- renderUI({
    req(input$kaart, input$deelgebied)
    
    # Explanation text based on deelgebied input
    explanation_text_bezetting <- if (input$deelgebied == "All") {
      "Deze sectie toont de figuren die betrekking hebben op de bezetting van de gebieden, zowel in percentage oppervlak als het aantal gebieden."
    } else {
      "Deze sectie toont het percentage oppervlak bezet door soorten voor het geselecteerde deelgebied."
    }
    
    explanation_text_verspreiding <- if (input$deelgebied == "All") {
      "De onderstaande barplot toont het aantal soorten per deelgebied."
    } else {
      "De onderstaande barplot toont het aantal soorten per deelgebied."
    }
    
    tagList(
      div(
        style = "margin-bottom: 20px;",
        h3("Bezetting", style = "margin-left: 15px; margin-top: 15px; font-size: 18px; font-weight: bold;"),
        
        p(explanation_text_bezetting, 
          style = "font-size: 12px; margin-bottom: 10px; margin-left: 15px;"),
        
        # Tabs for All deelgebieden
        if (input$deelgebied == "All") {
          navset_underline(
            id = "tabset_bezetting",
            nav_panel(title = "Percentage Oppervlak", value = "percentage"), 
            nav_panel(title = "Aantal Gebieden", value = "aantal_gebieden")  
          )
        }
      ),
      
      div(
        style = "padding: 5px;",
        
        # Conditional display of percentage_oppervlak chart
        conditionalPanel(
          condition = if (input$deelgebied == "All") {
            "input.tabset_bezetting == 'percentage'"
          } else {
            "input.deelgebied != 'All'"
          },
          fluidRow(
            height = "600px",
            highchartOutput("percentage_oppervlak", height = "600px")
          )
        ),
        
        # Conditional display of aantal_gebieden chart
        conditionalPanel(
          condition = if (input$deelgebied == "All") {
            "input.tabset_bezetting == 'aantal_gebieden'"
          } else {
            "false" 
          },
          fluidRow(
            height = "600px",
            highchartOutput("aantal_gebieden", height = "600px")
          )
        ),
        
        # Download buttons below the figures
        fluidRow(
          conditionalPanel(
            condition = if (input$deelgebied == "All") {
              "input.tabset_bezetting == 'percentage'"
            } else {
              "false" 
            },
            column(6, 
                   actionButton("download_data_sp_in_gebied", 
                                label = "Download Data", 
                                icon = icon("download"), 
                                style = "font-size: 12px; background-color: transparent; border: none; color: black;"),
                   actionButton("download_png_percentage", 
                                label = "Download Figuur", 
                                icon = icon("file-image"), 
                                style = "font-size: 12px; background-color: transparent; border: none; color: black;")
            )
          ),
          
          conditionalPanel(
            condition = if (input$deelgebied == "All") {
              "input.tabset_bezetting == 'aantal_gebieden'"
            } else {
              "input.deelgebied != 'All'" 
            },
            column(6,
                   actionButton("download_data_percentage_oppervlak", 
                                label = "Download Data", 
                                icon = icon("download"), 
                                style = "font-size: 12px; background-color: transparent; border: none; color: black;"),
                   actionButton("download_png_aantal", 
                                label = "Download Figuur", 
                                icon = icon("file-image"), 
                                style = "font-size: 12px; background-color: transparent; border: none; color: black;")
            )
          )
        )
      ),
      
      # Barplot for Verspreiding
      if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", 
                             "Vogelrichtlijngebieden (SBZ-V)", 
                             "Natura 2000 Habitattypes", 
                             "Soortenbeschermingsprogramma's (SBP's)") & 
          input$deelgebied == "All") {
        div(
          style = "margin-bottom: 20px;",
          
          h3("Verspreiding", style = "margin-left: 15px; margin-top: 15px; font-size: 18px; font-weight: bold;"),
          
          p(explanation_text_verspreiding, 
            style = "font-size: 12px; margin-bottom: 10px; margin-left: 15px;"),
          
          fluidRow(
            height = "600px",
            highchartOutput("barplot_verspreiding", height = "600px")
          ),
          
          fluidRow(
            column(6,
                   actionButton("download_data_percentage_oppervlak", 
                                label = "Download Data", 
                                icon = icon("download"), 
                                style = "font-size: 12px;background-color: transparent;border:none;color:black"),
                   actionButton("download_png_verspreiding", 
                                label = "Download Figuur", 
                                icon = icon("file-image"), 
                                style = "font-size: 12px;background-color: transparent;border:none;color:black")
            )
          )
        )
      }
    )
  })
  
  output$download_data_sp_in_gebied <- downloadHandler(
    filename = function() {
      paste("data_sp_in_gebied_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(metrics(), file, row.names = FALSE)
    }
  )
  
  output$download_data_percentage_oppervlak <- downloadHandler(
    filename = function() {
      paste("data_percentage_oppervlak_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(metrics(), file, row.names = FALSE)
    }
  )
  
  # Download PNG images based on current tab
  output$download_png_percentage <- downloadHandler(
    filename = function() {
      paste("percentage_oppervlak_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      webshot::webshot(output$percentage_oppervlak, file = file)
    }
  )
  
  output$download_png_aantal <- downloadHandler(
    filename = function() {
      paste("aantal_gebieden_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      webshot::webshot(output$aantal_gebieden, file = file)
    }
  )
  
  output$download_png_verspreiding <- downloadHandler(
    filename = function() {
      paste("barplot_verspreiding_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      webshot::webshot(output$barplot_verspreiding, file = file)
    }
  )
  
  output$kaart2 <- renderPlotly({
    
    req(input$kaart, input$deelgebied)
    
    if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)")) {
      if (input$deelgebied == "All") {
        plot_ly() %>%
          add_sf(
            data = list_wfs[[input$kaart]],
            color = I("grey"),
            text = ~code,
            hoverinfo = "none",
            showlegend = FALSE
          ) %>%
          add_sf(
            data = Provincies_grenzen,
            fill = NA,
            color = I("darkgrey"),
            showlegend = FALSE
          ) %>%
          layout(
            xaxis = list(title = ""),
            yaxis = list(title = ""),
            margin = list(t = 0, r = 0, l = 0, b = 0),
            font = list(color = "black"),
            paper_bgcolor = "transparent",
            plot_bgcolor = "transparent"
          ) %>%
          config(displayModeBar = FALSE)
      } else {
        
        selected_area <- list_wfs[[input$kaart]] %>% filter(code == input$deelgebied)
        bbox <- st_bbox(selected_area)
        
        plot_ly() %>%
          add_sf(
            data = list_wfs[[input$kaart]],
            color = I("grey"),
            text = ~code,
            hoverinfo = "none",
            showlegend = FALSE
          ) %>%
          add_sf(
            data = Provincies_grenzen,
            fill = NA,
            color = I("darkgrey"),
            showlegend = FALSE
          ) %>%
          add_sf(
            data = selected_area,
            color = I("#c04384"),
            showlegend = FALSE
          ) %>%
          layout(
            xaxis = list(title = "", range = c(bbox["xmin"], bbox["xmax"])), 
            yaxis = list(title = "", range = c(bbox["ymin"], bbox["ymax"])), 
            margin = list(t = 0, r = 0, l = 0, b = 0),
            font = list(color = "black"),
            paper_bgcolor = "transparent",
            plot_bgcolor = "transparent"
          ) %>%
          config(displayModeBar = FALSE)
      }
    } else {
      return()
    }
  })
  
  output$percentage_oppervlak <- renderHighchart({

    if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
      if (input$deelgebied == "All") {
        data <- metrics() %>%
          filter(is.na(code))
      } 
      else {
        data <- metrics() %>%
          filter(code == input$deelgebied)
      }
    }
    else if (input$kaart == "Natuurbeheerplannen") {
      data <- metrics() %>%
        filter(type == "of" & gebied == "NBHP")
    }
    else if (input$kaart == "ANB patrimonium") {
      if (input$deelgebied == "All") {
        data <- metrics() %>%
          filter(type == "of" & gebied == "PATDAT_incl_ob")
      } 
      else {
        data <- metrics() %>%
          filter(gebied == input$deelgebied)
      }
    }
    else if (input$kaart == "Soortenbeschermingsprogramma's (SBP's)") {
      data <- metrics() %>%
        filter(is.na(deelgebied))
    }
    
    
    if (nrow(data) != 0) {
      df <- data %>%
        filter(overlap != 0) %>%
        arrange(desc(overlap)) %>%
        mutate(y = overlap * 100)

      chart <- highchart() %>%
        hc_chart(type = 'bar', height = 600) %>%
        hc_xAxis(categories = df$soort, title = list(text = ""), labels = list(rotation = -0, fontSize = "8px", step = 1), min = 0) %>%
        hc_yAxis(title = list(text = 'Overlap (%)'), labels = list(format = '{value}%')) %>%
        hc_plotOptions(
          bar = list(  
            dataLabels = list(enabled = TRUE, format = '{point.y:.2f}%'), 
            borderColor = "black",
            borderWidth = 0.2,
            #pointWidth = 10,
            pointPadding = 0.1, 
            groupPadding = 0
          )
        ) %>%
        hc_tooltip(
          headerFormat = '',
          pointFormat = '<b>{point.category}: {point.y:.2f}%</b>',
          style = list(color = "black", fontsize = '14px', fontWeight = 'bold')
        ) %>%
        hc_add_theme(hc_theme_elementary()) %>%
        hc_add_series(
          name = "Overlap (%)",
          data = df$y,
          color = "#e6b4ce"
        ) %>%
        hc_legend(enabled = FALSE)
      
      chart
    }
  })
  
  output$aantal_gebieden <- renderHighchart({
    
    if (input$kaart == "Natuurbeheerplannen") {
      df <- metrics() %>%
        filter(type == "n_nbhp") %>%
        transmute(soort, y = as.integer(overlap)) %>%
        arrange(desc(y))
    }
    else if (input$kaart == "ANB patrimonium") {
      df <- metrics() %>%
        filter(type == "n_domeinen") %>%
        transmute(soort, y = as.integer(overlap)) %>%
        arrange(desc(y))
    }
    else if (input$kaart == "Soortenbeschermingsprogramma's (SBP's)") {
      df <- metrics() %>%
        filter(!is.na(deelgebied) & overlap != 0) %>%
        group_by(soort) %>%
        summarise(y = n()) %>%
        arrange(desc(y)) 
    }
    else {
      df <- metrics() %>%
        filter(!is.na(code) & overlap != 0) %>%
        group_by(soort) %>%
        summarise(y = n()) %>%
        arrange(desc(y)) 
    }
    
    if (nrow(df) != 0) {
      chart <- highchart() %>%
        hc_chart(type = 'bar') %>%
        hc_xAxis(categories = df$soort, title = list(text = ""), labels = list(rotation = -0, fontSize = "8px", step = 1)) %>%
        hc_yAxis(title = list(text = '# gebieden'), labels = list(format = '{value}'), min = 0) %>%  
        hc_plotOptions(
          bar = list(  
            dataLabels = list(enabled = TRUE, format = '{point.y:.0f}'), 
            borderColor = "black",
            borderWidth = 0.2,
            pointPadding = 0, 
            groupPadding = 0
          )
        ) %>%
        hc_tooltip(
          headerFormat = '',
          pointFormat = '<b>{point.category}: {point.y:.0f}</b>',
          style = list(color = "black", fontsize = '14px', fontWeight = 'bold')
        ) %>%
        hc_chart(backgroundColor = 'rgba(0, 0, 0, 0)') %>%
        hc_add_theme(hc_theme_elementary()) %>%
        hc_add_series(
          name = "Overlap (%)",
          data = df$y,
          color = "#e6b4ce"
        ) %>%
        hc_legend(enabled = FALSE)
      
      chart
    }
  })
  
  output$barplot_verspreiding <- renderHighchart({
    df <- metrics_nspec_per_gebied()
    
    if (nrow(df) != 0) {
      
      if(input$kaart == "Natura 2000 Habitattypes") {
        df <- df %>%
          arrange(groep, desc(y))
        
        chart <- hchart(df, type = "bar", hcaes(x = code, y = y, group = groep))
      } else {
        chart <- hchart(df, type = "bar", hcaes(x = code, y = y))
      }
      
      chart <- chart %>%
        hc_chart(height = 500) %>%
        hc_xAxis(categories = df$code, title = list(text = ""), labels = list(rotation = -0, fontSize = "8px", step = 1)) %>%
        hc_yAxis(title = list(text = '# soorten'), labels = list(format = '{value}'), min = 0) %>%  
        hc_plotOptions(
          bar = list(  
            dataLabels = list(enabled = TRUE, format = '{point.y:.0f}'), 
            color = "#e6b4ce",
            borderColor = "black",
            borderWidth = 0.2,
            pointPadding = 0, 
            groupPadding = 0
          )
        ) %>%
        hc_tooltip(
          headerFormat = '',
          pointFormat = '<b>{point.category}: {point.y:.0f} soorten</b>',
          style = list(color = "black", fontsize = '14px', fontWeight = 'bold')
        ) %>%
        hc_chart(backgroundColor = 'rgba(0, 0, 0, 0)') %>%
        hc_add_theme(hc_theme_hcrt()) %>%
        hc_legend(enabled = FALSE)
      
      chart
    }
  })
  
  # PAGINA 2 - SOORTENFICHES
  
  ## REACTIEVE ELEMENTEN
  occ_species <- reactive({
    occ_flanders %>%
      filter(Soort == input$soort) 
  })

  metrics2 <- reactive({
    list_metrics[[input$kaart2]] %>%
      st_drop_geometry() %>%
      filter(soort == input$soort & type == "in") %>%
      left_join(species_list, by = c("soort" = "Soort"), relationship = "many-to-many") %>%
      select(soort, species, abbr, Groep, code, naam, overlap, EU_lijst) %>%
      distinct()
  })

  occ_sum <- reactive({
    occ_species() %>%
      st_drop_geometry() %>%
      count(year, name = "n") %>%
      right_join(data.frame(year = 2015:2024), by = "year") %>%
      arrange(year) %>%
      na.replace(0)
  })

  pal <- reactive({
    df <- metrics2() %>%
      filter(!is.na(code))
    
    colors <- c("lightgrey", colorRampPalette(c("lightgrey", "#c04384"))(99))
    
    if (nrow(df) != 0) {
      colorNumeric(
        palette = colors,
        domain = c(min(df$overlap), max(df$overlap))
      )
    }
  })
  
  ## OUTPUTS
  
  output$soortenfiches_input <- renderUI({
    if (input$kaart2 %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes", "Natuurbeheerplannen", "ANB patrimonium", "Soortenbeschermingsprogramma's (SBP's)")) {
      data <- list_metrics[[input$kaart2]]
      
      df <- data.frame(naam = data$naam, code = data$code, stringsAsFactors = FALSE)
      
      df <- na.omit(df)
      df <- df[order(df$naam), ]
      
      choices <- setNames(df$code, paste(df$naam, "(", df$code, ")", sep = " "))
      
      selectizeInput("deelgebied", 
                     label = "Deelgebied:", 
                     choices = c("All" = "All", choices))
    } else if (input$kaart == "ANB patrimonium") {
      choices <- setNames(am_patdat_wgs84$code, paste(am_patdat_wgs84$naam, am_patdat_wgs84$code, sep = " - "))
      
      selectizeInput("regio", 
                     label = "Beheerregio:", 
                     choices = c("All" = "All", choices))
    } else {
      NULL 
    }
  })

  output$soort <- renderText({
    input$soort
  })

  output$species <- renderText({
    paste(unique(metrics2()$species), collapse = ", ")
  })
  
  output$union_list <- renderUI({
    
    if (unique(metrics2()$EU_lijst) == 1) {
      HTML('<span style="color: #c04384; font-weight: bold;"><i class="fas fa-bell"></i> Op unielijst (uitvoeringsverordening 2016/1141)</span>')
    } else if (unique(metrics2()$EU_lijst) == 2) {
      HTML('<span style="color: #c04384; font-weight: bold;"><i class="fas fa-bell"></i> Op unielijst (uitvoeringsverordening 2017/1263)</span>')
    } else if (unique(metrics2()$EU_lijst) == 3) {
      HTML('<span style="color: #c04384; font-weight: bold;"><i class="fas fa-bell"></i> Op unielijst (uitvoeringsverordening 2019/1262)</span>')
    } else if (unique(metrics2()$EU_lijst) == 4) {
      HTML('<span style="color: #c04384; font-weight: bold;"><i class="fas fa-bell"></i> Op unielijst (uitvoeringsverordening 2022/1203)</span>')
    } else {
      HTML("")
    }
  })

  output$tot_obs <- renderText({
    format(nrow(occ_species()), big.mark = ".")
  })

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

  output$kaartnaam <- renderText({
    paste(input$kaart)
  })
  
  output$soortenfiches_ui <- renderUI(
    tagList(
      div(
        card="card",
        style = "border: 10px solid #fff; border-radius: 8px; overflow: hidden; margin-bottom: 20px;",
        
        fluidRow(
          height = "600px", 
          column(
            width = 3,
            highchartOutput("piechart"),
            full_screen = TRUE
          ),
          column(
            width = 9,
            highchartOutput("barchart")
          )
        )
      ),
      
      if (input$kaart2 %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)")) {
        div(
          card="card",
          style = "border: 10px solid #fff; border-radius: 8px; overflow: hidden; margin-bottom: 20px;",
          
          fluidRow(
            class = "custom-kaart-row", 
            leafletOutput("kaart", height = "600px"),
            div(downloadButton("download_kaart", "Download PNG", class = "custom-download-button"))
          )
        )
      }
    )
  )
  
  output$piechart <- renderHighchart({
    x <- metrics2() %>%
      filter(is.na(code)) %>%
      pull(overlap)
      
    df <- data.frame(
      label = c("IN", "NIET IN"),
      gebied = c(paste("in", input$kaart2), paste("niet in", input$kaart2)),
      overlap = c(x, 1 - x),
      color = c("#c04384", "lightgrey"))

    data_list <- df %>%
      mutate(y = overlap * 100, name = gebied) %>%
      select(name, y, label, color) %>%
      list_parse()
    
    highchart() %>%
      hc_chart(type = "pie", width = NULL) %>%
      hc_plotOptions(pie = list(
        innerSize = '60%',
        dataLabels = list(
          enabled = FALSE,
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
      hc_size(height = NULL) %>%
      hc_chart(backgroundColor = 'rgba(0, 0, 0, 0)') %>%
      hc_add_theme(hc_theme_elementary()) %>%
      hc_legend(enabled = FALSE)
  })
  
  
  output$barchart <- renderHighchart({
    if (nrow(metrics2()) != 0) {
      df <- metrics2() %>%
        filter(!is.na(code) & overlap != 0) %>%
        arrange(overlap) %>%
        mutate(code = factor(code, levels = code)) %>%
        mutate(y = overlap * 100)
      
      chart <- highchart() %>%
        hc_chart(type = 'column') %>%
        hc_xAxis(categories = df$code, title = list(text = ""), labels = list(rotation = -90)) %>%
        hc_yAxis(title = list(text = 'Overlap (%)'), labels = list(format = '{value}%')) 
      
      if (nrow(metrics2()) == 1) {
        chart <- chart %>%
          hc_plotOptions(column = list(
            dataLabels = list(enabled = TRUE, format = '{point.y:.2f}%'),
            color = "#c04384",
            borderColor = "black",
            borderWidth = 0.5,
            pointPadding = 0.1, 
            groupPadding = 0.1
          ))
      }
      
      else {
        chart <- chart %>%
          hc_plotOptions(
            column = list(
              dataLabels = list(enabled = TRUE, format = '{point.y:.2f}%'),
              borderColor = "black",
              borderWidth = 0.5,
              pointPadding = 0.1, 
              groupPadding = 0.1
            ))
      }
      
      chart <- chart %>%
        hc_tooltip(
          headerFormat = '',
          pointFormat = '<b>{point.naam} ({point.code}): {point.y:.2f}%</b>'
        ) %>%
        hc_chart(backgroundColor = 'rgba(0, 0, 0, 0)') %>%
        hc_add_theme(hc_theme_elementary()) %>%
        hc_add_series(
          name = "Overlap",
          data = df %>% mutate(y = overlap * 100) %>% select(code, y, naam),
          colorByPoint = TRUE,
          colors = pal()(df$overlap)) %>%
        hc_legend(enabled = FALSE) 
      
      chart
    }
  })

  output$kaart <- renderLeaflet({
    
    if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)")) {
      
      metrics <- list_metrics[[input$kaart2]] %>%
        filter(soort == input$soort & type == "in" & !is.na(code))
      
      if (nrow(metrics) == 0) {
        return(NULL)
      }
      
      map <- leaflet() %>%
        addTiles(urlTemplate = "", attribution = NULL, group = "Zonder achtergrond") %>%
        addProviderTiles(providers$OpenStreetMap, group = "OSM (default)") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addPolygons(data = Provincies_grenzen, color = "black", fillColor = "#f0f0f0", weight = 0.5, group = "Provincies") %>%
        addPolygons(data = list_wfs[[input$kaart2]], color = "black", fillColor = "lightgrey", opacity = 0.7, weight = 0.5, fillOpacity = 1, label = ~paste0(naam, " (", code, "): 0%"), highlight = highlightOptions(stroke = TRUE, color = "black", weight = 2)) 
      
      if(nrow(metrics) != 0) {
        map <- map %>%
          addPolygons(data = metrics, color = "black", fillColor = ~pal()(overlap), opacity = 1, weight = 0.5, fillOpacity = 1, label = ~paste0(naam, " (", gebied, "): ", round(overlap * 100, 1), "%"), highlight = highlightOptions(stroke = TRUE, color = "black", weight = 2))
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
      
      if(nrow(metrics) != 0) {
        map <- map %>%
          addLegend(pal = pal(), values = metrics$overlap, opacity = 0.8, position = "bottomright")
      }
      
      map
      
    }
    else {
      return(NULL)
    }
  })

  output$download_kaart <- downloadHandler(
    filename = function()
      nameFile(soort = input$soort,
               type = "in",
               content = input$kaart, fileExt = "png"),
    content = function(file) {
      
      tmpFile <- tempfile(fileext = ".html")

      htmlwidgets::saveWidget(kaart_in(), file = tmpFile, selfcontained = FALSE)

      webshot::webshot(url = tmpFile, file = file,
                       vwidth = 1000, vheight = 500, cliprect = "viewport")
    }
  )
}

shinyApp(ui = ui, server = server)