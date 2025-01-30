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
# lu_sbp_pls <- readRDSfromURL("https://github.com/inbo/prius-radius/raw/dashboard/radius/data/spatial/lu_sbp_pls.rds")

list_wfs <- list("Habitatrichtlijngebieden (SBZ-H)" = ps_hbtrl_wgs84, "Vogelrichtlijngebieden (SBZ-V)" = ps_vglrl_wgs84, "Natura 2000 Habitattypes" = n2khab_wgs84, "Natuurbeheerplannen" = ps_nbhp_wgs84, "ANB patrimonium" = am_patdat_wgs84)

## Metric data
HBTRL <- ps_hbtrl_wgs84 %>%
  right_join(read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/HBTRL_long.csv"), by = c("code" = "gebied"), keep = TRUE)
VGLRL <- ps_vglrl_wgs84 %>%
  right_join(read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/VGLRL_long.csv"), by = c("code" = "gebied"), keep = TRUE)
N2KHAB <- n2khab_wgs84 %>%
  right_join(read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/N2KHAB_long.csv"), by = c("code" = "gebied"), keep = TRUE)
NBHP <- read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/NBHP_long.csv") %>%
  mutate(gebied = case_when(
    gebied == "Type1" ~ "Natuurbeheerplan Type 1",
    gebied == "Type2" ~ "Natuurbeheerplan Type 2",
    gebied == "Type3" ~ "Natuurbeheerplan Type 3",
    gebied == "Type4" ~ "Natuurbeheerplan Type 4",
    gebied == "BosbeU" ~ "Uitgebreid Bosbeheerplan",
    gebied == "MilDom" ~ "Beheerplan Militair domein",
    gebied == "VNatre" ~ "Vlaams Natuurreservaat",
    gebied == "BosreA" ~ "Aangewezen Bosreservaat",
    gebied == "HarmPG" ~ "Harmonisch Park- en Groenbeheerplan",
    gebied == "Natres" ~ "Erkend Natuurreservaat",
    gebied == "BosreE" ~ "Erkend Bosreservaat",
    gebied == "NatPer" ~ "Natuurlijke persoon",
    gebied == "PrivRec" ~ "Privaatrechtelijke rechtspersoon",
    gebied == "VOANB" ~ "Agentschap voor Natuur en Bos",
    gebied == "Bestuur" ~ "Bestuur",
    gebied == "VOander" ~ "Andere Vlaamse overheid",
    gebied == "Andere" ~ "Andere",
    gebied == "DomNa" ~ "Domein met natuurprotocol",
    TRUE ~ gebied
  ))

PATDAT <- read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/PATDAT_incl_ob_long.csv") %>%
  mutate(gebied = case_when(
    gebied == "Bosdec" ~ "Technisch beheer conform bosdecreet",
    gebied == "Eigend" ~ "Eigendom",
    gebied == "Huur" ~ "Huur",
    gebied == "BehOve" ~ "Beheerovereenkomst",
    gebied == "ProLan" ~ "Protocol landsverdediging",
    gebied == "Erfpac" ~ "Erfpacht",
    gebied == "AntKem" ~ "Antwerpse Kempen",
    gebied == "HKeVoe" ~ "Hoge Kempen tot Voeren",
    gebied == "GroGor" ~ "Groene Gordels",
    gebied == "BraWou" ~ "Brabantse Wouden",
    gebied == "DemZKe" ~ "Demerland & Zuiderkempen",
    gebied == "VArSch" ~ "Vlaamse Ardennen & Schelde-Leie",
    gebied == "KusWes" ~ "Kust & Westhoek",
    gebied == "Taxand" ~ "Taxandria",
    gebied == "LtHKem" ~ "Lage tot aan Hoge Kempen",
    gebied == "ZanVla" ~ "Zandig Vlaanderen",
    TRUE ~ gebied))

SBP_pgs <- read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/SBP_pgs_long.csv") %>%
  separate(gebied, into = c("gebied", "deelgebied"), sep = "\\s*\\(\\s*", fill = "right") %>%
  mutate(deelgebied = gsub("\\)$", "", deelgebied)) %>%
  mutate(gebied = gsub("\\s{2,}", " ", gebied)) %>%
  arrange(gebied)


SBP_pls <- read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/SBP_pls_long.csv") %>%
  mutate(deelgebied = NA) %>%
  filter(! soort %in% c("Amerikaanse stierkikker", "Chinese wolhandkrab", "gevlekte Amerikaanse rivierkreeft", "zwartbekgrondel", "gestreepte Amerikaanse rivierkreeft", "marmergrondel", "rode Amerikaanse rivierkreeft", "Kesslergrondel", "Turkse rivierkreeft", "Pontische stroomgrondel", "geknobbelde Amerikaanse rivierkreeft", "Californische rivierkreeft")) %>%
  mutate(gebied = gsub("\\s{2,}", " ", gebied))


SBP_pls_extra <- read.csv("https://raw.githubusercontent.com/inbo/prius-radius/main/radius/data/output/SBP_pls_extra.csv") %>%
  filter(!gebied %in% c("clusters") & !type %in% c("in_bek", "of_bek", "in_deelbek", "of_deelbek", "in_sbp_al", "in_sbp_ul", "of_sbp_al", "of_sbp_ul")) %>%
  mutate(deelgebied = NA) %>%
  mutate(
    deelgebied = case_when(
      type %in% c("in_clust", "of_clust") ~ gebied,
      TRUE ~ deelgebied  
    ),
    gebied = case_when(
      type %in% c("in_clust", "of_clust") ~ NA,
      type %in% c("in_sbp", "of_sbp") ~ gebied,
      TRUE ~ gebied  
    ),
    type = case_when(
      type %in% c("in_clust", "in_sbp") ~ "in",
      type %in% c("of_clust", "of_sbp") ~ "of",
      TRUE ~ type 
    ) 
  ) %>%
  mutate(deelgebied = gsub("\\)$", "", deelgebied)) %>%
  mutate(gebied = gsub("\\s{2,}", " ", gebied))


SBP <- rbind(SBP_pgs, SBP_pls, SBP_pls_extra)

list_metrics <- list("Habitatrichtlijngebieden (SBZ-H)" = HBTRL, "Vogelrichtlijngebieden (SBZ-V)" = VGLRL, "Natura 2000 Habitattypes" = N2KHAB, "Natuurbeheerplannen" = NBHP, "ANB patrimonium" = PATDAT, "Soortenbeschermingsprogramma's (SBP's)" = SBP)

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

# Custom CSS lay-out 
custom_css <- "

  .p {
    font-size: 12px;
    font-weight: normal;
    color: black;
  }
  
  .h1 {
    font-size: 16px;
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

  .custom-header {
      font-size: 18px;
      font-weight: bold;
      color: black;
  }
  
  .custom-header-row {
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
  
  .custom-header-row2 {
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
  font-size: 12px;
  }
  
  .main-content {
    flex: 1;
    padding-left: 5px;
    padding-right: 5px;
    margin-left: 5px;
    margin-right: 5px;
    overflow-y: auto;
  }
  
  custom-download-button {
      margin: 2px;
      border-color: #c04384;
      height: 30px; 
      padding: 5px 10px; 
  }
  
  custom-card {
      margin-bottom: 5px; 
      padding: 10px; 
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
  
  
  ## PAGINA 1 - SOORTENFICHES
  tabPanel(
    title = "Soortenfiches",
    div(class = "custom-container",
        
        ### Sidebar
        div(class = "custom-sidebar",
            selectizeInput(inputId = "soort", label = "Soort:", 
                           choices = sort(unique(occ_flanders$Soort)), selected = 1),
            selectInput('kaart', label = 'Kaarttype:', choices = names(list_metrics)),
            uiOutput("soortenfiches_input")
        ),
        
        ### Main content
        div(class = "main-content",
            
            #### Header
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
            
            #### Main
            uiOutput("soortenfiches_ui")
        )
    )
  ),
  
  ## PAGINA 2 - GEBIEDSFICHES
  tabPanel(
    title = "Gebiedsfiches",
    div(
      class = "custom-container",
      
      ### Sidebar
      div(
        class = "custom-sidebar",
        
        selectizeInput("kaart2", "Kaarttype:", choices = names(list_metrics)),
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
          class = "custom-header-row2",
          column(9, 
                 textOutput("gebiedsnaam") %>% tagAppendAttributes(class = "custom-header"),
                 textOutput("deelgebied") %>% tagAppendAttributes(class = "custom-subheader")),
          column(3, plotlyOutput("kaart_header", height = "100px"))
        ),
        
        #### Main 
        uiOutput("gebiedsfiches_ui")
      )
    )
  ),
  
  ## PAGINA 3 - OVER DASHBOARD
  tabPanel("Over",
           tags$head(
             tags$style("
      ul, ol {
        font-size: 12px;
        line-height: 1.4;
      }
      ul li, ol li {
        margin-bottom: 5px;
      }
    ")
           ),
           fluidRow(
             column(12,
                    h1("RadIUS project", class = "custom-header"),
                    p("Dit dashboard werd ontwikkeld als onderdeel van het RadIUS-project (Een prioritering voor het terreinbeheer), waarin het voorkomen van invasieve uitheemse soorten (IUS) in Vlaamse natuurterreinen werd geanalyseerd.", class = "p"),
                    p("De analyse omvat 79 invasieve soorten in Vlaanderen: de 46 soorten van de Unielijst (Verordening EU nr. 1143/2014) met minstens één waarneming sinds 2015, en een selectie van 33 andere IUS.", class = "p"),
                    p("Hun voorkomen werd geanalyseerd in diverse types natuurterreinen:", class = "p"),
                    tags$ul(
                      tags$li("Vogel- en Habitatrichtlijngebieden"),
                      tags$li("Natura 2000-habitattypes"),
                      tags$li("Terreinen met een natuurbeheerplan (natuurgebieden)"),
                      tags$li("Het ANB-patrimonium"),
                      tags$li("Soortenbeschermingsprogramma's (SBP's)")
                    ),
                    p("Dit dashboard bundelt alle resultaten en biedt een tool voor diverse actoren om zelf aan de slag te gaan met de data, voor het ontwikkelen van beheerprioriteiten voor specifieke gebieden of regio's, of het vinden van quick-wins voor het lokaal of gewestelijk IUS-beheer.", class = "p"),
                    p("Voor meer informatie over de achterliggende methode en resultaten, zie het ", 
                      a("RadIUS rapport", href = "https://doi.org/10.21436/inbor.116153244", target = "_blank"), ".", class = "p")
             )
           ),
           
           fluidRow(
             column(12,
                    h2("Analyse", class = "h1"),
                    p("Gekende puntlocaties werden omgezet in wolkvormige polygonen, waarna de overlap met de verschillende terreinen werd bepaald. Voor de omzetting werd een buffer toegepast. Een sensitiviteitsanalyse wees 100m aan als een geschikte radius voor de berekening van de rangschikking van soorten.", class = "p"),
                    p("De analyse beschouwt twee belangrijke aspecten:", class = "p"),
                    tags$ol(
                      tags$li("Bezetting: Het aandeel (percentage) van het oppervlak van gebied X dat door de soort wordt bedekt."),
                      tags$li("Gebondenheid: Het aandeel (percentage) van het areaal van de soort dat in gebied X valt.")
                    )
             )
           ),
           
           fluidRow(
             column(12,
                    h2("Dashboard", class = "h1"),
                    p("Het dashboard bundelt informatie in 'soortenfiches' per soort en 'gebiedsfiches' per type natuurterrein.", class = "p"),
                    p("Via de tab 'Soortenfiches' kun je voor alle 79 soorten het voorkomen (bezetting en gebondenheid) in de verschillende gebiedstypes raadplegen.", class = "p"),
                    p("Onder de tab 'Gebiedsfiches' wordt voor ieder van de natuurterreinen weergegeven welke soorten in welke mate voorkomen (bezetting en gebondenheid).", class = "p")
             )
           ),
           
           fluidRow(
             column(12,
                    h2("Data", class = "h1"),
                    p("Verspreidingsgegevens werden gedownload van ", 
                      a("GBIF", href = "https://www.gbif.org/", target = "_blank"), ". (laatste download 13 december 2024)", class = "p"),
                    p("Voor de analyse werden de volgende kaartlagen gebruikt (versie oktober 2024):", class = "p"),
                    tags$ul(
                      tags$li(a("Habitatrichtlijngebieden (SBZ-H)", href = "https://www.vlaanderen.be/datavindplaats/catalogus/habitatrichtlijndeelgebieden", target = "_blank")),
                      tags$li(a("Vogelrichtlijngebieden (SBZ-V)", href = "https://www.vlaanderen.be/datavindplaats/catalogus/vogelrichtlijngebieden", target = "_blank")),
                      tags$li(a("Natura 2000-habitattypes", href = "https://zenodo.org/records/10167695", target = "_blank")),
                      tags$li(a("Natuurbeheerplannen", href = "https://www.vlaanderen.be/datavindplaats/catalogus/natuurbeheerplannen", target = "_blank")),
                      tags$li(a("ANB-patrimonium", href = "https://www.vlaanderen.be/datavindplaats/catalogus/openbare-bossen-en-natuurdomeinen-beheerd-door-anb-in-naam-van-de-vlaamse-overheid", target = "_blank")),
                      tags$li(a("Soortenbeschermingsprogramma's", href = "https://www.vlaanderen.be/datavindplaats/catalogus/soortenbeschermingsprogrammas", target = "_blank"))
                    )
             )
           ),
           
           fluidRow(
             column(12,
                    h2("Contact", class = "h1"),
                    p("Voor vragen of problemen met het dashboard, neem contact op met ", 
                      a("Fleur Petersen", href = "mailto:fleur.petersen@inbo.be"), " of ", 
                      a("Bram D'hondt", href = "mailto:bram.dhondt@inbo.be"), ".", class = "p")
             )
           ),
           
           fluidRow(
             column(12,
                    p(paste("Laatste update van het dashboard: 7 januari 2025"), 
                      style = "text-align: right; margin-bottom: 1px; font-size: 11px; font-style: italic; color: #888;"),
                    p("*Let op: Deze datum betreft de laatste update van het dashboard zelf, niet van de onderliggende data.", 
                      style = "text-align: right; font-size: 10px; font-style: italic; color: #888;")
             )
           )
  ),
  nav_spacer()
)


########### SERVER ############
server <- function(input, output, session) {
  
  # PAGINA 1 - SOORTENFICHES
  
  ## REACTIEVE ELEMENTEN
  occ_species <- reactive({
    occ_flanders %>%
      filter(Soort == input$soort) 
  })

  metrics <- reactive({
    if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
      list_metrics[[input$kaart]] %>%
        st_drop_geometry() %>%
        filter(soort == input$soort & type %in% c("in", "of")) %>%
        left_join(species_list, by = c("soort" = "Soort"), relationship = "many-to-many") %>%
        select(soort, species, abbr, Groep, code, naam, type, overlap, EU_lijst) %>%
        distinct()
    }
    else if (input$kaart %in% c("Natuurbeheerplannen", "ANB patrimonium")) {
      list_metrics[[input$kaart]] %>%
        st_drop_geometry() %>%
        filter(soort == input$soort & type %in% c("in", "of")) %>%
        rename(code = gebied) %>%
        left_join(species_list, by = c("soort" = "Soort"), relationship = "many-to-many") %>%
        select(soort, species, Groep, code, type, overlap, EU_lijst) %>%
        distinct()
    }
    
    else if (input$kaart == "Soortenbeschermingsprogramma's (SBP's)") {
      if (input$sbp %in% c("SBP Beekprik", "SBP Kleine Modderkruiper", "SBP Rivierdonderpad")) {
        list_metrics[[input$kaart]] %>%
          st_drop_geometry() %>%
          filter(soort == input$soort & (gebied == input$sbp | is.na(gebied)) & type %in% c("in", "of")) %>%
          rename(code = deelgebied) %>%
          left_join(species_list, by = c("soort" = "Soort"), relationship = "many-to-many") %>%
          select(soort, species, Groep, code, type, overlap, EU_lijst) %>%
          distinct()
        
      } else {
        list_metrics[[input$kaart]] %>%
          st_drop_geometry() %>%
          filter(soort == input$soort & (gebied == input$sbp) & type %in% c("in", "of")) %>%
          rename(code = deelgebied) %>%
          left_join(species_list, by = c("soort" = "Soort"), relationship = "many-to-many") %>%
          select(soort, species, Groep, code, type, overlap, EU_lijst) %>%
          distinct()
      }
    }
  })

  occ_sum <- reactive({
    occ_species() %>%
      st_drop_geometry() %>%
      count(year, name = "n") %>%
      right_join(data.frame(year = 2015:2024), by = "year") %>%
      arrange(year) %>%
      na.replace(0)
  })

  pal_bezetting <- reactive({
    if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes", "Soortenbeschermingsprogramma's (SBP's)")) {
      df <- metrics() %>%
        filter(!is.na(code) & type == "of")
    }
    else if (input$kaart == "Natuurbeheerplannen") {
      df <- metrics() %>%
        filter(code != "NBHP" & type == "of")
    }
    else if (input$kaart == "ANB patrimonium") {
      df <- metrics() %>%
        filter(code != "PATDAT_incl_ob" & type == "of")
    }
    
    colors <- c("lightgrey", colorRampPalette(c("lightgrey", "#c04384"))(99))
    
    if (nrow(df) != 0) {
      colorNumeric(
        palette = colors,
        domain = c(min(df$overlap), max(df$overlap))
      )
    }
  })
  
  
  pal_gebondenheid <- reactive({
    if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes", "Soortenbeschermingsprogramma's (SBP's)")) {
      df <- metrics() %>%
        filter(!is.na(code) & type == "in")
    }
    else if (input$kaart == "Natuurbeheerplannen") {
      df <- metrics() %>%
        filter(code != "NBHP" & type == "in")
    }
    else if (input$kaart == "ANB patrimonium") {
      df <- metrics() %>%
        filter(code != "PATDAT_incl_ob" & type == "in")
    }
    
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
    if (input$kaart %in% c("Soortenbeschermingsprogramma's (SBP's)")) {
      
      data <- list_metrics[[input$kaart]]

      selectizeInput("sbp",
                     label = "Soortenbeschermingsprogramma:",
                     choices = c(na.omit(unique(data$gebied))))
    } else {
      NULL
    }
  })
  
  output$soort <- renderText({
    input$soort
  })

  output$species <- renderText({
    
    paste((species_list %>% filter(Soort == input$soort))$Species, collapse = ", ")
  })
  
  output$union_list <- renderUI({
    if (unique(metrics()$EU_lijst) == 1) {
      HTML('<span style="color: #c04384; font-weight: bold;"><i class="fas fa-bell"></i> Op unielijst (uitvoeringsverordening 2016/1141)</span>')
    } else if (unique(metrics()$EU_lijst) == 2) {
      HTML('<span style="color: #c04384; font-weight: bold;"><i class="fas fa-bell"></i> Op unielijst (uitvoeringsverordening 2017/1263)</span>')
    } else if (unique(metrics()$EU_lijst) == 3) {
      HTML('<span style="color: #c04384; font-weight: bold;"><i class="fas fa-bell"></i> Op unielijst (uitvoeringsverordening 2019/1262)</span>')
    } else if (unique(metrics()$EU_lijst) == 4) {
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
  
  output$soortenfiches_ui <- renderUI({
    
    explanation_text <- if (input$kaart == "Soortenbeschermingsprogramma's (SBP's)") {
      HTML(paste0(
        "<p style='font-size: 12px;'>Hieronder wordt voor ", input$soort, " het voorkomen weergegeven als:</p>
    <ol style='margin-top: 5px;'>
      <li><strong>Gebondenheid:</strong> het percentage van het totale areaal dat binnen gebieden van ", input$sbp, " valt.</li>
      <li><strong>Bezetting:</strong> percentage van totale oppervlak van gebieden van ", input$sbp, " in Vlaanderen dat bezet wordt door de soort.</li>
    </ol>"
      ))
    }
    else {
      HTML(paste0(
        "<p style='font-size: 12px;'>Hieronder wordt voor ", input$soort, " het voorkomen weergegeven als:</p>
    <ol style='margin-top: 5px;'>
      <li><strong>Gebondenheid:</strong> het percentage van het totale areaal dat binnen ", input$kaart, " valt.</li>
      <li><strong>Bezetting:</strong> percentage van totale oppervlak ", input$kaart, " in Vlaanderen dat bezet wordt door de soort.</li>
    </ol>"
      ))
    }
      
    
    text_gebondenheid <- if (input$kaart == "Soortenbeschermingsprogramma's (SBP's)") {
      if (input$sbp %in% c("SBP Kleine Modderkruiper", "SBP Rivierdonderpad", "SBP Beekprik")) {
        if (input$soort %in% c("Amerikaanse stierkikker", "Chinese wolhandkrab", "gevlekte Amerikaanse rivierkreeft", "zwartbekgrondel", "gestreepte Amerikaanse rivierkreeft", "marmergrondel", "rode Amerikaanse rivierkreeft", "Kesslergrondel", "Turkse rivierkreeft", "Pontische stroomgrondel", "geknobbelde Amerikaanse rivierkreeft", "Californische rivierkreeft")) {
          HTML(paste0("<p style='font-size: 12px;'> Gebondenheid weergegeven over alle gebieden afgebakend binnen ", input$sbp, " (links) en per watercluster (rechts).</p>"))
        } else {
          HTML(paste0("<p style='font-size: 12px;'> Gebondenheid weergegeven over alle gebieden afgebakend binnen ", input$sbp, ".</p>"))
        }
      } else {
        HTML(paste0("<p style='font-size: 12px;'> Gebondenheid weergegeven over alle gebieden afgebakend binnen ", input$sbp, " (links) en per afzonderlijk gebied (rechts).</p>"))
      }
    }
    else {
      HTML(paste0("<p style='font-size: 12px;'> Gebondenheid weergegeven over alle ", input$kaart, " (links) en per ",
                  ifelse(input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)"), 
                         "afzonderlijk gebied (rechts).</p>", 
                         ifelse(input$kaart == "Natura 2000 Habitattypes", 
                                "habitattype (rechts).</p>", 
                                ifelse(input$kaart == "Natuurbeheerplannen", 
                                       "eigendomstype (rechts).</p>", 
                                       ifelse(input$kaart == "ANB patrimonium", 
                                              "beheerregio (rechts).</p>", 
                                              "afzonderlijk gebied (rechts).</p>")
                                )
                         )
                  )
      ))
    }
    
    text_bezetting <- if (input$kaart == "Soortenbeschermingsprogramma's (SBP's)") {
      if (input$sbp %in% c("SBP Kleine Modderkruiper", "SBP Rivierdonderpad", "SBP Beekprik")) {
        if (input$soort %in% c("Amerikaanse stierkikker", "Chinese wolhandkrab", "gevlekte Amerikaanse rivierkreeft", "zwartbekgrondel", "gestreepte Amerikaanse rivierkreeft", "marmergrondel", "rode Amerikaanse rivierkreeft", "Kesslergrondel", "Turkse rivierkreeft", "Pontische stroomgrondel", "geknobbelde Amerikaanse rivierkreeft", "Californische rivierkreeft")) {
          HTML(paste0("<p style='font-size: 12px;'> Bezetting weergegeven over alle gebieden afgebakend binnen ", input$sbp, " (links) en per watercluster (rechts).</p>"))
        } else {
          HTML(paste0("<p style='font-size: 12px;'> Bezetting weergegeven over alle gebieden afgebakend binnen ", input$sbp, ".</p>"))
        }
      } else {
        HTML(paste0("<p style='font-size: 12px;'> Bezetting weergegeven over alle gebieden afgebakend binnen ", input$sbp, " (links) en per afzonderlijk gebied (rechts).</p>"))
      }
    }
    else {
      HTML(paste0("<p style='font-size: 12px;'> Bezetting weergegeven over alle ", input$kaart, " (links) en per ",
                  ifelse(input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)"), 
                         "afzonderlijk gebied (rechts).</p>", 
                         ifelse(input$kaart == "Natura 2000 Habitattypes", 
                                "habitattype (rechts).</p>", 
                                ifelse(input$kaart == "Natuurbeheerplannen", 
                                       "eigendomstype (rechts).</p>", 
                                       ifelse(input$kaart == "ANB patrimonium", 
                                              "beheerregio (rechts).</p>", 
                                              "afzonderlijk gebied (rechts).</p>")
                                )
                         )
                  )
      ))
    }
    
    
    if (nrow(metrics() %>% filter(type == "of" & overlap != 0)) == 0) {
      div(class = "custom-value-box", 
          height = "50px",
          style = "padding: 10px; background-color: #f8f9fa; border: 1px solid #ccc; border-radius: 5px;",
          paste0(input$soort, " komt niet voor in ", ifelse(input$kaart == "Soortenbeschermingsprogramma's (SBP's)", input$sbp, input$kaart))
      )
    }
    else {
      
      tagList(
        div(
          card = "card",
          style = "border: 10px solid #fff; border-radius: 8px; overflow: hidden; margin-bottom: 20px;",
          p(explanation_text),
          
          navset_underline(
            
            nav_panel(
              title = "Gebondenheid",
              p(text_gebondenheid),
              fluidRow(
                height = "500px", 
                column(
                  width = 3,
                  highchartOutput("piechart_gebondenheid", height = "500px"),
                  full_screen = TRUE
                ),
                column(
                  width = 9,
                  highchartOutput("barchart_gebondenheid", height = "500px")
                )
              ),
              
              if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)")) {
                fluidRow(
                  class="custom-kaart-row", 
                  div(style = "height: 20px;"),  
                  leafletOutput("kaart", height="500px"),
                )
              }
            ),
            nav_panel(
              title = "Bezetting",  
              p(text_bezetting),
              fluidRow(
                height = "500px", 
                column(
                  width = 3,
                  highchartOutput("piechart_bezetting", height = "500px"),
                  full_screen = TRUE
                ),
                column(
                  width = 9,
                  highchartOutput("barchart_bezetting", height = "500px")
                )
              )
            )
          )
        )
      )}
  })
  
  output$piechart_gebondenheid <- renderHighchart({
    
    req(input$soort, input$kaart)
    
    if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes", "Soortenbeschermingsprogramma's (SBP's)")) {
      x <- metrics() %>%
        filter(is.na(code) & type == "in") %>%
        pull(overlap)
    }
    else if (input$kaart == "Natuurbeheerplannen") {
      x <- metrics() %>%
        filter(code == "NBHP" & type == "in") %>%
        pull(overlap)
    }
    else if (input$kaart == "ANB patrimonium") {
      x <- metrics() %>%
        filter(code == "PATDAT_incl_ob" & type == "in") %>%
        pull(overlap)
    }
    
    print(x)
    
    if (input$kaart == "Soortenbeschermingsprogramma's (SBP's)") {
      df <- data.frame(
        gebied = c(paste("in", input$sbp), paste("buiten", input$sbp)),
        overlap = c(x, 1 - x),
        color = c("#c04384", "lightgrey"))
    } else {
      df <- data.frame(
        gebied = c(paste("in", input$kaart), paste("buiten", input$kaart)),
        overlap = c(x, 1 - x),
        color = c("#c04384", "lightgrey"))
    }
    
    print(df)
    
    data_list <- df %>%
      mutate(y = overlap * 100, name = gebied) %>%
      select(name, y, color) %>%
      list_parse()
    
    print(data_list)
    
    highchart() %>%
      hc_chart(type = "pie", width = NULL) %>%
      hc_plotOptions(pie = list(
        innerSize = '60%',
        dataLabels = list(
          enabled = FALSE,
          distance = -30,
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
          pointFormat = '<b>{point.y:.2f}</b> %', 
          style = list(color = "black", fontsize = '14px', fontWeight = 'bold')
        )
      ) %>%
      hc_size(height = NULL) %>%
      hc_chart(backgroundColor = 'rgba(0, 0, 0, 0)') %>%
      hc_add_theme(hc_theme_elementary()) %>%
      hc_legend(enabled = FALSE)
  })
  
  
  output$barchart_gebondenheid <- renderHighchart({
    req(input$soort, input$kaart)
    
    if (nrow(metrics()) != 0) {
      if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes", "Soortenbeschermingsprogramma's (SBP's)")) {
    
        df <- metrics() %>%
          filter(!is.na(code) & type == "in") %>%
          mutate(code = factor(code, levels = code)) %>%
          mutate(y = overlap * 100) %>%
          arrange(desc(overlap))
        
      }
      
      if (input$kaart == "Natuurbeheerplannen") {
        df <- metrics() %>%
          filter(code != "NBHP" & type == "in") %>%
          mutate(code = factor(code, levels = code)) %>%
          mutate(y = overlap * 100) %>%
          arrange(desc(overlap))
      }
      else if (input$kaart == "ANB patrimonium") {
        df <- metrics() %>%
          filter(code != "PATDAT_incl_ob" & type == "in") %>%
          mutate(code = factor(code, levels = code)) %>%
          mutate(y = overlap * 100) %>%
          arrange(desc(overlap))
      }
      
      if (input$kaart == "Soortenbeschermingsprogramma's (SBP's)") {
        if (input$sbp %in% c("SBP Kleine Modderkruiper", "SBP Beekprik", "SBP Rivierdonderpad") & !input$soort %in% c("Amerikaanse stierkikker", "Chinese wolhandkrab", "gevlekte Amerikaanse rivierkreeft", "zwartbekgrondel", "gestreepte Amerikaanse rivierkreeft", "marmergrondel", "rode Amerikaanse rivierkreeft", "Kesslergrondel", "Turkse rivierkreeft", "Pontische stroomgrondel", "geknobbelde Amerikaanse rivierkreeft", "Californische rivierkreeft")) {
          NULL
        } else {
          chart <- highchart() %>%
            hc_chart(type = 'bar', height = 500) %>%
            hc_xAxis(categories = df$code, title = list(text = ""), labels = list(fontSize = "8px", step = 1)) %>%
            hc_yAxis(title = list(text = 'Overlap (%)'), labels = list(format = '{value}%')) %>%
            hc_plotOptions(
              bar = list(  
                dataLabels = list(enabled = TRUE, format = '{point.y:.2f}%'), 
                borderColor = "black",
                borderWidth = 0.2,
                pointPadding = 0.1, 
                groupPadding = 0
              )
            ) %>%
            hc_tooltip(
              headerFormat = '',
              pointFormat = tooltip_format <- if(input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
                '<b>{point.naam} ({point.category}): {point.y:.2f}%</b>'
              } else {
                '<b>{point.category}: {point.y:.2f}%</b>'
              },
              style = list(color = "black", fontsize = '14px', fontWeight = 'bold')
            ) %>%
            hc_chart(backgroundColor = 'rgba(0, 0, 0, 0)') %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_add_series(
              name = "Overlap (%)",
              data = if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
                df %>% select(y, naam) %>% list_parse()
              } else {
                df$y
              },
              colorByPoint = TRUE,
              colors = pal_gebondenheid()(df$overlap)
            ) %>%
            hc_legend(enabled = FALSE)
          
          chart
        }
      } 
      else {
        chart <- highchart() %>%
          hc_chart(type = 'bar', height = 500) %>%
          hc_xAxis(categories = df$code, title = list(text = ""), labels = list(fontSize = "8px", step = 1)) %>%
          hc_yAxis(title = list(text = 'Overlap (%)'), labels = list(format = '{value}%')) %>%
          hc_plotOptions(
            bar = list(  
              dataLabels = list(enabled = TRUE, format = '{point.y:.2f}%'), 
              borderColor = "black",
              borderWidth = 0.2,
              pointPadding = 0.1, 
              groupPadding = 0
            )
          ) %>%
          hc_tooltip(
            headerFormat = '',
            pointFormat = tooltip_format <- if(input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
              '<b>{point.naam} ({point.category}): {point.y:.2f}%</b>'
            } else {
              '<b>{point.category}: {point.y:.2f}%</b>'
            },
            style = list(color = "black", fontsize = '14px', fontWeight = 'bold')
          ) %>%
          hc_chart(backgroundColor = 'rgba(0, 0, 0, 0)') %>%
          hc_add_theme(hc_theme_elementary()) %>%
          hc_add_series(
            name = "Overlap (%)",
            data = if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
              df %>% select(y, naam) %>% list_parse()
            } else {
              df$y
            },
            colorByPoint = TRUE,
            colors = pal_gebondenheid()(df$overlap)
          ) %>%
          hc_legend(enabled = FALSE)
        
        chart
      }
    }
  })
  
  output$piechart_bezetting <- renderHighchart({
    req(input$soort, input$kaart)
    
    if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes", "Soortenbeschermingsprogramma's (SBP's)")) {
      x <- metrics() %>%
        filter(is.na(code) & type == "of") %>%
        pull(overlap)
    }
    else if (input$kaart == "Natuurbeheerplannen") {
      x <- metrics() %>%
        filter(code == "NBHP" & type == "of") %>%
        pull(overlap)
    }
    else if (input$kaart == "ANB patrimonium") {
      x <- metrics() %>%
        filter(code == "PATDAT_incl_ob" & type == "of") %>%
        pull(overlap)
    }
    
    df <- data.frame(
      gebied = c(paste("in", input$kaart), paste("buiten", input$kaart)),
      overlap = c(x, 1 - x),
      color = c("#c04384", "lightgrey"))
    
    data_list <- df %>%
      mutate(y = overlap * 100, name = gebied) %>%
      select(name, y, color) %>%
      list_parse()
    
    highchart() %>%
      hc_chart(type = "pie", width = NULL) %>%
      hc_plotOptions(pie = list(
        innerSize = '60%',
        dataLabels = list(
          enabled = FALSE,
          distance = -30,
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
          pointFormat = '<b>{point.y:.2f}</b> %', 
          style = list(color = "black", fontsize = '14px', fontWeight = 'bold')
        )
      ) %>%
      hc_size(height = NULL) %>%
      hc_chart(backgroundColor = 'rgba(0, 0, 0, 0)') %>%
      hc_add_theme(hc_theme_elementary()) %>%
      hc_legend(enabled = FALSE)
  })
  
  
  output$barchart_bezetting <- renderHighchart({
    req(input$soort, input$kaart)
    
    if (nrow(metrics()) != 0) {
      if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes", "Soortenbeschermingsprogramma's (SBP's)")) {
        df <- metrics() %>%
          filter(!is.na(code) & type == "of") %>%
          mutate(code = factor(code, levels = code)) %>%
          mutate(y = overlap * 100) %>%
          arrange(desc(overlap))
      }
      
      if (input$kaart == "Natuurbeheerplannen") {
        df <- metrics() %>%
          filter(code != "NBHP" & type == "of") %>%
          mutate(code = factor(code, levels = code)) %>%
          mutate(y = overlap * 100) %>%
          arrange(desc(overlap))
      }
      else if (input$kaart == "ANB patrimonium") {
        df <- metrics() %>%
          filter(code != "PATDAT_incl_ob" & type == "of") %>%
          mutate(code = factor(code, levels = code)) %>%
          mutate(y = overlap * 100) %>%
          arrange(desc(overlap))
      }
      
      if (input$kaart == "Soortenbeschermingsprogramma's (SBP's)") {
        if (input$sbp %in% c("SBP Kleine Modderkruiper", "SBP Beekprik", "SBP Rivierdonderpad") & !input$soort %in% c("Amerikaanse stierkikker", "Chinese wolhandkrab", "gevlekte Amerikaanse rivierkreeft", "zwartbekgrondel", "gestreepte Amerikaanse rivierkreeft", "marmergrondel", "rode Amerikaanse rivierkreeft", "Kesslergrondel", "Turkse rivierkreeft", "Pontische stroomgrondel", "geknobbelde Amerikaanse rivierkreeft", "Californische rivierkreeft")) {
          NULL
        } else {
          chart <- highchart() %>%
            hc_chart(type = 'bar', height = 500) %>%
            hc_xAxis(categories = df$code, title = list(text = ""), labels = list(fontSize = "8px", step = 1)) %>%
            hc_yAxis(title = list(text = 'Overlap (%)'), labels = list(format = '{value}%')) %>%
            hc_plotOptions(
              bar = list(  
                dataLabels = list(enabled = TRUE, format = '{point.y:.2f}%'), 
                borderColor = "black",
                borderWidth = 0.2,
                pointPadding = 0.1, 
                groupPadding = 0
              )
            ) %>%
            hc_tooltip(
              headerFormat = '',
              pointFormat = tooltip_format <- if(input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
                '<b>{point.naam} ({point.category}): {point.y:.2f}%</b>'
              } else {
                '<b>{point.category}: {point.y:.2f}%</b>'
              },
              style = list(color = "black", fontsize = '14px', fontWeight = 'bold')
            ) %>%
            hc_chart(backgroundColor = 'rgba(0, 0, 0, 0)') %>%
            hc_add_theme(hc_theme_elementary()) %>%
            hc_add_series(
              name = "Overlap (%)",
              data = if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
                df %>% select(y, naam) %>% list_parse()
              } else {
                df$y
              },
              colorByPoint = TRUE,
              colors = pal_bezetting()(df$overlap)
            ) %>%
            hc_legend(enabled = FALSE)
          
          chart
        }
      } 
      else {
        chart <- highchart() %>%
          hc_chart(type = 'bar', height = 500) %>%
          hc_xAxis(categories = df$code, title = list(text = ""), labels = list(fontSize = "8px", step = 1)) %>%
          hc_yAxis(title = list(text = 'Overlap (%)'), labels = list(format = '{value}%')) %>%
          hc_plotOptions(
            bar = list(  
              dataLabels = list(enabled = TRUE, format = '{point.y:.2f}%'), 
              borderColor = "black",
              borderWidth = 0.2,
              pointPadding = 0.1, 
              groupPadding = 0
            )
          ) %>%
          hc_tooltip(
            headerFormat = '',
            pointFormat = tooltip_format <- if(input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
              '<b>{point.naam} ({point.category}): {point.y:.2f}%</b>'
            } else {
              '<b>{point.category}: {point.y:.2f}%</b>'
            },
            style = list(color = "black", fontsize = '14px', fontWeight = 'bold')
          ) %>%
          hc_chart(backgroundColor = 'rgba(0, 0, 0, 0)') %>%
          hc_add_theme(hc_theme_elementary()) %>%
          hc_add_series(
            name = "Overlap (%)",
            data = if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
              df %>% select(y, naam) %>% list_parse()
            } else {
              df$y
            },
            colorByPoint = TRUE,
            colors = pal_bezetting()(df$overlap)
          ) %>%
          hc_legend(enabled = FALSE)
        
        chart
      }
    }
  })

  output$kaart <- renderLeaflet({
    req(input$soort, input$kaart)
    
    if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)")) {
      
      metrics <- list_metrics[[input$kaart]] %>%
        filter(soort == input$soort & type == "in" & !is.na(code))
      
      if (nrow(metrics) == 0) {
        return(NULL)
      }
      
      map <- leaflet(options = leafletOptions(minZoom = 8)) %>%
        addTiles(urlTemplate = "", attribution = NULL, group = "Zonder achtergrond") %>%
        addProviderTiles(providers$OpenStreetMap, group = "OSM (default)") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addPolygons(data = Provincies_grenzen, color = "black", fillColor = "#f0f0f0", weight = 0.5, group = "Provincies") %>%
        addPolygons(data = list_wfs[[input$kaart]], color = "black", fillColor = "lightgrey", opacity = 0.7, weight = 0.5, fillOpacity = 0, label = ~paste0(naam, " (", code, "): 0%"), highlight = highlightOptions(stroke = TRUE, color = "black", weight = 2)) 
      
      if(nrow(metrics) != 0) {
        map <- map %>%
          addPolygons(data = metrics, color = "black", fillColor = ~pal_gebondenheid()(overlap), opacity = 1, weight = 0.5, fillOpacity = 0.7, label = ~paste0(naam, " (", gebied, "): ", round(overlap * 100, 1), "%"), highlight = highlightOptions(stroke = TRUE, color = "black", weight = 2))
      }
      
      map <- map %>%
        addCircleMarkers(data = occ_species(), radius = 3, color = "blue", fillOpacity = 0.7, weight = 0.5, label = ~paste0(day, "/", month, "/", year), group = "Waarnemingen") %>%
        addLayersControl(
          baseGroups = c("Geen achtergrond", "OSM (default)", "Satellite"),
          overlayGroups = c("Waarnemingen"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup("Waarnemingen") %>%
        fitBounds(lng1 = 2.54, lat1 = 50.68, lng2 = 5.92, lat2 = 51.51)  # Grenzen van Vlaanderen
      
      if(nrow(metrics) != 0) {
        map <- map %>%
          addLegend(pal = pal_gebondenheid(), values = metrics$overlap, opacity = 0.8, position = "bottomright")
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

      htmlwidgets::saveWidget(kaart(), file = tmpFile, selfcontained = FALSE)

      webshot::webshot(url = tmpFile, file = file,
                       vwidth = 1000, vheight = 500, cliprect = "viewport")
    }
  )
  
  # PAGINA 2 - GEBIEDSFICHES
  
  ## REACTIEVE ELEMENTEN
  metrics2 <- reactive({
    data <- list_metrics[[input$kaart2]] %>%
      st_drop_geometry() %>%
      left_join(species_list, by = c("soort" = "Soort"), relationship = "many-to-many") 
    
    if (input$kaart2 %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)")) {
      data <- data %>%
        filter(type %in% c("in", "of")) %>%
        select(soort, species, abbr, Groep, code, naam, type, overlap, EU_lijst) %>%
        distinct()
    }
    else if (input$kaart2 == "Natura 2000 Habitattypes") {
      data <- data %>% 
        filter(type  %in% c("in", "of")) %>%
        left_join(habitats, by = c("code", "naam" = "habitat")) %>%
        select(soort, species, abbr, Groep, code, naam, groep, type, overlap, EU_lijst) %>%
        distinct()
    } 
    else if (input$kaart2 == "Natuurbeheerplannen") {
      data <- data %>%
        filter(!type %in% c("sel")) %>%
        transmute(soort, species, abbr, Groep, code = gebied, type, overlap, EU_lijst) %>%
        distinct()
    }
    else if (input$kaart2 == "ANB patrimonium") {
      data <- data %>%
        filter(!type %in% c("sel")) %>%
        transmute(soort, species, abbr, Groep, code = gebied, type, overlap, EU_lijst) %>%
        distinct()
    }
    else if (input$kaart2 == "Soortenbeschermingsprogramma's (SBP's)"){
      
      if (input$sbp2 %in% c("SBP Beekprik", "SBP Kleine Modderkruiper", "SBP Rivierdonderpad")) {
        list_metrics[[input$kaart2]] %>%
          st_drop_geometry() %>%
          filter((gebied == input$sbp2 | is.na(gebied)) & type %in% c("in", "of")) %>%
          transmute(soort, species, abbr, gebied, code = deelgebied, type, overlap, EU_lijst) %>%
          distinct()
        
      } else {
        list_metrics[[input$kaart2]] %>%
          st_drop_geometry() %>%
          filter((gebied == input$sbp2) & type %in% c("in", "of")) %>%
          transmute(soort, species, abbr, gebied, code = deelgebied, type, overlap, EU_lijst) %>%
          distinct()
      }
    }
    
    filtered_data <- switch(input$soort_selectie,
                            "all" = data,
                            "unielijst" = data %>% filter(EU_lijst != 0),
                            "niet_unielijst" = data %>% filter(EU_lijst == 0)
    )
    
    return(filtered_data)
  })
  
  metrics_nspec_per_gebied <- reactive({
    if (input$kaart2 %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)")) {
      df <- metrics2() %>% 
        filter(!is.na(code) & type == "of" & overlap != 0) %>%
        group_by(code, naam) %>%
        summarise(y = n()) %>%
        arrange(desc(y))
    }
    else if (input$kaart2 == "Natura 2000 Habitattypes") {
      df <- metrics2() %>% 
        filter(!is.na(code) & type == "of" & overlap != 0) %>%
        group_by(code, naam) %>%
        summarise(y = n()) %>%
        arrange(desc(y)) %>% 
        left_join(habitats, by = c("code")) %>%
        select(groep, code, naam, y)
    }
    
    else if (input$kaart2 == "Soortenbeschermingsprogramma's (SBP's)") {
      df <- metrics2() %>%
        filter(!is.na(code) & type == "of" & overlap != 0) %>%
        group_by(code) %>%
        summarise(y = n()) %>%
        arrange(desc(y))
    }
    
    else {
      df <- metrics2() %>%
        filter(type == "of" & overlap != 0)
    }
  })
  
  ## OUTPUTS
  
  output$gebiedsnaam <- renderText({
    input$kaart2
  })
  
  
  output$gebiedsfiches_input <- renderUI({
    if (input$kaart2 == "Soortenbeschermingsprogramma's (SBP's)") {
      tagList(
        selectizeInput("sbp2", 
                       label = "Soortenbeschermingsprogramma:", 
                       choices = na.omit(unique(list_metrics[[input$kaart2]]$gebied)),
                       selected = NULL),
        # uiOutput("sbp_deelgebied_ui"),
        # selectizeInput("selected_species",
        #                label = "Selecteer soorten:",
        #                choices = sort(unique(species_list$Soort)),
        #                multiple = TRUE,
        #                options = list(placeholder = 'Selecteer een of meerdere soorten'))
      )
    } else if (input$kaart2 %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
      data <- list_metrics[[input$kaart2]]
      
      df <- data.frame(naam = data$naam, code = data$code, stringsAsFactors = FALSE)
      
      df <- na.omit(df)
      df <- df[order(df$naam), ]
      
      choices <- setNames(df$code, paste(df$naam, "(", df$code, ")", sep = " "))
      
      selectizeInput("deelgebied2", 
                     label = "Deelgebied:", 
                     choices = c("All" = "All", choices))
    } else if (input$kaart2 == "ANB patrimonium") {
      selectizeInput("deelgebied2", 
                     label = "Beheerregio:", 
                     choices = c("All" = "All", unique(na.omit(am_patdat_wgs84$code))))
    } else {
      NULL 
    }
  })
  
  output$deelgebied <- renderText({
    
    req(input$kaart2, input$deelgebied2)
    
    if (input$kaart2 %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes") & input$deelgebied2 != "All") {
      list_metrics[[input$kaart2]] %>%
        filter(code == input$deelgebied2) %>%
        st_drop_geometry() -> data
      
      paste0(unique(data$naam), " (", unique(data$code), ")")
    }
    else if (input$kaart2 == "ANB patrimonium" & input$deelgebied2 != "All") {
      input$deelgebied2
    }
    else if (input$kaart2 == "Soortenbeschermingsprogramma's (SBP's)") {
      input$sbp2
    }
    else {
      NULL
    }
  })
  
  output$gebiedsfiches_ui <- renderUI({
    req(input$kaart2, input$deelgebied2)
    
      is_all_deelgebieden <- input$deelgebied2 == "All" || input$kaart2 %in% c("Natuurbeheerplannen")
      
      explanation_text_bezetting <- if (is_all_deelgebieden) {
        HTML(paste0(
          "<p style='font-size: 12px;'>Hieronder wordt voor alle aanwezige soorten, het voorkomen weergegeven als:</p>
    <ol style='margin-top: 5px;'>
      <li><strong>Bezetting:</strong> percentage van totale oppervlak ", input$kaart2, " in Vlaanderen dat bezet wordt door de soort</li>
      <li><strong>Aantal Gebieden:</strong> aantal ", ifelse(input$kaart2 == "Habitatrichtlijngebieden (SBZ-H)", "Habitatrichtlijngebieden",
                                   ifelse(input$kaart2 == "Vogelrichtlijngebieden (SBZ-V)", "Vogelrichtlijngebieden", 
                                          ifelse(input$kaart2 == "Natura 2000 Habitattypes", "habitattypes", 
                                                 ifelse(input$kaart2 == "ANB patrimonium", "ANB-domeinen", 
                                                        ifelse(input$kaart2 == "Natuurbeheerplannen", "Natuurbeheerplannen", 
                                                               "gebieden"))))), " waarin een soort voorkomt</li>
      <li><strong>Gebondenheid:</strong> het percentage van het totale areaal dat binnen ", input$kaart2, " valt</li>
    </ol>"
        ))
      } else if (input$kaart2 == "ANB patrimonium") {
        HTML(paste0(
          "<p>Hieronder wordt voor alle aanwezige soorten, het voorkomen weergegeven als:</p>
    <ol style='margin-top: 20px;'>
      <li><strong>Bezetting:</strong> percentage van totale oppervlak ", input$deelgebied2, " in Vlaanderen dat bezet wordt door de soort</li>
      <li><strong>Gebondenheid:</strong> het percentage van het totale areaal dat binnen ", input$deelgebied2, " valt</li>
    </ol>"
        ))
      } else if (input$kaart2 == "Soortenbeschermingsprogramma's (SBP's)") {
        HTML(paste0(
          "<p>Hieronder wordt voor alle aanwezige soorten, het voorkomen weergegeven als:</p>
    <ol style='margin-top: 20px;'>
      <li><strong>Bezetting:</strong> percentage van totale oppervlak ", input$sbp2, " in Vlaanderen dat bezet wordt door de soort</li>
      <li><strong>Gebondenheid:</strong> het percentage van het totale areaal dat binnen ", input$sbp2, " valt</li>
    </ol>"
        ))
      } else {
        HTML(paste0(
          "<p>Hieronder wordt voor alle aanwezige soorten, het voorkomen weergegeven als:</p>
    <ol style='margin-top: 20px;'>
      <li><strong>Bezetting:</strong> percentage van totale oppervlak ", input$deelgebied2, " in Vlaanderen dat bezet wordt door de soort</li>
      <li><strong>Gebondenheid:</strong> het percentage van het totale areaal dat binnen ", input$deelgebied2, " valt</li>
    </ol>"
        ))
      }
      
      
      explanation_text_verspreiding <- "De onderstaande barplot toont het aantal soorten per deelgebied."
      
      tagList(
        div(
          style = "margin-bottom: 20px; margin-left: 10px;",
          h3("Soorten", style = "margin-top: 15px; font-size: 18px; font-weight: bold;"),
          p(explanation_text_bezetting),
          
          # Tabs
          navset_underline(
            id = "tabset_bezetting",
            nav_panel(title = "Bezetting", value = "percentage"),
            if (is_all_deelgebieden) {
              nav_panel(title = "Aantal Gebieden", value = "aantal_gebieden")
            },
            nav_panel(title = "Gebondenheid", value = "gebondenheid")
          )
        ),
        
        div(
          style = "padding: 5px;",
          
          # 'bezetting' tab
          conditionalPanel(
            condition = "input.tabset_bezetting == 'percentage'",
            fluidRow(
              height = "600px",
              highchartOutput("percentage_oppervlak", height = "600px")
            )
          ),
          
          # 'aantal gebieden' tab
          conditionalPanel(
            condition = "input.tabset_bezetting == 'aantal_gebieden'",
            fluidRow(
              height = "600px",
              highchartOutput("aantal_gebieden", height = "600px")
            )
          ),
          
          # 'gebondenheid' tab
          conditionalPanel(
            condition = "input.tabset_bezetting == 'gebondenheid'",
            fluidRow(
              height = "600px",
              highchartOutput("gebondenheid", height = "600px")
            )
          ),
          
          # # Download buttons below the figures
          # fluidRow(
          #   conditionalPanel(
          #     condition = "input.tabset_bezetting == 'percentage'",
          #     column(6, 
          #            actionButton("download_data_bezetting", 
          #                         label = "Download Data", 
          #                         icon = icon("download"), 
          #                         style = "font-size: 12px; background-color: transparent; border: none; color: black;"),
          #            actionButton("download_png_bezetting", 
          #                         label = "Download Figuur", 
          #                         icon = icon("file-image"), 
          #                         style = "font-size: 12px; background-color: transparent; border: none; color: black;")
          #     )
          #   ),
          #   
          #   conditionalPanel(
          #     condition = "input.tabset_bezetting == 'aantal_gebieden'",
          #     column(6,
          #            actionButton("download_data_aantal_gebieden", 
          #                         label = "Download Data", 
          #                         icon = icon("download"), 
          #                         style = "font-size: 12px; background-color: transparent; border: none; color: black;"),
          #            actionButton("download_png_aantal_gebieden", 
          #                         label = "Download Figuur", 
          #                         icon = icon("file-image"), 
          #                         style = "font-size: 12px; background-color: transparent; border: none; color: black;")
          #     )
          #   ),
          #   conditionalPanel(
          #     condition = "input.tabset_bezetting == 'gebondenheid'",
          #     column(6, 
          #            actionButton("download_data_gebondenheid", 
          #                         label = "Download Data", 
          #                         icon = icon("download"), 
          #                         style = "font-size: 12px; background-color: transparent; border: none; color: black;"),
          #            actionButton("download_png_gebondenheid", 
          #                         label = "Download Figuur", 
          #                         icon = icon("file-image"), 
          #                         style = "font-size: 12px; background-color: transparent; border: none; color: black;")
          #     )
          #   )
          # )
        ),
        
        # Barplot for Verspreiding
        if (input$kaart2 %in% c("Habitatrichtlijngebieden (SBZ-H)", 
                                "Vogelrichtlijngebieden (SBZ-V)", 
                                "Natura 2000 Habitattypes", 
                                "Soortenbeschermingsprogramma's (SBP's)") && 
            is_all_deelgebieden) {
          div(
            style = "margin-bottom: 20px;",
            
            h3("Verspreiding", style = "margin-top: 15px; font-size: 18px; font-weight: bold;"),
            
            p(explanation_text_verspreiding, 
              style = "border-radius: 0px; font-size: 12px; margin: 2px; line-height: 1.4;"),
            
            fluidRow(
              height = "600px",
              highchartOutput("barplot_verspreiding", height = "600px")
            ),
            
            # fluidRow(
            #   column(6,
            #          actionButton("download_data_percentage_oppervlak", 
            #                       label = "Download Data", 
            #                       icon = icon("download"), 
            #                       style = "font-size: 12px;background-color: transparent;border:none;color:black"),
            #          actionButton("download_png_verspreiding", 
            #                       label = "Download Figuur", 
            #                       icon = icon("file-image"), 
            #                       style = "font-size: 12px;background-color: transparent;border:none;coloexplanar:black")
            #   )
            # )
          )
        }
      )
  })
  
  
  output$download_data_bezetting <- downloadHandler(
    filename = function() {
      paste("data_bezetting_", input$kaart2, "_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(metrics2(), file, row.names = FALSE)
    }
  )
  
  output$download_data_percentage_oppervlak <- downloadHandler(
    filename = function() {
      paste("data_aantal_gebieden_", input$kaart2, "_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(metrics2(), file, row.names = FALSE)
    }
  )
  
  # Download PNG images based on current tab
  output$download_png_bezetting <- downloadHandler(
    filename = function() {
      paste("png_bezetting_", input$kaart2, "_", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      webshot::webshot(output$percentage_oppervlak, file = file)
    }
  )
  
  output$download_png_aantal_gebieden <- downloadHandler(
    filename = function() {
      paste("png_aantal_gebieden_", input$kaart2, "_", Sys.Date(), ".png", sep="")
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
  
  output$kaart_header <- renderPlotly({
    
    req(input$kaart2, input$deelgebied2)
    
    if (input$kaart2 %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)")) {
      if (input$deelgebied2 == "All") {
        plot_ly() %>%
          add_sf(
            data = list_wfs[[input$kaart2]],
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
        
        selected_area <- list_wfs[[input$kaart2]] %>% filter(code == input$deelgebied2)
        bbox <- st_bbox(selected_area)
        
        plot_ly() %>%
          add_sf(
            data = list_wfs[[input$kaart2]],
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
    }
    else {
      return()
    }
  })
  
  output$percentage_oppervlak <- renderHighchart({
    
    if (input$kaart2 %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
      if (input$deelgebied2 == "All") {
        data <- metrics2() %>%
          filter(is.na(code) & type == "of")
      } 
      else {
        data <- metrics2() %>%
          filter(code == input$deelgebied2 & type == "of")
      }
    }
    else if (input$kaart2 == "Natuurbeheerplannen") {
      data <- metrics2() %>%
        filter(type == "of" & code == "NBHP")
    }
    else if (input$kaart2 == "ANB patrimonium") {
      if (input$deelgebied2 == "All") {
        data <- metrics2() %>%
          filter(type == "of" & code == "PATDAT_incl_ob")
      } 
      else {
        data <- metrics2() %>%
          filter(code == input$deelgebied2 & type == "of")
      }
    }
    else if (input$kaart2 == "Soortenbeschermingsprogramma's (SBP's)") {
      data <- metrics2() %>%
        filter(is.na(code) & type == "of")
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
          color = "#ecc7da"
        ) %>%
        hc_legend(enabled = FALSE)
      
      chart
    }
  })
  
  # pal_aantal2 <- reactive({
  #   if (input$kaart2 %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes", "Soortenbeschermingsprogramma's (SBP's)")) {
  #     df <- metrics2() %>%
  #       filter(!is.na(code) & overlap != 0 & type == "of") %>%
  #       group_by(soort) %>%
  #       summarise(overlap = n()) 
  #   }
  #   else if (input$kaart2 == "Natuurbeheerplannen") {
  #     df <- metrics2() %>%
  #       filter(type == "n_nbhp")
  #   }
  #   else if (input$kaart2 == "ANB patrimonium") {
  #     df <- metrics2() %>%
  #       filter(type == "n_domeinen")
  #   }
  #   
  #   colors <- c("lightgrey", colorRampPalette(c("lightgrey", "#c04384"))(99))
  #   
  #   if (nrow(df) != 0) {
  #     colorNumeric(
  #       palette = colors,
  #       domain = c(min(df$overlap), max(df$overlap))
  #     )
  #   }
  # })
  
  output$aantal_gebieden <- renderHighchart({
    
    if (input$kaart2 == "Natuurbeheerplannen") {
      df <- metrics2() %>%
        filter(type == "n_nbhp") %>%
        transmute(soort, overlap, y = overlap) %>%
        arrange(desc(y))
    }
    else if (input$kaart2 == "ANB patrimonium") {
      df <- metrics2() %>%
        filter(type == "n_domeinen") %>%
        transmute(soort, overlap, y = overlap) %>%
        arrange(desc(y))
    }
    else if (input$kaart2 == "Soortenbeschermingsprogramma's (SBP's)") {
      df <- metrics2() %>%
        filter(!is.na(code) & type == "of" & overlap != 0) %>%
        group_by(soort) %>%
        summarise(y = n()) %>%
        arrange(desc(y)) 
    }
    else {
      df <- metrics2() %>%
        filter(!is.na(code) & type == "of" & overlap != 0) %>%
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
          color = "#ecc7da"
        ) %>%
        hc_legend(enabled = FALSE)
      
      chart
    }
  })
  
  output$gebondenheid <- renderHighchart({
    
    if (input$kaart2 %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
      if (input$deelgebied2 == "All") {
        data <- metrics2() %>%
          filter(is.na(code) & type == "in")
      } 
      else {
        data <- metrics2() %>%
          filter(code == input$deelgebied2 & type == "in")
      }
    }
    else if (input$kaart2 == "Natuurbeheerplannen") {
      data <- metrics2() %>%
        filter(type == "in" & code == "NBHP")
    }
    else if (input$kaart2 == "ANB patrimonium") {
      if (input$deelgebied2 == "All") {
        data <- metrics2() %>%
          filter(type == "in" & code == "PATDAT_incl_ob")
      } 
      else {
        data <- metrics2() %>%
          filter(code == input$deelgebied2 & type == "in")
      }
    }
    else if (input$kaart2 == "Soortenbeschermingsprogramma's (SBP's)") {
      data <- metrics2() %>%
        filter(is.na(code) & type == "in")
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
          color = "#ecc7da"
        ) %>%
        hc_legend(enabled = FALSE)
      
      chart
    }
  })
  
  # pal_verspreiding2 <- reactive({
  #   
  #   df <- metrics_nspec_per_gebied()
  #   
  #   colors <- c("lightgrey", colorRampPalette(c("lightgrey", "#c04384"))(99))
  #   
  #   if (nrow(df) != 0) {
  #     colorNumeric(
  #       palette = colors,
  #       domain = c(min(df$y), max(df$y))
  #     )
  #   }
  # })
  
  output$barplot_verspreiding <- renderHighchart({
    df <- metrics_nspec_per_gebied()
    
    if (nrow(df) != 0) {
      
      # if(input$kaart2 == "Natura 2000 Habitattypes") {
      #   df <- df %>%
      #     arrange(groep, desc(y))
      #   
      #   chart <- highchart(df, type = "bar", hcaes(x = code, y = y, group = groep))
      # } else {
      #   chart <- highchart(df, type = "bar", hcaes(x = code, y = y))
      # }
      
      chart <- highchart() %>%
        hc_chart(type = 'bar', height = 500) %>%
        hc_xAxis(categories = df$code, title = list(text = ""), labels = list(rotation = -0, fontSize = "8px", step = 1)) %>%
        hc_yAxis(title = list(text = '# soorten'), labels = list(format = '{value}'), min = 0) %>%  
        hc_plotOptions(
          bar = list(
            dataLabels = list(enabled = TRUE, format = '{point.y:.0f}'), 
            borderColor = "black",
            borderWidth = 0.2,
            pointPadding = 0.1, 
            groupPadding = 0
          )
        ) %>%
        hc_tooltip(
          headerFormat = '',
          pointFormat = '<b>{point.category}: {point.y:.0f}</b>',
          style = list(color = "black", fontsize = '14px', fontWeight = 'bold')
        ) %>%
        hc_add_theme(hc_theme_elementary()) %>%
        hc_add_series(
          name = "Overlap (%)",
          data = if (input$kaart %in% c("Habitatrichtlijngebieden (SBZ-H)", "Vogelrichtlijngebieden (SBZ-V)", "Natura 2000 Habitattypes")) {
            df %>% select(y, naam) %>% list_parse()
          } else {
            df$y
          },
          color = "#ecc7da"
        ) %>%
        hc_legend(enabled = FALSE)
      
      chart
    }
  })
}

shinyApp(ui = ui, server = server)