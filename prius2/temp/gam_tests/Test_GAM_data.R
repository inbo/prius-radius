#--------------------------------------------
#-----------------Load packages--------------
#--------------------------------------------
packages <- c("rgbif", "dplyr", "purrr", "assertthat", "readr", "here", "retry", "CoordinateCleaner", "remotes", "stringr", "ggplot2")

installed_packages <- installed.packages() |>
  as.data.frame()

for (package in packages) {
  print(package)
  if (!package %in% rownames(installed.packages()) ) { 
    install.packages( package ) 
  }
  library(package, character.only = TRUE)
}


#--------------------------------------------
#-----------Source helper functions----------
#--------------------------------------------
source("./prius2/scripts/helper_functions.R")


#--------------------------------------------
#-----------Retrieve GBIF taxonkeys----------
#--------------------------------------------
accepted_taxonkeys<-read.csv(file.path("prius2", "temp", "gam_tests", "accepted_taxonkeys.csv"))%>%
  pull(x)


#--------------------------------------------
#-----------Define download settings---------
#--------------------------------------------
#All basis of record types, except `FOSSIL SPECIMEN` and `LIVING SPECIMEN`, which can have misleading location information (e.g. location of captive animal).
basis_of_record <- c(
  "OBSERVATION", 
  "HUMAN_OBSERVATION",
  "MATERIAL_SAMPLE",
  "PRESERVED_SPECIMEN", 
  "UNKNOWN", 
  "MACHINE_OBSERVATION",
  "OCCURRENCE"
)

#Time period
year_begin <- 2000
year_end <- 2025

#Only georeferenced points
hasCoordinate <- TRUE

#Define Belgium bbox with a buffer around
belgium_bbox <- sf::st_bbox(c(xmin = 2.1, xmax = 6.9, ymin = 49.2, ymax = 51.7), crs = 4326)%>%
  sf::st_as_sfc()%>%
  sf::st_as_text()


#--------------------------------------------
#---------------Perform download-------------
#--------------------------------------------
gbif_user  <- Sys.getenv("GBIF_USER",   unset = NA)
gbif_pwd   <- Sys.getenv("GBIF_PWD",    unset = NA)
gbif_email <- Sys.getenv("GBIF_EMAIL",  unset = NA)

gbif_download_key <-  rgbif::occ_download(
  pred_within(belgium_bbox),    
  pred_in("taxonKey", accepted_taxonkeys),
  pred_in("basisOfRecord", basis_of_record),
  pred_gte("year", year_begin),
  pred_lte("year", year_end),
  pred("hasCoordinate", hasCoordinate),
  pred("occurrenceStatus", "PRESENT"),
  pred("hasGeospatialIssue", FALSE), #Remove default geospatial issues
  user  =  gbif_user,
  pwd   = gbif_pwd,
  email = gbif_email,
  curlopts = list(http_version = 2)
)


rgbif::occ_download_wait(gbif_download_key)#Check download status


#--------------------------------------------
#--------------Retrieve download-------------
#--------------------------------------------
test_folder<-file.path("prius2", "temp", "gam_tests")
gbif_download_key<-"0008224-260226173443078"
rgbif::occ_download_get(gbif_download_key, path = test_folder, overwrite = TRUE)
metadata <- rgbif::occ_download_meta(key = gbif_download_key)
gbif_download_key <- metadata$key


#extract_GBIF_occurrence
data.path <- file.path(test_folder, gbif_download_key)
unzip(paste0(data.path,".zip"),exdir = data.path, overwrite = TRUE)
global <- as.data.frame(data.table::fread(paste0(data.path,"/occurrence.txt"),
                                          select=c("acceptedTaxonKey","acceptedScientificName", "decimalLatitude", "decimalLongitude", "kingdom", "phylum", "class","order", "genus", "coordinateUncertaintyInMeters", "identificationVerificationStatus", "year", "datasetKey"),
                                          header = TRUE))



#--------------------------------------------
#-------- Filter global occurrences----------
#--------------------------------------------
#remove unverified records
identificationVerificationStatus_to_discard <- c( "unverified",
                                                  "unvalidated",
                                                  "not validated",
                                                  "under validation",
                                                  "not able to validate",
                                                  "control could not be conclusive due to insufficient knowledge",
                                                  "1",
                                                  "uncertain",
                                                  "unconfirmed",
                                                  "douteux",
                                                  "invalide",
                                                  "non r\u00E9alisable",
                                                  "verification needed" ,
                                                  "probable",
                                                  "unconfirmed - not reviewed",
                                                  "validation requested",
                                                  "unconfirmed - plausible")
#Left: 1.937.098 records
global.occ<-global %>%
  dplyr::filter(acceptedTaxonKey%in%accepted_taxonkeys) %>%   
  dplyr::filter(!str_to_lower(identificationVerificationStatus) %in% identificationVerificationStatus_to_discard)

world<-rnaturalearth::ne_countries(scale=50)
ggplot()+
geom_sf(data = world,  colour = "black", fill = NA)+
geom_point(data=global.occ, aes(decimalLongitude, decimalLatitude), color="blue")+
labs(x="Longitude", y="Latitude")+
theme_bw()

#Load EEA data 
gpkg_path <- here::here("prius2", "spatial", "utm1_bel_regions.geojson")
eea_grid_regions <- sf::st_read(gpkg_path)
flemish_grid<-filter(eea_grid_regions, isFlandersN2000==TRUE)

global.occ<-sf::st_as_sf(global.occ, coords=c("decimalLongitude", "decimalLatitude"), crs=4326)%>%
  sf::st_transform( sf::st_crs(flemish_grid) )

occ_flanders<-sf::st_intersection(global.occ, flemish_grid) #1196084; 545356 in N2000 areas

#Check occurrences
ggplot()+
  geom_sf(data = flemish_grid,  colour = "black", fill = NA)+
  geom_sf(data=occ_flanders,  color="blue")+
  labs(x="Longitude", y="Latitude")+
  theme_bw()

rm(global.occ, global)
gc()


#---------------------------------------------------
#----------------Create plots-----------------------
#---------------------------------------------------
#Create a histogram of coordinate uncertainty of PrIUS2 species in Flanders
occ_flanders %>%
  filter(coordinateUncertaintyInMeters <= 10000) %>%
  ggplot(aes(coordinateUncertaintyInMeters)) +
  geom_histogram(
    binwidth = 200,
    boundary = 0,  # <-- ensures bins start at 0
    color = "black",
    fill = "#b6effc"
  ) +
  stat_bin(
    binwidth = 200,
    boundary = 0,  # <-- match the histogram bins
    geom = 'text',
    color = 'black',
    aes(label = after_stat(count)),
    position = position_stack(vjust = 1.1)
  ) +
  scale_x_continuous(breaks = seq(0, 10000, by = 1000)) +
  labs(
    x = "Coordinate uncertainty (meters)",
    y = "Number of records"
  ) +
  theme_bw()

#---------------------------------------------------
#----------------Download class data in Flanders-----------------------
#---------------------------------------------------
prius_classkeys <- read_csv(file = here::here("prius2",  "input", "prius2_class_cube.csv"))%>%
  pull(classkey)%>%
  unique()

gbif_download_key <-  rgbif::occ_download(
  pred_within(belgium_bbox),    
  pred_in("taxonKey", prius_classkeys),
  pred_in("basisOfRecord", basis_of_record),
  pred_gte("year", year_begin),
  pred_lte("year", year_end),
  pred("hasCoordinate", hasCoordinate),
  pred("occurrenceStatus", "PRESENT"),
  pred("hasGeospatialIssue", FALSE), #Remove default geospatial issues
  user  =  gbif_user,
  pwd   = gbif_pwd,
  email = gbif_email,
  curlopts = list(http_version = 2)
)

rgbif::occ_download_wait(gbif_download_key)#Check download status


#--------------------------------------------
#--------------Retrieve download-------------
#--------------------------------------------
gbif_download_key<-"0010381-260226173443078"
rgbif::occ_download_get(gbif_download_key, path = test_folder, overwrite = TRUE)
metadata <- rgbif::occ_download_meta(key = gbif_download_key)
gbif_download_key <- metadata$key


#extract_GBIF_occurrence
data.path <- file.path(test_folder, gbif_download_key)
unzip(paste0(data.path,".zip"),exdir = data.path, overwrite = TRUE)
library(arrow)
library(dplyr)
library(stringr)

# Open large file lazily (does NOT load into memory)
occ_ds <- open_dataset(
  file.path(data.path, "occurrence.txt"),
  format = "csv",
  parse_options = csv_parse_options(
    delimiter = "\t"
  )
)

identificationVerificationStatus_to_discard <- c(
  "unverified","unvalidated","not validated","under validation",
  "not able to validate",
  "control could not be conclusive due to insufficient knowledge",
  "1","uncertain","unconfirmed","douteux","invalide",
  "non réalisable","verification needed","probable",
  "unconfirmed - not reviewed","validation requested",
  "unconfirmed - plausible"
)

# Lazy filtering
filtered_occ <- occ_ds %>%
  select(
    acceptedTaxonKey, acceptedScientificName,
    decimalLatitude, decimalLongitude,
    kingdom, phylum, class, classKey, order, genus,
    coordinateUncertaintyInMeters,
    identificationVerificationStatus,
    year, datasetKey
  ) %>%
  filter(classKey %in% prius_classkeys) %>%
  filter(
    !tolower(identificationVerificationStatus) %in%
      identificationVerificationStatus_to_discard
  )

write_dataset(
  filtered_occ,
  "filtered_occ_parquet",
  format = "parquet"
)


