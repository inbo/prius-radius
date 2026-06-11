#---------------------------------------------------------
#---------------------Load EEA grid-----------------------
#---------------------------------------------------------

gpkg_path <- here::here("prius2", "spatial", "utm1_bel_regions.geojson")
eea_grid_regions <- sf::st_read(gpkg_path)


fistools::download_gdrive_if_missing(gfileID = "1YgGWuKbXs-JANeNENgg39V7DXKZYMrgS",
                                     destfile = "prius2/input/prius2_class_cube.csv",
                                     update_always = TRUE,
                                     email = Sys.getenv("email"))

#---------------------------------------------------------
#--------Load and filter classcube data-------------------
#---------------------------------------------------------
classcube <- read_csv(
  file = here::here("prius2",  "input", "prius2_class_cube.csv"),
  col_types = cols(
    year = col_double(),
    eeacellcode = col_character(),
    classkey = col_double(),
    occurrences = col_double(),
    mincoordinateuncertaintyinmeters = col_double()
  ),
  na = ""
)%>%
  tidylog::rename(eea_cell_code = eeacellcode,
                  classKey = classkey,
                  cobs = occurrences) %>%
  tidylog::select(-mincoordinateuncertaintyinmeters)%>% #Remove columns we don't need
  tidylog::filter(!is.na(eea_cell_code))%>% #Remove data not assigned to any cell code
  tidylog::filter(!is.na(classKey))  %>%
  tidylog::filter(year >= 2000)%>%
  tidylog::left_join(eea_grid_regions,
                     by = c("eea_cell_code" = "CELLCODE"))%>%
  tidylog::filter(isFlanders)

gc()


df_cc <- 
  classcube %>%
  tidylog::group_by(classKey) %>%
  tidylog::distinct(eea_cell_code) %>%
  tidylog::ungroup()

df_begin_year <- 
  classcube %>%
  tidylog::group_by(classKey) %>%
  tidylog::summarize(begin_year = min(year))

df_cc <- 
  df_cc %>%
  tidylog::left_join(df_begin_year, by = "classKey") %>%
  tidylog::select(classKey, begin_year, eea_cell_code)

make_time_series <- function(eea_cell_code, classKey, begin_year, last_year ) {
  expand_grid(eea_cell_code = eea_cell_code,
              classKey = classKey,
              year = seq(from = begin_year, to = last_year))
  
}

# create timeseries slots
df_ts <- purrr::pmap_dfr(df_cc, 
                         .f = make_time_series, 
                         last_year = year(Sys.Date())
)


# Add for each classkey in each year and in each cell the number of observations (obs)
df_ts <- 
  df_ts %>%
  tidylog::left_join(
    classcube %>% tidylog::select(classKey,
                           year,
                           eea_cell_code,
                           cobs), 
    by = c("classKey", "year", "eea_cell_code"))

#Add for each cell information on whether it belongs to Flanders and to specific N2000 areas
df_ts <-
  df_ts%>%
  tidylog::left_join( 
    eea_grid_regions %>% tidylog::select(CELLCODE,
                                         isFlanders,
                                         isFlandersSAC,
                                         isFlandersSPA,
                                         isFlandersN2000,
                                         geometry), 
    by = c("eea_cell_code" = "CELLCODE"))


df_ts <-
  df_ts %>%
  tidylog::replace_na(list(cobs = 0))

df_ts <- 
  df_ts %>%
  tidylog::mutate(pa_cobs = if_else(cobs > 0, 1, 0)) %>%
  tidylog::relocate(classKey, 
                    year, 
                    eea_cell_code, 
                    cobs, 
                    pa_cobs,
                    classKey,
                    isFlanders,
                    isFlandersSAC,
                    isFlandersSPA,
                    isFlandersN2000)


filter_compact_time_series <- function(df, col_to_filter) {
  df %>%
    tidylog::filter(!!sym(col_to_filter) == TRUE) %>%
    tidylog::group_by(classKey, year) %>%
    tidylog::summarise(
      cobs = sum(cobs),
      c_ncells = sum(pa_cobs)
    ) %>%
    tidylog::ungroup()
}

colnames_to_filter <- c("isFlanders", "isFlandersN2000")
gc()
df_ts_compact_N2000<-filter_compact_time_series(df = df_ts, col_to_filter = "isFlandersN2000")
df_ts_compact_Flanders<-filter_compact_time_series(df = df_ts, col_to_filter = "isFlanders")


ggplot()+
  geom_bar(data=df_ts_compact_N2000, aes(x=year, y=cobs), stat ="identity")+
  geom_vline(xintercept = 2018, linetype = "dashed", color = "red") +
  facet_wrap(~classKey, scales="free")+
  theme_bw()

ggplot()+
  geom_bar(data=df_ts_compact_N2000, aes(x=year, y=c_ncells), stat ="identity")+
  geom_vline(xintercept = 2018, linetype = "dashed", color = "red") +
  facet_wrap(~classKey, scales="free")+
  theme_bw()
