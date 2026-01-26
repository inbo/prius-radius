oldlist<- read.csv(file.path("prius2", "temp", "prius2_list_v25-09-23.csv"))

newlist <- oldlist %>%
  dplyr::mutate(GBIF_canonicalName = case_when(GBIF_canonicalName == "Amynthas tokioensis" ~ "Perichaeta tokioensis",
                                               GBIF_canonicalName == "Brachyponera chinensis" ~ "Pachycondyla chinensis",
                                               GBIF_canonicalName == "Metaphire hilgendorfi" ~ "Perichaeta hilgendorfi",
                                               GBIF_canonicalName == "Misgurnus bipartitus" ~ "Misgurnus mohoity",
                                               GBIF_canonicalName == "Nanozostera japonica" ~ "Zostera japonica",
                                               GBIF_canonicalName == "Egeria densa" ~ "Elodea densa",
                                               TRUE ~ GBIF_canonicalName ),
                GBIF_scientificName= case_when(GBIF_canonicalName == "Perichaeta tokioensis" ~ "Perichaeta tokioensis Beddard, 1892",
                                               GBIF_canonicalName == "Pachycondyla chinensis" ~ "Pachycondyla chinensis (Emery, 1895)",
                                               GBIF_canonicalName == "Perichaeta hilgendorfi" ~ "Perichaeta hilgendorfi Michaelsen, 1892",
                                               GBIF_canonicalName == "Misgurnus mohoity" ~ "Misgurnus mohoity (Dybowski, 1869)",
                                               GBIF_canonicalName == "Zostera japonica" ~ "Zostera japonica Asch. & Graebn.",
                                               GBIF_canonicalName == "Elodea densa" ~ "Elodea densa (Planch.) Casp.",
                                               TRUE ~ GBIF_scientificName ),
                GBIF_code= case_when(GBIF_canonicalName == "Perichaeta tokioensis" ~ 9711540,
                                     GBIF_canonicalName == "Pachycondyla chinensis" ~ 1319870,
                                     GBIF_canonicalName == "Perichaeta hilgendorfi" ~ 9565801,
                                     GBIF_canonicalName =="Misgurnus mohoity" ~ 2367911,
                                     GBIF_canonicalName == "Zostera japonica" ~ 2863943,
                                     GBIF_canonicalName == "Elodea densa" ~ 5329263,
                                     TRUE ~ GBIF_code ))

write.csv(newlist, file.path("prius2", "temp", "prius2_list_v26-01-16.csv"))
