# OPMERKING
# zie 5_suitability_climate_only.R

library(terra)
library(ggplot2)
library(scales)
library(tools)

# --- settings ---
prius2_dir <- "E:/risk-modelling-and-mapping/data/projects/PrIUS2"

out_dir <- file.path(prius2_dir, "output/distribution_maps/combined_climate_habitat")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pal <- c("#440154FF", "#3B528BFF", "#21918CFF", "#5DC963FF", "#FDE725FF")

max_cells_for_plot <- 2e6  # set to NULL to disable downsampling

# --- list species directories (one level under PrIUS2) ---
species_dirs <- list.dirs(
  prius2_dir,
  full.names = TRUE,
  recursive = FALSE
)

if (length(species_dirs) == 0) {
  stop("No species folders found under prius2_dir.")
}

# --- find tifs ONLY under Combined/Current/Predictions/Rasters ---
tif_files <- unlist(lapply(species_dirs, function(sp_dir) {
  
  combined_raster_dir <- file.path(
    sp_dir,
    "Combined", "Current", "Predictions", "Rasters"
  )
  
  if (!dir.exists(combined_raster_dir)) {
    return(character(0))
  }
  
  list.files(
    combined_raster_dir,
    pattern = "_Combined_current_ensemble\\.tif$",
    full.names = TRUE,
    recursive = FALSE
  )
}), use.names = FALSE)

if (length(tif_files) == 0) {
  stop("No files found matching '*_Combined_current_ensemble.tif' in Combined/Current/Predictions/Rasters.")
}

message("Found ", length(tif_files), " tif(s).")

# --- loop ---
for (tif_path in tif_files) {
  
  base_name <- file_path_sans_ext(basename(tif_path))
  GBIF_canonicalName <- sub("_Combined_current_ensemble$", "", base_name)
  
  png_path <- file.path(out_dir, paste0(GBIF_canonicalName, ".png"))
  
  message("Processing (overwrite): ", GBIF_canonicalName)
  
  tryCatch({
    
    r <- rast(tif_path)
    
    # optional downsampling for plotting speed
    if (!is.null(max_cells_for_plot) && ncell(r) > max_cells_for_plot) {
      fact <- ceiling(sqrt(ncell(r) / max_cells_for_plot))
      r <- aggregate(r, fact = fact, fun = mean, na.rm = TRUE)
    }
    
    # reproject to Belgian Lambert 72
    r_belge <- project(r, "EPSG:31370", method = "bilinear")
    
    # raster -> data.frame
    r_df <- as.data.frame(r_belge, xy = TRUE, na.rm = TRUE)
    names(r_df) <- c("x", "y", "value")
    
    p <- ggplot(r_df) +
      geom_raster(aes(x = x, y = y, fill = value)) +
      scale_fill_gradientn(
        colours = pal,
        limits = c(0, 1),
        oob = squish
      ) +
      coord_equal(expand = FALSE) +
      theme_void() +
      theme(legend.position = "none")
    
    ggsave(
      filename = png_path,
      plot = p,
      width = 15.0,
      height = 5.1,
      units = "cm",
      dpi = 200
    )
    
  }, error = function(e) {
    warning("Failed for: ", GBIF_canonicalName, " — ", conditionMessage(e))
  })
}

message("All done (Combined-only). Output: ", out_dir)
