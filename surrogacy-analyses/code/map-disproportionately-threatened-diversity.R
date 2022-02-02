#' ---
#' title: Global Reptile Assessment Paper <br> <strong> Disproportionately Threatened Diversity Comparison (Figure 2B, D)
#' ---
#' 
#' ## Set things up
#' ### Load libraries
library(tidyverse)
library(raster)
library(colorist)
#' ### Custom functions
#' ### Function to compare relative threat among classes using library(colorist)
get_comparison_map <- function(div_stack){
  #' ### Only keep areas where at least one class has more than 10% threatened
  div_stack <- div_stack/100
  div_stack_metrics <- metrics_distill(div_stack)
  div_stack_ratio <- sum(div_stack)
  div_stack_ratio[] <- apply(div_stack[], 1, function(r){
    r <- r %>% sort(decreasing = TRUE)
    r[1]/r[2]
  })
  div_hotspots <- div_stack_metrics$layer_id * (div_stack_metrics$intensity >= .1) * (div_stack_ratio >= 2)
  # vert_threatened_pd_hotspots[!(vert_threatened_pd_hotspots %in% 1:4) & (!is.na(vert_threatened_pd_stack_raw[["Mammals"]]))] <- 0
  # div_hotspots[] <- factor(div_hotspots[], levels = c(0:4 %>% as.character()))
  projection(div_hotspots) <- CRS("+proj=igh +datum=WGS84 +ellps=WGS84 +units=m +no_defs")
  col_pal <- c("#D9D9D975", paste0(c("#528be1", "#b87750", "#50aa65", "#666760"), "FF"))
  tm_shape(div_hotspots) + 
    tm_raster(palette = col_pal, title = "", labels = c("NA", "Amphibians", "Birds", "Mammals", "Reptiles")) +
    tm_layout(frame = FALSE, legend.show = TRUE, legend.position = c("left", "bottom"))
  
  return(div_hotspots)
}
#'
#' ## Disproportionately threatened species richness (50km resolution - Figure 2B)
#' 
#' ### Load data
#' #### Species richness (overall)
#' ##### Read rasters
species_richness_overall <- list.files("data/SR_rasters50", pattern = "overall", full.names = TRUE) %>% 
  purrr::map(function(f){
    r <- readRDS(f) # read serialized raster object
    projection(r) <- CRS("+proj=igh +datum=WGS84 +ellps=WGS84 +units=m +no_defs") # add projection
    r
    })
#' ##### Stack rasters
species_richness_overall <- species_richness_overall %>% purrr::map(function(r) extend(r, extent(Reduce(extend, species_richness_overall)))) %>% 
  set_names(c("Amphibians", "Birds", "Mammals", "Reptiles")) %>% 
  raster::stack()
#' #### Species richness (threatened)
#' ##### Read rasters
species_richness_threatened <- list.files("data/SR_rasters50", pattern = "threatened", full.names = TRUE) %>% 
  purrr::map(function(f){
    r <- readRDS(f) # read serialized raster object
    projection(r) <- CRS("+proj=igh +datum=WGS84 +ellps=WGS84 +units=m +no_defs") # add projection
    r
  })
#' ##### Stack rasters
species_richness_threatened <- species_richness_threatened %>% purrr::map(function(r) extend(r, extent(Reduce(extend, species_richness_threatened)))) %>% 
  set_names(c("Amphibians", "Birds", "Mammals", "Reptiles")) %>% 
  raster::stack()
#'
#' ### Calculate relative threat for species richness
species_richness_relative_threat <- species_richness_threatened/species_richness_overall
species_richness_relative_threat <- species_relative_threat * (sum(species_richness_overall) >= 5) # Remove areas with less than 5 combined species across the four taxa
#'
#' ### Generate map
species_richness_relative_threat_raster <- get_comparison_map(species_richness_relative_threat)
tm_shape(species_richness_relative_threat_raster) + tm_raster()
#'
#' ## Disproportionately threatened phylogenetic diversity (50km resolution - Figure 2D)
#' ### Load data
phylogenetic_diversity_relative_threat <- list.files("data/PD_rasters50/", pattern = "pd_relative_threat", full.names = TRUE) %>% 
  purrr::map(function(f){
    r <- readRDS(f) # read serialized raster object
    projection(r) <- CRS("+proj=igh +datum=WGS84 +ellps=WGS84 +units=m +no_defs") # add projection
    r
  }) %>% 
  set_names(c("Amphibians", "Birds", "Mammals", "Reptiles")) %>% 
  raster::stack()
#' ### Generate map
phylogenetic_diversity_relative_threat_raster <- get_comparison_map(phylogenetic_diversity_relative_threat)
tm_shape(species_richness_relative_threat_raster) + tm_raster()

