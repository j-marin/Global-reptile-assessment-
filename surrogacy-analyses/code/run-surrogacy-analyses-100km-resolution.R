#' ---
#' title: Global Reptile Assessment <br> <strong> Surrogacy in Conservation For Threatened Species - 100km resolution analyses
#' ---
#' 
#' ## Set things up
#' ### Create new input raster file directories
dir.create("data/input_rasters")
#' #### Create directories for 100km resolution rasters
dir.create("data/input_rasters/100")
dir.create("data/input_rasters/100/reptiles")
dir.create("data/input_rasters/100/birds")
dir.create("data/input_rasters/100/mammals")
dir.create("data/input_rasters/100/amphibians")
dir.create("data/input_rasters/100/combined")
#'
#' ## Load cell-by-species matrices
threatened_reptiles_pa_matrix100 <- readRDS("data/threatened_amphibians_pa_matrix100.rds")
threatened_birds_pa_matrix100 <- readRDS("data/threatened_birds_pa_matrix100.rds")
threatened_mammals_pa_matrix100 <- readRDS("data/threatened_mammals_pa_matrix100.rds")
threatened_amphibians_pa_matrix100 <- readRDS("data/threatened_amphibians_pa_matrix100.rds")
#'
#' ## Generate rasters from cell-by-species matrices
#' ### Reptiles
purrr::map(names(threatened_reptiles_pa_matrix100)[3:4], function(sp){
  r <- rasterFromXYZ(threatened_reptiles_pa_matrix100 %>% dplyr::select(x, y, sp))
  proj4string(r) <- "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  writeRaster(r, filename = paste0("data/input_rasters/100/reptiles/", sp, ".tif"), format = "GTiff")
})
#' ### Birds
purrr::map(names(threatened_birds_pa_matrix100)[-c(1:2)], function(sp){
  r <- rasterFromXYZ(threatened_birds_pa_matrix100 %>% dplyr::select(x, y, sp))
  proj4string(r) <- "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  writeRaster(r, filename = paste0("data/input_rasters/100/birds/", sp, ".tif"), format = "GTiff")
})
#' ### Mammals
purrr::map(names(threatened_mammals_pa_matrix100)[-c(1:2)], function(sp){
  r <- rasterFromXYZ(threatened_mammals_pa_matrix100 %>% dplyr::select(x, y, sp))
  proj4string(r) <- "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  writeRaster(r, filename = paste0("data/input_rasters/100/mammals/", sp, ".tif"), format = "GTiff")
})
#' ### Amphibians
purrr::map(names(threatened_amphibians_pa_matrix100)[-c(1:2)], function(sp){
  r <- rasterFromXYZ(threatened_amphibians_pa_matrix100 %>% dplyr::select(x, y, sp))
  proj4string(r) <- "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  writeRaster(r, filename = paste0("data/input_rasters/100/amphibians/", sp, ".tif"), format = "GTiff")
})
#' ### Copy out amphibians, birds and mammals into a "combined" folder
file.copy(from = c(list.files("data/input_rasters/100/birds", full.names = TRUE),
                   list.files("data/input_rasters/100/mammals", full.names = TRUE),
                   list.files("data/input_rasters/100/amphibians", full.names = TRUE)), 
          to = "data/input_rasters/combined" 
)
#'
#' ## Run Zonation
##### Set up Zonation software 
##### -- IMPORTANT -- ##################################################################################################
#### For Zonation analyses to run, make sure that you have installed Zonation 4, 
#### a full download can be found at https://www.helsinki.fi/en/researchgroups/metapopulation-research-centre/software,
#### then place the Zonation software's executable file ("zig4.exe") somewhere within your working directory 
########################################################################################################################
##### Install R zonator package, if not already installed
if (!("zonator" %in% installed.packages()[,"Package"])) install.packages("zonator")
### Create .dat template files for running core area Zonation (CAZ) or additive benefit function (ABF)
## Create template files by copying "template.dat" file included with zonator installation
zonator_path <- find.package("zonator")
setwd(zonator_path)
file.copy("extdata/template.dat", "extdata/template_CAZ.dat")
file.copy("extdata/template.dat", "extdata/template_ABF.dat")
## Update removal rule (rule 2 is the Additive Benefit Function) within "template_ABF.dat" file
template_ABF <- readLines("extdata/template_ABF.dat")
template_ABF[2] <- "removal rule = 2"
writeLines(template_ABF, "extdata/template_ABF.dat")
#' ### Create directory to store outputs
setwd("~")
dir.create("output")
dir.create("output/zonation_runs")
dir.create("output/zonation_runs/100")
#' ### Run zonation
#' ### NOTE: ABF stands for Additive Benefit Function algorithm (strategy 1 in the paper) and CAZ stands for Core Area Zonation algorithm (strategy 2 in the paper)
#' #### Reptiles
run_zonation(id = "reptiles", runs = 5, algorithm = "CAZ", out_dir = "output/zonation_runs/100", input_rasters_dir = "data/input_rasters/100")
run_zonation(id = "reptiles", runs = 5, algorithm = "ABF", out_dir = "output/zonation_runs/100", input_rasters_dir = "data/input_rasters/100")
#' #### Birds
run_zonation(id = "birds", runs = 5, algorithm = "CAZ", out_dir = "output/zonation_runs/100", input_rasters_dir = "data/input_rasters/100")
run_zonation(id = "birds", runs = 5, algorithm = "ABF", out_dir = "output/zonation_runs/100", input_rasters_dir = "data/input_rasters/100")
#' #### Mammals
run_zonation(id = "mammals", runs = 5, algorithm = "CAZ", out_dir = "output/zonation_runs/100", input_rasters_dir = "data/input_rasters/100")
run_zonation(id = "mammals", runs = 5, algorithm = "ABF", out_dir = "output/zonation_runs/100", input_rasters_dir = "data/input_rasters/100")
#' #### Amphibians
run_zonation(id = "amphibians", runs = 5, algorithm = "CAZ", out_dir = "output/zonation_runs/100", input_rasters_dir = "data/input_rasters/100")
run_zonation(id = "amphibians", runs = 5, algorithm = "ABF", out_dir = "output/zonation_runs/100", input_rasters_dir = "data/input_rasters/100")
#' #### Combined
run_zonation(id = "combined", runs = 5, algorithm = "CAZ", out_dir = "output/zonation_runs/100", input_rasters_dir = "data/input_rasters/100")
run_zonation(id = "combined", runs = 5, algorithm = "ABF", out_dir = "output/zonation_runs/100", input_rasters_dir = "data/input_rasters/100")
#'
#' ## Extract Zonation ranks
#' ### CAZ algorithm
ranks_CAZ_reptiles <- extract_zonation_ranks(taxon = "reptiles", algorithm = "CAZ", out_dir = "output/zonation_runs/100")
ranks_CAZ_birds <- extract_zonation_ranks(taxon = "birds", algorithm = "CAZ", out_dir = "output/zonation_runs/100")
ranks_CAZ_mammals <- extract_zonation_ranks(taxon = "mammals", algorithm = "CAZ", out_dir = "output/zonation_runs/100")
ranks_CAZ_amphibians <- extract_zonation_ranks(taxon = "amphibians", algorithm = "CAZ", out_dir = "output/zonation_runs/100")
ranks_CAZ_combined <- extract_zonation_ranks(taxon = "combined", algorithm = "CAZ", out_dir = "output/zonation_runs/100")
#' ### ABF algorithm
ranks_ABF_reptiles <- extract_zonation_ranks(taxon = "reptiles", algorithm = "ABF", out_dir = "output/zonation_runs/100")
ranks_ABF_birds <- extract_zonation_ranks(taxon = "birds", algorithm = "ABF", out_dir = "output/zonation_runs/100")
ranks_ABF_mammals <- extract_zonation_ranks(taxon = "mammals", algorithm = "ABF", out_dir = "output/zonation_runs/100")
ranks_ABF_amphibians <- extract_zonation_ranks(taxon = "amphibians", algorithm = "ABF", out_dir = "output/zonation_runs/100")
ranks_ABF_combined <- extract_zonation_ranks(taxon = "combined", algorithm = "ABF", out_dir = "output/zonation_runs/100")
#' ### Generate ranks based on randomized cell sequences
ranks_random_rept <- random_ranks(ranks_CAZ_rept, runs = 100)
ranks_random_bird <- random_ranks(ranks_CAZ_bird, runs = 100)
ranks_random_mamm <- random_ranks(ranks_CAZ_mamm, runs = 100)
ranks_random_amph <- random_ranks(ranks_CAZ_amph, runs = 100)
#' ### Calculate terrestrial extents over which to assess surrogacy
#' #### Create vertebrate classes object
vert_classes <- c("Birds", "Reptiles", "Mammals", "Amphibians")
#' #### Extract Zonation rank maps
map_extent <- purrr::map(vert_classes, extract_zonation_maps, algorithm = "CAZ", out_dir = "output/zonation_runs_threatened", suffix = "082020", resolution = "100") %>% 
  set_names(vert_classes)
#' #### Extract combined terrestrial extent over which are any threatened species occur, regardless of class
terrestrial_extent <- map_extent[["Birds"]] > 0 | map_extent[["Reptiles"]] > 0 | map_extent[["Mammals"]] > 0 | map_extent[["Amphibians"]] > 0
terrestrial_extent <- as.data.frame(terrestrial_extent, xy = TRUE) %>% mutate(cellID = 1:length(terrestrial_extent[])) %>% dplyr::filter(complete.cases(.))
ranks_random_terrestrial <- random_ranks(terrestrial_extent$cellID, runs = 100)
#' #### Threatened Reptile+Birds extent
ranks_random_rept_bird <- random_ranks(as.data.frame(map_extent[["Birds"]] > 0 | map_extent[["Reptiles"]] > 0, xy = TRUE) %>% mutate(cellID = 1:length(x[])) %>% dplyr::filter(complete.cases(.)) %>% pull(cellID), runs = 100)
#' #### Threatened Reptile+Mammals extent
ranks_random_rept_mamm <- random_ranks(as.data.frame(map_extent[["Mammals"]] > 0 | map_extent[["Reptiles"]] > 0, xy = TRUE) %>% mutate(cellID = 1:length(x[])) %>% dplyr::filter(complete.cases(.)) %>% pull(cellID), runs = 100)
#' #### Threatened Reptile+Amphibians extent
ranks_random_rept_amph <- random_ranks(as.data.frame(map_extent[["Amphibians"]] > 0 | map_extent[["Reptiles"]] > 0, xy = TRUE) %>% mutate(cellID = 1:length(x[])) %>% dplyr::filter(complete.cases(.)) %>% pull(cellID), runs = 100)
#' #### Threatened Birds+Mammals extent
ranks_random_bird_mamm <- random_ranks(as.data.frame(map_extent[["Birds"]] > 0 | map_extent[["Mammals"]] > 0, xy = TRUE) %>% mutate(cellID = 1:length(x[])) %>% dplyr::filter(complete.cases(.)) %>% pull(cellID), runs = 100)
#' #### Threatened Birds+Amphibians extent
ranks_random_bird_amph <- random_ranks(as.data.frame(map_extent[["Birds"]] > 0 | map_extent[["Amphibians"]] > 0, xy = TRUE) %>% mutate(cellID = 1:length(x[])) %>% dplyr::filter(complete.cases(.)) %>% pull(cellID), runs = 100)
#' #### Threatened Mammals+Amphibians extent
ranks_random_mamm_amph <- random_ranks(as.data.frame(map_extent[["Mammals"]] > 0 | map_extent[["Amphibians"]] > 0, xy = TRUE) %>% mutate(cellID = 1:length(x[])) %>% dplyr::filter(complete.cases(.)) %>% pull(cellID), runs = 100)
#'
#' ## Generate Surrogacy Curves
#' ### CAZ
#' #### Reptiles as target
#' ##### Optimal curve
curve_CAZ_rept_threat_rept <- get_sai_curves_target(target_matrix = threatened_reptiles_pa_matrix100, target_ranks = ranks_CAZ_rept_threat, algorithm = "CAZ", runs = 5)
saveRDS(curve_CAZ_rept_threat_rept, "output/zonation_runs_threatened/100/curve_CAZ_rept_threat_rept.rds")
#' ##### Birds as surrogate
curve_CAZ_rept_threat_bird <- get_sai_curves_surrogate(target_matrix = threatened_reptiles_pa_matrix100, surrogate_ranks = ranks_CAZ_bird_threat, algorithm = "CAZ", runs = 5)
saveRDS(curve_CAZ_rept_threat_bird, "output/zonation_runs_threatened/100/curve_CAZ_rept_threat_bird.rds")
#' ##### Mammals as surrogate
curve_CAZ_rept_threat_mamm <- get_sai_curves_surrogate(target_matrix = threatened_reptiles_pa_matrix100, surrogate_ranks = ranks_CAZ_mamm_threat, algorithm = "CAZ", runs = 5)
saveRDS(curve_CAZ_rept_threat_mamm, "output/zonation_runs_threatened100/curve_CAZ_rept_threat_mamm.rds")
#' ##### Amphibians as surrogate
curve_CAZ_rept_threat_amph <- get_sai_curves_surrogate(target_matrix = threatened_reptiles_pa_matrix100, surrogate_ranks = ranks_CAZ_amph_threat, algorithm = "CAZ", runs = 5)
saveRDS(curve_CAZ_rept_threat_amph, "output/zonation_runs_threatened100/curve_CAZ_rept_threat_amph.rds")
#' ##### Combination as surrogate
curve_CAZ_rept_threat_comb <- get_sai_curves_surrogate(target_matrix = threatened_reptiles_pa_matrix100, surrogate_ranks = ranks_CAZ_comb, algorithm = "CAZ", runs = 5)
saveRDS(curve_CAZ_rept_threat_amph, "output/zonation_runs_threatened100/curve_CAZ_rept_threat_amph.rds")
#'
#' ### ABF
#' #### Reptiles as target
#' ##### Optimal curve
curve_ABF_rept_threat_rept <- get_sai_curves_target(target_matrix = threatened_reptiles_pa_matrix100, target_ranks = ranks_ABF_rept_threat, algorithm = "ABF", runs = 5)
saveRDS(curve_ABF_rept_threat_rept, "output/zonation_runs_threatened100/curve_ABF_rept_threat_rept.rds")
#' ##### Birds as surrogate
curve_ABF_rept_threat_bird <- get_sai_curves_surrogate(target_matrix = threatened_reptiles_pa_matrix100, surrogate_ranks = ranks_ABF_bird_threat, algorithm = "ABF", runs = 5)
saveRDS(curve_ABF_rept_threat_bird, "output/zonation_runs_threatened100/curve_ABF_rept_threat_bird.rds")
#' ##### Mammals as surrogate
curve_ABF_rept_threat_mamm <- get_sai_curves_surrogate(target_matrix = threatened_reptiles_pa_matrix100, surrogate_ranks = ranks_ABF_mamm_threat, algorithm = "ABF", runs = 5)
saveRDS(curve_ABF_rept_threat_mamm, "output/zonation_runs_threatened100/curve_ABF_rept_threat_mamm.rds")
#' ##### Amphibians as surrogate
curve_ABF_rept_threat_amph <- get_sai_curves_surrogate(target_matrix = threatened_reptiles_pa_matrix100, surrogate_ranks = ranks_ABF_amph_threat, algorithm = "ABF", runs = 5)
saveRDS(curve_ABF_rept_threat_amph, "output/zonation_runs_threatened100/curve_ABF_rept_threat_amph.rds")
#' ##### Combination as surrogate
curve_ABF_rept_threat_comb <- get_sai_curves_surrogate(target_matrix = threatened_reptiles_pa_matrix100, surrogate_ranks = ranks_ABF_comb, algorithm = "ABF", runs = 5)
saveRDS(curve_ABF_rept_threat_amph, "output/zonation_runs_threatened100/curve_ABF_rept_threat_amph.rds")
#'
#'
#' ## Generate random expectations
#' ### Reptiles as target
#' #### CAZ
#' ##### Terrestrial extent
curve_CAZ_rept_threat_random_terr_ext <- get_sai_curves_random(target_matrix = threatened_reptiles_pa_matrix100, random_ranks = ranks_random_terrestrial, reference_coordinates = ranks_CAZ_rept_threat$reference_coordinates, algorithm = "CAZ", runs = 10)
saveRDS(curve_CAZ_rept_threat_random_terr_ext, "output/zonation_runs_threatened100/curve_CAZ_rept_threat_random_terr_ext.rds")
#' ##### Birds as surrogate
curve_CAZ_rept_threat_random_bird <- get_sai_curves_random(target_matrix = threatened_reptiles_pa_matrix100, random_ranks = ranks_random_rept_bird, reference_coordinates = ranks_CAZ_rept_threat$reference_coordinates, algorithm = "CAZ", runs = 10)
saveRDS(curve_CAZ_rept_threat_random_bird, "output/zonation_runs_threatened100/curve_CAZ_rept_threat_random_bird.rds")
#' ##### Mammals as surrogate
curve_CAZ_rept_threat_random_mamm <- get_sai_curves_random(target_matrix = threatened_reptiles_pa_matrix100, random_ranks = ranks_random_rept_mamm, reference_coordinates = ranks_CAZ_rept_threat$reference_coordinates, algorithm = "CAZ", runs = 10)
saveRDS(curve_CAZ_rept_threat_random_mamm, "output/zonation_runs_threatened100/curve_CAZ_rept_threat_random_mamm.rds")
#' ##### Amphibians as surrogate
curve_CAZ_rept_threat_random_amph <- get_sai_curves_random(target_matrix = threatened_reptiles_pa_matrix100, random_ranks = ranks_random_rept_amph, reference_coordinates = ranks_CAZ_rept_threat$reference_coordinates, algorithm = "CAZ", runs = 10)
saveRDS(curve_CAZ_rept_threat_random_amph, "output/zonation_runs_threatened100/curve_CAZ_rept_threat_random_amph.rds")
#' ##### Combined as surrogate
curve_CAZ_rept_threat_random_comb <- get_sai_curves_random(target_matrix = threatened_reptiles_pa_matrix100, random_ranks = ranks_random_rept_comb, reference_coordinates = ranks_CAZ_rept_threat$reference_coordinates, algorithm = "CAZ", runs = 10)
saveRDS(curve_CAZ_rept_threat_random_comb, "output/zonation_runs_threatened100/curve_CAZ_rept_threat_random_comb.rds")
#' #### ABF
#' ##### Terrestrial extent
curve_ABF_rept_threat_random_terr_ext <- get_sai_curves_random(target_matrix = threatened_reptiles_pa_matrix100, random_ranks = ranks_random_terrestrial, reference_coordinates = ranks_ABF_rept_threat$reference_coordinates, algorithm = "ABF", runs = 10)
saveRDS(curve_ABF_rept_threat_random_terr_ext, "output/zonation_runs_threatened100/curve_ABF_rept_threat_random_terr_ext.rds")
#' ##### Birds as surrogate
curve_ABF_rept_threat_random_bird <- get_sai_curves_random(target_matrix = threatened_reptiles_pa_matrix100, random_ranks = ranks_random_rept_bird, reference_coordinates = ranks_ABF_rept_threat$reference_coordinates, algorithm = "ABF", runs = 10)
saveRDS(curve_ABF_rept_threat_random_bird, "output/zonation_runs_threatened100/curve_ABF_rept_threat_random_bird.rds")
#' ##### Mammals as surrogate
curve_ABF_rept_threat_random_mamm <- get_sai_curves_random(target_matrix = threatened_reptiles_pa_matrix100, random_ranks = ranks_random_rept_mamm, reference_coordinates = ranks_ABF_rept_threat$reference_coordinates, algorithm = "ABF", runs = 10)
saveRDS(curve_ABF_rept_threat_random_mamm, "output/zonation_runs_threatened100/curve_ABF_rept_threat_random_mamm.rds")
#' ##### Amphibians as surrogate
curve_ABF_rept_threat_random_amph <- get_sai_curves_random(target_matrix = threatened_reptiles_pa_matrix100, random_ranks = ranks_random_rept_amph, reference_coordinates = ranks_ABF_rept_threat$reference_coordinates, algorithm = "ABF", runs = 10)
saveRDS(curve_ABF_rept_threat_random_amph, "output/zonation_runs_threatened100/curve_ABF_rept_threat_random_amph.rds")
#' ##### Combination as surrogate
curve_ABF_rept_threat_random_comb <- get_sai_curves_random(target_matrix = threatened_reptiles_pa_matrix100, random_ranks = ranks_random_rept_comb, reference_coordinates = ranks_ABF_rept_threat$reference_coordinates, algorithm = "ABF", runs = 10)
saveRDS(curve_ABF_rept_threat_random_comb, "output/zonation_runs_threatened100/curve_ABF_rept_threat_random_comb.rds")
#'
#' ## Generate summary outputs
#' ### CAZ
#' #### Reptiles as target
#' ##### Birds as surrogate
CAZ_rept_bird_threat_sai_curves <- combine_sai_curves(target = curve_CAZ_rept_threat_rept[1:nrow(curve_CAZ_rept_threat_random_terr_ext), ], surrogate = curve_CAZ_rept_threat_bird[1:nrow(curve_CAZ_rept_threat_random_terr_ext), ], random = curve_CAZ_rept_threat_random_terr_ext)
CAZ_rept_bird_threat_sai <- calculate_sai(CAZ_rept_bird_threat_sai_curves)[[1]]
CAZ_rept_bird_threat_sai_plot <- plot_sai_curves(CAZ_rept_bird_threat_sai_curves)
## Mammals as surrogate
CAZ_rept_mamm_threat_sai_curves <- combine_sai_curves(target = curve_CAZ_rept_threat_rept[1:nrow(curve_CAZ_rept_threat_random_terr_ext), ], surrogate = curve_CAZ_rept_threat_mamm[1:nrow(curve_CAZ_rept_threat_random_terr_ext), ], random = curve_CAZ_rept_threat_random_terr_ext)
CAZ_rept_mamm_threat_sai <- calculate_sai(CAZ_rept_mamm_threat_sai_curves)[[1]]
CAZ_rept_mamm_threat_sai_plot <- plot_sai_curves(CAZ_rept_mamm_threat_sai_curves)
## Amphibians as surrogate
CAZ_rept_amph_threat_sai_curves <- combine_sai_curves(target = curve_CAZ_rept_threat_rept[1:nrow(curve_CAZ_rept_threat_random_terr_ext), ], surrogate = curve_CAZ_rept_threat_amph[1:nrow(curve_CAZ_rept_threat_random_terr_ext), ], random = curve_CAZ_rept_threat_random_terr_ext)
CAZ_rept_amph_threat_sai <- calculate_sai(CAZ_rept_amph_threat_sai_curves)[[1]]
CAZ_rept_amph_threat_sai_plot <- plot_sai_curves(CAZ_rept_amph_threat_sai_curves)
## Combined as surrogate
CAZ_rept_comb_threat_sai_curves <- combine_sai_curves(target = curve_CAZ_rept_threat_rept[1:nrow(curve_CAZ_rept_threat_random_terr_ext), ], surrogate = curve_CAZ_rept_threat_comb[1:nrow(curve_CAZ_rept_threat_random_terr_ext), ], random = curve_CAZ_rept_threat_random_terr_ext)
CAZ_rept_comb_threat_sai <- calculate_sai(CAZ_rept_comb_threat_sai_curves)[[1]]
CAZ_rept_comb_threat_sai_plot <- plot_sai_curves(CAZ_rept_comb_threat_sai_curves)
#' ### ABF
#' #### Reptiles as target
#' ##### Birds as surrogate
ABF_rept_bird_threat_sai_curves <- combine_sai_curves(target = curve_ABF_rept_threat_rept[1:nrow(curve_ABF_rept_threat_random_terr_ext), ], surrogate = curve_ABF_rept_threat_bird[1:nrow(curve_ABF_rept_threat_random_terr_ext), ], random = curve_ABF_rept_threat_random_terr_ext)
ABF_rept_bird_threat_sai <- calculate_sai(ABF_rept_bird_threat_sai_curves)[[1]]
ABF_rept_bird_threat_sai_plot <- plot_sai_curves(ABF_rept_bird_threat_sai_curves)
## Mammals as surrogate
ABF_rept_mamm_threat_sai_curves <- combine_sai_curves(target = curve_ABF_rept_threat_rept[1:nrow(curve_ABF_rept_threat_random_terr_ext), ], surrogate = curve_ABF_rept_threat_mamm[1:nrow(curve_ABF_rept_threat_random_terr_ext), ], random = curve_ABF_rept_threat_random_terr_ext)
ABF_rept_mamm_threat_sai <- calculate_sai(ABF_rept_mamm_threat_sai_curves)[[1]]
ABF_rept_mamm_threat_sai_plot <- plot_sai_curves(ABF_rept_mamm_threat_sai_curves)
## Amphibians as surrogate
ABF_rept_amph_threat_sai_curves <- combine_sai_curves(target = curve_ABF_rept_threat_rept[1:nrow(curve_ABF_rept_threat_random_terr_ext), ], surrogate = curve_ABF_rept_threat_amph[1:nrow(curve_ABF_rept_threat_random_terr_ext), ], random = curve_ABF_rept_threat_random_terr_ext)
ABF_rept_amph_threat_sai <- calculate_sai(ABF_rept_amph_threat_sai_curves)[[1]]
ABF_rept_amph_threat_sai_plot <- plot_sai_curves(ABF_rept_amph_threat_sai_curves)
## Combined as surrogate
ABF_rept_comb_threat_sai_curves <- combine_sai_curves(target = curve_ABF_rept_threat_rept[1:nrow(curve_ABF_rept_threat_random_terr_ext), ], surrogate = curve_ABF_rept_threat_comb[1:nrow(curve_ABF_rept_threat_random_terr_ext), ], random = curve_ABF_rept_threat_random_terr_ext)
ABF_rept_comb_threat_sai <- calculate_sai(ABF_rept_comb_threat_sai_curves)[[1]]
ABF_rept_comb_threat_sai_plot <- plot_sai_curves(ABF_rept_comb_threat_sai_curves)
