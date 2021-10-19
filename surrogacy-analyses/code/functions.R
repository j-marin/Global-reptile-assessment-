#' ---
#' title: Global Reptile Assessment <br> <strong> Surrogacy in Conservation For Threatened Species - Functions
#' ---
#' 
#' ### Create edit of create_zproject() function to accommodate different algorithms
#' ### ABF = Additive Benefit Function (strategy 1) and CAZ = Core Area Zonation (strategy 2)
create_zproject_edit <- function(name, dir, variants, dat_template_file = NULL,
                                 spp_template_file = NULL, spp_template_dir = NULL,
                                 overwrite = FALSE, debug = FALSE, algorithm = c("CAZ", "ABF"), ...) {
  
  algorithm <- match.arg(algorithm)
  
  if (!file.exists(dir)) {
    stop("Directory ", dir, " provided does not exist.")
  }
  
  # Create the new location
  project_dir <- file.path(dir, name)
  if (file.exists(project_dir)) {
    if (overwrite) {
      if (debug) message("Removing existing directory ", project_dir)
      unlink(project_dir, recursive = TRUE, force = TRUE)
    } else {
      stop("Project ", project_dir, " already exists and overwrite is off")
    }
  }
  
  if (debug) message("Creating a project directory ", project_dir)
  dir.create(project_dir)
  
  # Create an empty README file for the project
  if (debug) message("Creating an empty README file")
  file.create(file.path(project_dir, "README.md"), showWarnings = FALSE)
  
  # Create the variant subfolders with content
  for (variant in variants) {
    variant_dir <- file.path(project_dir, variant)
    if (debug) message("Creating a variant directory ", variant_dir)
    dir.create(variant_dir)
    
    # If no templates are provided, use the ones shipped with zonator. Change
    # the filenames to match the variant.
    if (is.null(dat_template_file)) {
      
      if (algorithm == "CAZ") template_file <- "template_CAZ.dat"
      if (algorithm == "ABF") template_file <- "template_ABF.dat"
      
      dat_template_file <- system.file("extdata", paste(template_file, sep = ""),
                                       package = "zonator")
    }
    dat_to <- file.path(variant_dir, paste0(variant, ".dat"))
    
    # If no templates are provided, use the ones shipped with zonator. Change
    # the filenames to match the variant.
    if (is.null(spp_template_file) & is.null(spp_template_dir)) {
      spp_template_file <- system.file("extdata", "template.spp",
                                       package = "zonator")
    }
    
    # Define the target variant spp file path
    spp_to <- file.path(variant_dir, paste0(variant, ".spp"))
    
    # Copy the templates to the new variant folder
    if (debug) message("Copying template dat-file ", dat_template_file,
                       " to variant directory ", variant_dir)
    if (file.exists(dat_template_file)) {
      file.copy(from = dat_template_file, to = dat_to, overwrite = TRUE)
    } else {
      stop("dat-file template ", dat_template_file, " not found")
    }
    # Work out the details depending if using a template file or a
    # directory of input rasters.
    if (!is.null(spp_template_dir)) {
      # We may have multiple directories
      if (all(sapply(spp_template_dir, function(x) file.exists(x)))) {
        if (debug) {
          if (length(spp_template_dir) > 1) {
            dir_msg <- paste("Creating a spp file from rasters in directories ",
                             paste(spp_template_dir, collapse = ", "))
          } else{
            dir_msg <- paste("Creating a spp file from rasters in directory ",
                             spp_template_dir)
          }
          message(dir_msg)
        }
        create_spp(filename = spp_to, spp_file_dir = spp_template_dir, ...)
      } else {
        stop("Spp template dir ", spp_template_dir, " not found.")
      }
    } else if (!is.null(spp_template_file)) {
      if (file.exists(spp_template_file)) {
        if (debug) {
          message("Copying template spp-file  ", spp_template_file,
                  " to variant directory ", variant_dir)
        }
        file.copy(from = spp_template_file, to = spp_to, overwrite = TRUE)
      } else {
        stop("Input template spp-file ", spp_template_file, " not found!")
      }
    }
    
    # Create to output folder
    output_dir <- file.path(variant_dir, paste0(variant, "_out"))
    if (debug) message("Creating an output directory ", output_dir)
    dir.create(output_dir, recursive = TRUE)
    # Create a bat file, first read the template content
    bat_from <- system.file("extdata", "template.bat", package = "zonator")
    cmd_sequence <- scan(file = bat_from, "character", sep = " ",
                         quiet = TRUE)
    # Replace tokens with actual (relative) paths
    dat_relative <- gsub(paste0(project_dir, .Platform$file.sep), "", dat_to)
    spp_relative <- gsub(paste0(project_dir, .Platform$file.sep), "", spp_to)
    output_dir_relative <- gsub(paste0(project_dir, .Platform$file.sep), "",
                                output_dir)
    cmd_sequence <- gsub("INPUT_DAT", dat_relative, cmd_sequence)
    cmd_sequence <- gsub("INPUT_SPP", spp_relative, cmd_sequence)
    cmd_sequence <- gsub("OUTPUT", file.path(output_dir_relative,
                                             paste0(variant, ".txt")),
                         cmd_sequence)
    # Write bat-file
    bat_to <- file.path(project_dir, paste0(variant, ".bat"))
    if (debug) message("Writing bat file ", bat_to)
    cat(paste0(paste(cmd_sequence, collapse = " "), "\n"), file = bat_to)
  }
  
  return(invisible(NULL))
}
#' ### Function to run zonation from input rasters
run_zonation <- function(id = "Amphibians",
                         runs = 10, 
                         working_dir = getwd(), 
                         algorithm = "CAZ",
                         out_dir = "output/zonation_runs",
                         input_rasters_dir = "data/input_rasters",
                         ...){
  #### Set working directory
  setwd(working_dir)
  #### Create directory to store outputs
  dir.create("output/zonation_runs/", showWarnings = FALSE)
  out_dir <- paste(out_dir, algorithm, sep = "/")
  dir.create(out_dir, showWarnings = FALSE)

  #### Define project settings
  zonation_project <- create_zproject_edit(name = id, dir = out_dir, 
                                           variants = paste(id, 1:runs, sep = ""),
                                           spp_template_dir = paste0(working_dir, "/", input_rasters_dir, "/", id),
                                           overwrite = TRUE, algorithm = algorithm,
                                           ...)
  #### Run Zonation
  for (run in 1:runs){
    temp_dir <- paste(working_dir, out_dir, id, sep = "/")
    setwd(temp_dir)
    run_bat(paste(id, run, ".bat", sep = ""), exe = paste(working_dir, "/data/zig4", sep = ""))
  }
  
  ## Reset working directory
  setwd(working_dir)
}
#' ### Function to extract output rasters for spatial prioritizations generated from Zonation runs
extract_zonation_maps <- function(taxon = "Reptiles", algorithm = "CAZ", resolution = "100", out_dir = "output/zonation_runs"){
  
  outdir <- paste0(out_dir, "/", resolution, "/", algorithm, "/", taxon, "_projected_", suffix)
  
  ### List rank.compressed.tif files
  dirfiles <- list.files(path = outdir, pattern = "rank.compressed.tif", recursive = TRUE, full.names = TRUE)
  
  ### Extract Zonation ranks
  rank_rasters <- purrr::map(dirfiles, raster) %>% raster::stack() %>% mean()
  
  return(rank_rasters)

}

#' ### Function to extract output rasters for spatial priority ranks generated from Zonation runs
extract_zonation_ranks <- function(taxon = "reptiles", algorithm = "CAZ", out_dir = "output/zonation_runs"){
  outdir <- paste0(out_dir, "/", algorithm, "/", taxon)
  
  ### List rank.compressed.tif files
  dirfiles <- list.files(path = outdir, pattern = "rank.compressed.tif", recursive = TRUE, full.names = TRUE)
  
  ### Extract Zonation ranks
  rank_rasters <- purrr::map(dirfiles, raster)
  
  runs <- length(rank_rasters)
  
  ### Extract ranks from
  zonation_ranks <- lapply(rank_rasters, function(x){
    target_rank_df <- x %>% as.data.frame(xy = TRUE) %>% mutate(cellID = 1:length(x[]))
    names(target_rank_df)[3] <- "rank"
    target_rank_df <- target_rank_df[order(target_rank_df$rank, decreasing = TRUE), ]
    target_rank_df
  })
  
  zonation_ranks <- data.frame(zonation_ranks[[1]], do.call("cbind", lapply(zonation_ranks[-1], "[[", 4)))
  names(zonation_ranks)[-c(1:2)] <- c("rank_priority", paste("rank", 1:(runs), sep = ""))
  ### Merge zonation ranks with the full reference coordinates: this will enable assigning the correct rows with the target matrix later on
  zonation_ranks <- list(zonation_ranks = zonation_ranks, reference_coordinates = as.data.frame(rank_rasters[[1]], xy = TRUE)[, 1:2])
  return(zonation_ranks) 
}

##### -- Random selection of grid cells -- #####
##### -- random_ranks -- #####
##### Generate N (specified in "runs") random sequences of grid cell numbers (based on the total number of grid cells in a target matrix) 
##### and store them within a data.frame
random_ranks <- function(target_ranks, runs = 1000){
  
  if (is.numeric(target_ranks)){
    random_ranks_df <- matrix(NA, nrow = length(target_ranks), ncol = runs) %>% as.data.frame()
    for(run in 1:runs){
      random_ranks_df[, run] <- c(sample(target_ranks, length(target_ranks)))
    }
  } else {
    random_ranks_df <- matrix(NA, nrow = length(target_ranks$zonation_ranks$rank1), ncol = runs) %>% as.data.frame()
    for(run in 1:runs){
      random_ranks_df[, run] <- c(sample(target_ranks$zonation_ranks$rank1, length(target_ranks$zonation_ranks$rank1)))
    }
  }
  
  
  names(random_ranks_df) <- paste("rank", 1:(runs), sep = "")
  return(random_ranks_df)
}

##### -- Functions for estimating proportion of target diversity represented as a function area -- #####
##### Greedy algorithm
#### The function is the same regardless of whether the target is species, trait categories, or nodes.
target_accumulation_CAZ <- function(target_matrix){
  cumulative_target <- target_matrix %>% dplyr::mutate_all(cumsum)
  out_curves <- 100 * (rowSums(cumulative_target > 0)/ncol(target_matrix))  
  return(out_curves)
}

##### ABF
##### Equations derived from Pollock et al. (2017) Nature (doi: 10.1038/nature22368)
#### Trait categories
target_accumulation_ABF <- function(target_matrix, ranges){
  cumulative_target <- target_matrix %>% dplyr::mutate_all(cumsum)
  out_curves <- (1/ncol(target_matrix)) * rowSums(t(t(cumulative_target)/ranges)) * 100
  return(out_curves)
}

##### -- Species Accumulation Index -- #####
##### -- get_SAI_curves -- #####
##### Derive accumulation curves based on spatial prioritization ranks and the appropriate target diversity function
get_sai_curves_target <- function(target_matrix,
                                  target_ranks,
                                  algorithm = c("CAZ", "ABF"),
                                  n_cells = NULL,
                                  runs = 10
){
  
  #### Match algorithm
  algorithm <- match.arg(algorithm)
  if (is.null(n_cells)) n_cells <- nrow(target_ranks$zonation_ranks)  
  #### Calculate range sizes
  if (algorithm == "ABF") range_sizes <- colSums(target_matrix[, -c(1:2)])
  #### Update objects
  reference_coordinates <- target_ranks$reference_coordinates
  target_ranks <- target_ranks$zonation_ranks[1:n_cells, ]
  target_matrix <- left_join(reference_coordinates, target_matrix, by = c("x", "y"))
  target_matrix[is.na(target_matrix)] <- 0
  
  #### Subset colums and rows from rank data.frames
  target_ranks <- target_ranks[paste("rank", 1:runs, sep = "")] 
  
  #### Estimate curves
  ### Optimal
  ## Create output object
  O_curves <- data.frame(matrix(NA, nrow = nrow(target_ranks), ncol = ncol(target_ranks)))
  names(O_curves) <- paste("curve", 1:ncol(O_curves), sep = "")
  ## Calculate accumulation of appropriate target and algorithm combination
  for (i in 1:ncol(O_curves)){
    if (algorithm == "CAZ") O_curves[, i] <- target_accumulation_CAZ(target_matrix[target_ranks[, i], -c(1:2)])
    if (algorithm == "ABF") O_curves[, i] <- target_accumulation_ABF(target_matrix[target_ranks[, i], -c(1:2)], ranges = range_sizes)
  }
  # Filter curves
  O_curves <- O_curves %>% dplyr::filter(complete.cases(.))
  
  ### Return output
  return(O_curves)
}

##### Derive accumulation curves based on spatial prioritization ranks and the appropriate target diversity function
get_sai_curves_surrogate <- function(target_matrix,
                                     surrogate_ranks,
                                     algorithm = c("CAZ", "ABF"),
                                     n_cells = NULL,
                                     runs = 10
){
  
  #### Match algorithm
  algorithm <- match.arg(algorithm)
  if (is.null(n_cells)) n_cells <- nrow(surrogate_ranks$zonation_ranks) 
  #### Calculate range sizes
  if (algorithm == "ABF") range_sizes <- colSums(target_matrix[, -c(1:2)])
  #### Update objects
  reference_coordinates <- surrogate_ranks$reference_coordinates
  surrogate_ranks <- surrogate_ranks$zonation_ranks[1:n_cells, ]
  target_matrix <- left_join(reference_coordinates, target_matrix, by = c("x", "y"))
  target_matrix[is.na(target_matrix)] <- 0
  
  #### Subset colums and rows from rank data.frames
  surrogate_ranks <- surrogate_ranks[paste("rank", 1:runs, sep = "")] 

  ### Surrogate
  ## Create output object
  S_curves <- data.frame(matrix(NA, nrow = nrow(surrogate_ranks), ncol = ncol(surrogate_ranks)))
  names(S_curves) <- paste("curve", 1:ncol(S_curves), sep = "")
  ## Calculate accumulation of appropriate target and algorithm combination
  for (i in 1:ncol(S_curves)){
    if (algorithm == "CAZ") S_curves[, i] <- target_accumulation_CAZ(target_matrix[surrogate_ranks[, i], -c(1:2)])
    if (algorithm == "ABF") S_curves[, i] <- target_accumulation_ABF(target_matrix[surrogate_ranks[, i], -c(1:2)], ranges = range_sizes)
  }
  # Filter curves
  S_curves <- S_curves
  
  ### Return output
  return(S_curves)
}

##### Derive accumulation curves based on spatial prioritization ranks and the appropriate target diversity function
get_sai_curves_random <- function(target_matrix,
                                  random_ranks,
                                  reference_coordinates,
                                  algorithm = c("CAZ", "ABF"),
                                  n_cells = NULL,
                                  runs = 10
){
  
  #### Match algorithm
  algorithm <- match.arg(algorithm)
  if (is.null(n_cells)) n_cells <- nrow(random_ranks) 
  #### Calculate range sizes
  if (algorithm == "ABF") range_sizes <- colSums(target_matrix[, -c(1:2)])
  #### Update objects
  random_ranks <- random_ranks[1:n_cells, paste("rank", 1:runs, sep = "")]
  target_matrix <- left_join(reference_coordinates, target_matrix, by = c("x", "y"))
  target_matrix[is.na(target_matrix)] <- 0

  ### Random
  ## Create output object
  R_curves <- random_ranks
  names(R_curves) <- paste("curve", 1:ncol(R_curves), sep = "")
  for (i in 1:ncol(R_curves)){
    if (algorithm == "CAZ") R_curves[, i] <- target_accumulation_CAZ(target_matrix[random_ranks[, i], -c(1:2)])
    if (algorithm == "ABF") R_curves[, i] <- target_accumulation_ABF(target_matrix[random_ranks[, i], -c(1:2)], ranges = range_sizes)
  }
  # Filter curves
  R_curves <- R_curves %>% dplyr::filter(complete.cases(.))
  ### Return output
  return(R_curves)
}

##### -- combine_sai_curves() -- #####
combine_sai_curves <- function(target = curve_CAZ_rept_rept, 
                               surrogate = curve_CAZ_rept_bird, 
                               random = curve_CAZ_rept_random
                               ){
  
  target <- rbind(target, target[rep(nrow(target), nrow(random) - nrow(target)), ])
  surrogate <- rbind(surrogate, surrogate[rep(nrow(surrogate), nrow(random) - nrow(surrogate)), ])

  out <- list(optimal = target, surrogate = surrogate, random = random)
  
  return(out)
}

##### -- plot_sai_curves() -- #####
##### Plot derived accumulation curves 
plot_sai_curves <- function(sai_curves, add_sai = TRUE){
  
  ### Summarize Optimal curves
  O_mean <- rowMeans(sai_curves$optimal)
  O_CI_lower <- as.data.frame(apply(sai_curves$optimal, 1, function(x){ CI(as.vector(x))[3] }))[, 1]
  O_CI_upper <- as.data.frame(apply(sai_curves$optimal, 1, function(x){ CI(as.vector(x))[1] }))[, 1]
  ## Add an initial 0 to start curves at 0%
  O_mean <- c(0, O_mean); O_CI_lower <- c(0, O_CI_lower); O_CI_upper <- c(0, O_CI_upper)
  
  ### Summarize Surrogate curves
  S_mean <- rowMeans(sai_curves$surrogate)
  S_CI_lower <- as.data.frame(apply(sai_curves$surrogate, 1, function(x){ CI(as.vector(x))[3] }))[, 1]
  S_CI_upper <- as.data.frame(apply(sai_curves$surrogate, 1, function(x){ CI(as.vector(x))[1] }))[, 1]
  ## Add an initial 0 to start curves at 0%
  S_mean <- c(0, S_mean); S_CI_lower <- c(0, S_CI_lower); S_CI_upper <- c(0, S_CI_upper)
  
  ### Summarize Optimal curves
  R_mean <- rowMeans(sai_curves$random)
  R_CI_lower <- as.data.frame(apply(sai_curves$random, 1, function(x){ CI(as.vector(x))[3] }))[, 1]
  R_CI_upper <- as.data.frame(apply(sai_curves$random, 1, function(x){ CI(as.vector(x))[1] }))[, 1]
  ## Add an initial 0 to start curves at 0%
  R_mean <- c(0, R_mean); R_CI_lower <- c(0, R_CI_lower); R_CI_upper <- c(0, R_CI_upper)
  
  ### Generate x axis
  x_axis <- (0:nrow(sai_curves$optimal)/nrow(sai_curves$optimal)) * 100
  
  #### Generate plot
  ### Get plot data
  plot_dat <- data.frame(x_axis = x_axis, 
                         O_mean = O_mean, O_CI_lower = O_CI_lower, O_CI_upper = O_CI_upper,
                         S_mean = S_mean, S_CI_lower = S_CI_lower, S_CI_upper = S_CI_upper, 
                         R_mean = R_mean, R_CI_lower = R_CI_lower, R_CI_upper = R_CI_upper
                         )
  
  plot_dat <- plot_dat[c(seq(1, nrow(plot_dat), 100), nrow(plot_dat)), ]
  
  p <- ggplot(plot_dat) +
    geom_ribbon(aes(x = x_axis, ymin = O_CI_lower, ymax = O_CI_upper), color = "deepskyblue3", alpha = 0.2) +
    geom_line(aes(x = x_axis, y = O_mean), color = "deepskyblue3", size = 1.3) +
    geom_ribbon(aes(x = x_axis, ymin = S_CI_lower, ymax = S_CI_upper), color = "tomato", alpha = 0.2) +
    geom_line(aes(x = x_axis, y = S_mean), color = "tomato", size = 1.3) +
    geom_ribbon(aes(x = x_axis, ymin = R_CI_lower, ymax = R_CI_upper), color = grey(0.7), alpha = 0.2) +
    geom_line(aes(x = x_axis, y = R_mean), color = grey(0.4), size = 1.3) +
    theme_bw() +
    ylab("SAI") +
    xlab("% area") 
  
  if (isTRUE(add_sai)){
    sai <- calculate_sai(sai_curves)[[1]]
    p <- p + 
      annotate("text",  label = paste0("SAI = ", round(sai[["median"]], 2), " (", round(sai[["CI_lower"]], 2), ", ", round(sai[["CI_upper"]], 2), ")"), x = 50, y = 0, size = 3)
  }
  
  print(p)
  
  return(p)
  
}

##### -- calculate_sai() -- #####
##### Calculate the Species Accumulation Index from the optimal/target, surrogate and random curves 
calculate_sai <- function(sai_curves){
  #### Create a percentage area object
  area <- (0:nrow(sai_curves$optimal)/nrow(sai_curves$optimal)) * 100
  #### Calculate area under curves
  ### Create output objects
  sai_areas_optimal <- sai_areas_surrogate <- sai_areas_random <- sai_values <- NULL
  ### Area under Optimal curves
  for (i in 1:ncol(sai_curves$optimal)){
    sai_areas_optimal[i] <- trapz(area, c(0, sai_curves$optimal[, i]))
  }
  ### Area under Surrogate curves
  for (i in 1:ncol(sai_curves$surrogate)){
    sai_areas_surrogate[i] <- trapz(area, c(0, sai_curves$surrogate[, i]))
  }
  ### Area under Random curves
  for (i in 1:ncol(sai_curves$random)){
    sai_areas_random[i] <- trapz(area, c(0, sai_curves$random[, i]))
  }
  sai_values <- as.vector(outer(rep(sai_areas_surrogate, times = length(sai_areas_optimal)), sai_areas_random, "-"))/as.vector(outer(rep(sai_areas_optimal, each = length(sai_areas_surrogate)), sai_areas_random, "-"))
  sai_values <- sai_values[sai_values != "-Inf" & sai_values != "NaN"]
  sai_summary <- c(quantile(sai_values, .025, na.rm = TRUE), quantile(sai_values, .5, na.rm = TRUE), quantile(sai_values, .975, na.rm = TRUE))
  names(sai_summary) <- c("CI_lower", "median", "CI_upper")
  return(list(summary = sai_summary, all_values = sai_values))
}

#### Percentage of species represented within each conservation area
biome_protected_target <- function(target_data, biome_cover, reference_raster = terrestrial_extent_raster){
  reference_df <- reference_raster %>% as.data.frame(xy = TRUE) %>% dplyr::filter(layer == 1) %>% dplyr::select(x, y)
  biome_cover_df <- projectRaster(biome_cover, reference_raster) %>% as.data.frame(xy = TRUE) %>% dplyr::rename(biome = layer) %>% dplyr::filter(biome == 1) %>% dplyr::mutate(cellID = cellFromXY(reference_raster, data.frame(x, y)))
  #### Calculate range sizes
  range_sizes <- colSums(target_data[, -c(1:2)])
  #### Combine conservation priorities data
  target_data_biome <- target_data %>% 
    dplyr::mutate(cellID = cellFromXY(reference_raster, data.frame(x, y))) %>% 
    dplyr::filter(cellID %in% biome_cover_df$cellID) %>% 
    dplyr::select(-cellID)
  #### Generate output object
  out <- data.frame(perc_area_protected = NA, perc_target_protected_CAZ = NA, perc_target_protected_ABF = NA)
  #### Only targets within grid cells with >= threshold percentage area of each conservation priority are represented
  ### Calculate level of representation based on greedy algorithm target
  perc_area_protected <- (nrow(target_data_biome)/nrow(reference_df)) * 100
  perc_target_protected_CAZ <- target_accumulation_CAZ(target_data_biome %>% dplyr::select(-x, -y))
  ### Calculate level of representation based on ABF algorithm target
  perc_target_protected_ABF <- target_accumulation_ABF(target_data_biome %>% dplyr::select(-x, -y), ranges = range_sizes)
  ### Populate output
  out$perc_area_protected <- perc_area_protected
  out$perc_target_protected_CAZ <- perc_target_protected_CAZ[length(perc_target_protected_CAZ)] 
  out$perc_target_protected_ABF <- perc_target_protected_ABF[length(perc_target_protected_ABF)]
    
  return(out)
}
