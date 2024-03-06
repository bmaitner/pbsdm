#' @name ensemble_range_map
#' @title Generate ensemble predictions from PBSDM range maps
#' @description This function evaluates model quality and creates an ensemble of the model outputs.
#' This function uses 5-fold, spatially stratified, cross-validation to evaluate distribution model quality.
#' @param occurrences Presence coordinates in long,lat format.
#' @param env Environmental rasters
#' @param method Optional. If supplied, both presence and background density estimation will use this method.
#' @param presence_method Optional. Method for estimation of presence density.
#' @param background_method Optional. Method for estimation of background density.
#' @param bootstrap Character.  One of "none" (the default, no bootstrapping),
#' "numbag" (presence function is bootstrapped),
#' or "doublebag" (presence and background functions are bootstrapped).
#' @param bootstrap_reps Integer.  Number of bootstrap replicates to use (default is 100)
#' @param quantile Quantile to use for thresholding.  Default is 0.05 (5 pct training presence). Set to 0 for minimum training presence (MTP).
#' @param background_buffer_width Numeric or NULL.  Width (meters or map units) of buffer to use to select background environment. If NULL, uses max dist between nearest occurrences.
#' @param constraint_regions See get_env_bg documentation
#' @param ... Additional parameters passed to internal functions.
#' @note Either `method` or both `presence_method` and `background_method` must be supplied.
#' @details Current plug-and-play methods include: "gaussian", "kde","vine","rangebagging", "lobagoc", and "none".
#' Current density ratio methods include: "ulsif", "rulsif".
#' @return List object containing elements (1) spatRaster ensemble layer showing the proportion of maps that are inclued in the range across the ensemble,
#'  (2) spatRasters for individual models, and (3) model quality information.
#' @importFrom pROC roc auc
#' @export
#' @examples {
#'
#'# load packages
#'  library(geodata)
#'
#'  # make temp directory
#'
#'  temp <- tempdir()
#'
#'  # Get some occurrence data
#'
#'  occurrences <- BIEN::BIEN_occurrence_species(species = "Trillium vaseyi",
#'                                               new.world = TRUE,
#'                                               cultivated = FALSE)
#'
#'
#'  # Thin down to unique occurrences
#'  occurrences <- unique(occurrences[c("longitude","latitude")])
#'
#'  # Get bioclim data
#'
#'  env <- worldclim_global(var = "bio",
#'                          res = 10,
#'                          path = temp)
#'
#'
#'  env <- env[[c(1,12)]]
#'
#'  ensemble <- ensemble_range_map(occurrences = occurrences,
#'                                 env = env,
#'                                 method = NULL,
#'                                 presence_method = c("gaussian", "rangebagging"),
#'                                 background_method = "gaussian",
#'                                 bootstrap = "numbag",
#'                                 bootstrap_reps = 10,
#'                                 quantile = 0.05)
#' }
ensemble_range_map <- function(occurrences,
                               env,
                               method = NULL,
                               presence_method = NULL,
                               background_method = NULL,
                               bootstrap = "none",
                               bootstrap_reps = 100,
                               quantile = 0.05,
                               constraint_regions = NULL,
                               background_buffer_width = NULL,
                               ...){


  # Check that methods were supplied
    if(is.null(method) & (is.null(presence_method) &
                          is.null(background_method))) {
      stop("Please supply either (1) method, or (2) both presence_method and background_method")
    }

  # Assign methods if needed
  if(!is.null(method)) {

    presence_method <- method
    background_method <- method

  }

  # assign NULL method as NA
  if(is.null(method)){method <- NA}


  models_to_use <- data.frame(method = method,
                              presence_method = presence_method,
                              background_method = background_method,
                              bootstrap = bootstrap,
                              bootstrap_reps = bootstrap_reps,
                              quantile = quantile)


    quality_list <- list()

    for(i in 1:nrow(models_to_use)){

      map_i_quality <- evaluate_range_map(occurrences = occurrences,
                                          env = env,
                                          method = models_to_use$method[i],
                                          presence_method = models_to_use$presence_method[i],
                                          background_method = models_to_use$background_method[i],
                                          bootstrap = models_to_use$bootstrap[i],
                                          bootstrap_reps = models_to_use$bootstrap_reps[i],
                                          quantile = models_to_use$quantile[i],
                                          width = background_buffer_width,
                                          constraint_regions = constraint_regions)

      map_i <- make_range_map(occurrences = occurrences,
                              env = env,
                              method = models_to_use$method[i],
                              presence_method = models_to_use$presence_method[i],
                              background_method = models_to_use$background_method[i],
                              bootstrap = models_to_use$bootstrap[i],
                              bootstrap_reps = models_to_use$bootstrap_reps[i],
                              quantile = models_to_use$quantile[i],
                              background_buffer_width = background_buffer_width)


    quality_list[[i]] <- map_i_quality

    if(i == 1){

      map_stack <- map_i

    }else{

      map_stack <- c(map_stack,map_i)

    }


    }#for loop


    # Make ensemble map (fraction of votes)

      ensemble <- app(x = map_stack,
                      fun=function(x){!is.na(x)})

      ensemble <- sum(ensemble)/nlyr(map_stack)

      names(ensemble) <- "consensus"

    output <- list( ensemble,
                    map_stack,
                    quality_list)

    names(output) <- c("ensemble_map",
                       "map_stack",
                       "quality_list" )


    return(output)

}#end fx
