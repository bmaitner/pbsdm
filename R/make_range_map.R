#' @name make_range_map
#' @title Make a range map using plug-and-play modeling.
#' @description This function produces range maps using plug-and-play modeling with either presence-background or density-ratio approaches.
#' @param occurrences Presence coordinates in long,lat format.
#' @param env Environmental rasters
#' @param method Optional. If supplied, both presence and background density estimation will use this method.
#' @param presence_method Optional. Method for estimation of presence density.
#' @param background_method Optional. Method for estimation of background density.
#' @param bootstrap Character.  One of "none" (the default, no bootstrapping),
#' "numbag" (presence function is bootstrapped),
#' or "doublebag" (presence and background functions are bootstrapped).
#' @param bootstrap_reps Integer.  Number of bootstrap replicates to use (default is 100)
#' @param quantile Quantile to use for thresholding.  Default is 0.05 (5 pct training presence). Set to 0 for minimum trainin presence (MTP), set to NULL to return continuous raster.
#' @param background_buffer_width The width (in m for unprojected rasters and map units for projected rasters) of the buffer to use for background data.
#' Defaults to NULL, which will take the maximum distance between occurrence records.
#' @param constraint_regions See get_env_bg documentation
#' @param standardize_preds Logical. Should environmental layers be scaled? Default is TRUE.
#' @param verbose Logical. If TRUE, prints progress messages.
#' @param ... Additional parameters passed to internal functions.
#' @note Either `method` or both `presence_method` and `background_method` must be supplied.
#' @importFrom terra varnames<-
#' @export
#' @details Current plug-and-play methods include: "gaussian", "kde","vine","rangebagging", "lobagoc", and "none".
#' Current density ratio methods include: "ulsif", "rulsif",and "maxnet".
#' @examples {
#'
#'# load in sample data
#'
#'  library(S4DM)
#'  library(terra)
#'
#'  # occurrence points
#'    data("sample_points")
#'    occurrences <- sample_points
#'
#'  # environmental data
#'    env <- rast(system.file('ex/sample_env.tif', package="S4DM"))
#'
#'  # rescale the environmental data
#'
#'    env <- scale(env)
#'
#'    map <- make_range_map(occurrences = occurrences,
#'                          env = env,
#'                          method = "gaussian",
#'                          presence_method = NULL,
#'                          background_method = NULL,
#'                          bootstrap = "none",
#'                          bootstrap_reps = 100,
#'                          quantile = 0.05,
#'                          background_buffer_width = 100000)
#'
#'    plot(map)
#'
#'
#' }
make_range_map <- function(occurrences,
               env,
               method = NULL,
               presence_method = NULL,
               background_method = NULL,
               bootstrap = "none",
               bootstrap_reps = 100,
               quantile = 0.05,
               background_buffer_width = NULL,
               constraint_regions = NULL,
               verbose = FALSE,
               standardize_preds = TRUE,
               ...){


  #Little internal function to handle nulls in method

      robust_in <- function(element,set){
        if(is.null(element)){
          return(FALSE)
        }else{
          if(element %in% set){
            return(TRUE)
          }else{return(FALSE)}
        }
      }#end robust in


  #Get presence and background data
    bg_data <- get_env_bg(coords = occurrences,
                          env = env,
                          method = "buffer",
                          width = background_buffer_width,
                          constraint_regions = constraint_regions,
                          standardize = standardize_preds)

    presence_data <- get_env_pres(coords = occurrences,
                                  env = env,
                                  env_bg = bg_data)




  #If density ratio was supplied
    if(robust_in(element = method,set = c("ulsif", "rulsif","maxnet"))){


      model <- fit_density_ratio(presence = presence_data$env,
                        background = bg_data$env,
                        method = method)

    }else{


      model <- fit_plug_and_play(presence = presence_data$env,
                                 background = bg_data$env,
                                 method = method,
                                 presence_method = presence_method,
                                 background_method = background_method,
                                 bootstrap = bootstrap,
                                 bootstrap_reps = bootstrap_reps)


    }


    #Project model to background points

    if(verbose){message("starting predictions")}

    if(robust_in(element = method,set = c("ulsif", "rulsif","maxnet"))){

      predictions <- project_density_ratio(dr_model = model,
                                     data = bg_data$env)

    }else{


      predictions <- project_plug_and_play(pnp_model = model,
                                           data = bg_data$env)

    }

    if(verbose){message("converting predictions to raster")}

    #Convert predictions to a raster

    if(verbose){message("making empty raster")}

        prediction_raster <- env[[1]]
        values(prediction_raster) <- NA

        message("assigning values")

        prediction_raster[bg_data$bg_cells] <- predictions

        names(prediction_raster) <- "suitability"
        varnames(prediction_raster) <- "suitability"

    #Apply thresholding

    if(verbose){message("thresholding predictions")}

    if(!is.null(quantile)){

      prediction_raster <- sdm_threshold(prediction_raster = prediction_raster,
                                         occurrence_sf = presence_data$occurrence_sf,
                                         quantile = quantile)
    }

    return(prediction_raster)

}


