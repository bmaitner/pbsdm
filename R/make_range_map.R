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
#' @param ... Additional parameters passed to internal functions.
#' @note Either `method` or both `presence_method` and `background_method` must be supplied.
#' @export
#' @details Current plug-and-play methods include: "gaussian", "kde","vine","rangebagging", "lobagoc", and "none".
#' Current density ratio methods include: "ulsif", "rulsif",and "maxnet".
make_range_map <- function(occurrences,
               env,
               method = NULL,
               presence_method = NULL,
               background_method = NULL,
               bootstrap = "none",
               bootstrap_reps = 100,
               quantile = 0.05,
               background_buffer_width = NULL){


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
    presence_data <- get_env_pres(coords = occurrences,
                                  env = env)

    bg_data <- get_env_bg(coords = occurrences,
                          env = env,
                          method = "buffer",
                          width = background_buffer_width)

    # test <- setValues(x = env[[1]],NA)
    # test[bg_data$bg_cells]<-1
    # plot(test)

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

    print("starting predictions")

    if(robust_in(element = method,set = c("ulsif", "rulsif","maxnet"))){

      predictions <- project_density_ratio(dr_model = model,
                                     data = bg_data$env)

    }else{


      predictions <- project_plug_and_play(pnp_model = model,
                                           data = bg_data$env)

    }

    print("converting predictions to raster")

    #Convert predictions to a raster

      print("making empty raster")

      suppressWarnings(prediction_raster <- setValues(env[[1]],
                                                      values = NA))
      # prediction_raster <- setValues(env[[1]],
      #                     values = NA)
      #
      print("assigning values")
      prediction_raster[bg_data$bg_cells] <- predictions


    #Apply thresholding

    print("thresholding predictions")
    if(!is.null(quantile)){
      prediction_raster <- sdm_threshold(prediction_raster = prediction_raster,
                                         occurrence_sp = presence_data$occurrence_sp,
                                         quantile = quantile)
    }




    return(prediction_raster)

}
