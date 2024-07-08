#' @name fit_plug_and_play
#' @title Fit presence-background distribution models in a plug-and-play framework.
#' @description This function fits presence-background species distribution models for the specified plug-and-play methods.
#' @param presence dataframe of covariates at presence points
#' @param background Optional. Dataframe of covariates at background points
#' @param method Optional. If supplied, both presence and background density estimation will use this method.
#' @param presence_method Optional. Method for estimation of presence density.
#' @param background_method Optional. Method for estimation of background density.
#' @param bootstrap Character.  One of "none" (the default, no bootstrapping),
#' "numbag" (presence function is bootstrapped),
#' or "doublebag" (presence and background functions are bootstrapped).
#' @param bootstrap_reps Integer.  Number of bootstrap replicates to use (default is 100)
#' @param ... Additional parameters passed to internal functions.
#' @note Either `method` or both `presence_method` and `background_method` must be supplied.
#' @details Current methods include: "gaussian", "kde","vine","rangebagging", "lobagoc", and "none".
#' @export
#' @return List of class "pnp_model" containing model objects and metadata needed for projecting the fitted models.
fit_plug_and_play <- function(presence = NULL,
                              background = NULL,
                              method = NULL,
                              presence_method = NULL,
                              background_method = NULL,
                              bootstrap = "none",
                              bootstrap_reps = 100,
                              ...){

  # Check that methods were supplied
  if(is.null(method) & (is.null(presence_method) &
                        is.null(background_method))) {
    stop("Please supply either (1) method, or (2) both presence_method and background_method")
  }


  if(is.null(method)){method <- NA}

  # Assign methods if needed
  if(!is.na(method)) {

    presence_method <- method
    background_method <- method

  }

  #Check that appropriate data were supplied and set parameters if they were
    if(presence_method != "none" & is.null(presence)){

      message("Please supply presence data")
      return(invisible(NULL))

    }else{
      bootstrap_sample_size_numerator <- nrow(presence)

    }

    if(background_method != "none" & is.null(background)){

      message("Please supply background data")
      return(invisible(NULL))

    }else{
      bootstrap_sample_size_denominator <- nrow(background)
    }



  # Check that methods are available

    current_modules <- get_functions(type = "pnp") |>
      gsub(pattern = "pnp_",replacement = "")

  if(!presence_method %in% current_modules) {
    stop(paste("Presence method not implemented. Please select one of: ",
               paste(current_modules,collapse = ", "),".",sep =  ))
  }

  if(!background_method %in% current_modules) {
    stop(paste("Background method not implemented. Please select one of: ",
               paste(current_modules,collapse = ", "),".",sep =  ))
  }

  #Set bootstrapping options

  num_bs <- FALSE
  den_bs <- FALSE

  if(bootstrap %in% c("numbag","doublebag")){
    num_bs <- TRUE
  }

  if(bootstrap == "doublebag"){
    den_bs <- TRUE
  }



  #Fit the numerator
  if(!num_bs){
    f1 <- do.call(what = paste('pnp_', presence_method, sep = ""),
                  list(data = presence, method = "fit", ...))

  }else{

    f1 <- list()

    for(i in 1:bootstrap_reps){

      presence_sample <- presence[sample(x = 1:bootstrap_sample_size_numerator,
                                         size = bootstrap_sample_size_numerator,
                                         replace = T),]

      f1[[i]] <-  do.call(what = paste('pnp_', presence_method, sep = ""),
                          list(data = presence_sample, method = "fit", ...))

    }


  }#end num fit


  #Fit the denominator
  if(!den_bs){

    f0 <- do.call(what = paste('pnp_', background_method, sep = ""),
                  list(data = background, method = "fit", ...))

  }else{

    f0 <- list()

    for(i in 1:bootstrap_reps){

      background_sample <- background[sample(x = 1:bootstrap_sample_size_denominator,
                                             size = bootstrap_sample_size_denominator,
                                             replace = T),]
      f0[[i]] <-  do.call(what = paste('pnp_', background_method, sep = ""),
                          list(data = background_sample, method = "fit",...))


    }
  }

  #Prepare output


  model <- list(f1 = f1,
                f0 = f0,
                f1_method = presence_method,
                f0_method = background_method,
                f1_bs = num_bs,
                f0_bs = den_bs)

  class(model) <- "pnp_model"
  return(model)

}#end fx

