#' @name project_plug_and_play
#' @title Projects fitted plug-and-play distribution models onto new covariates.
#' @description This function projects fitted plug-and-play species distribution models onto new covariates.
#' @param pnp_model A fitted plug-and-play model produced by `fit_plug_and_play`
#' @param data covariate data
#' @return A vector of relative suitabilities evaluates at the covariates supplied in the data object.
#' @export
#' @note The tsearchn function underlying rangebagging seems to fail sometimes with very uneven predictors. Rescaling helps.
project_plug_and_play <- function(pnp_model, data) {

  #Check that pnp_model is the correct class

  if(!inherits(x = pnp_model, what = "pnp_model")) {
    stop("Invalid pnp_model supplied.")
  }

  #Fit numerator

  if(pnp_model$f1_bs) {
    #If bs WAS done, use a do.call for each iteration and average them
    f1_est <- 0

    #It would be good to include a foreach option
    for(i in 1:length(pnp_model$f1)){
      f1_est <-
        f1_est +
        do.call(what = paste('pnp_',pnp_model$f1_method,sep = ""),
                list(data = data,
                     method = "predict",
                     object = pnp_model$f1[[i]]))


    }

    f1_est <- f1_est/length(pnp_model$f1)


  }else{

    #If bs wasn't done, just use one do.call
    f1_est <- do.call(what = paste('pnp_', pnp_model$f1_method, sep = ""),
                      list(data = data,
                           method = "predict",
                           object = pnp_model$f1))


  }



  #Fit denominator

  if(pnp_model$f0_bs) {
    #If bs WAS done, use a do.call for each iteration and average them
    f0_est <- 0

    #It would be good to include a foreach option
    for(i in 1:length(pnp_model$f0)){
      f0_est <-
        f0_est +
        do.call(what = paste('pnp_',pnp_model$f0_method,sep = ""),
                list(data = data,
                     method = "predict",
                     object = pnp_model$f0[[i]]))


    }

    f0_est <- f0_est/length(pnp_model$f0)


  }else{

    #If bs wasn't done, just use one do.call
    f0_est <- do.call(what = paste('pnp_',pnp_model$f0_method,sep = ""),
                      list(data = data,
                           method = "predict",
                           object = pnp_model$f0))


  }


  #If someone made an empty model, return nothing
  if(pnp_model$f1_method == "none" & pnp_model$f0_method == "none"){

    return(invisible(NULL))

  }

  #If presence only
  if(pnp_model$f0_method == "none"){

    return(S = exp(f1_est))

  }

  #If backgruond only (not sure why you'd do this, but whatever)
  if(pnp_model$f1_method == "none"){

    return(S = exp(f0_est))

  }

  #If a full presence/background model
  return(S = exp(f1_est - f0_est))



}
