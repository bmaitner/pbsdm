#' @name fit_density_ratio
#' @title Fit density-ratio distribution models in a plug-and-play framework.
#' @description This function fits density-ratio species distribution models for the specified density-ratio method.
#' @param presence dataframe of covariates at presence points
#' @param background Dataframe of covariates at background points
#' @param method Character. See "notes" for options.
#' @param ... Additional parameters passed to internal functions.
#' @details Current methods include: "ulsif", "rulsif", "maxnet"
#' @export
#' @return List of class "dr_model" containing model objects and metadata needed for projecting the fitted models.
#' @export
fit_density_ratio <- function(presence = NULL,
                              background = NULL,
                              method = NULL,
                              ...){
  #Check data and method

  #for now do this manually, but once function skeleton is working do this by looking up available internals
  current_modules <- c("ulsif","rulsif","maxnet")

  if(!method %in% current_modules) {
    stop(paste("Method not implemented. Please select one of: ",
               paste(current_modules,collapse = ", "),".",sep =  ))
  }


  #Fit the ratio
  dr <- do.call(what = paste('dr_', method, sep = ""),
                list(presence_data = presence,
                     background_data = background,
                     method = "fit",
                     ...))

  #Prepare output
  model <- list(ratio = dr,
                method = method)

  class(model) <- "dr_model"
  return(model)

}#End fx
