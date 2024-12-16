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
#' @examples \donttest{
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
#'  # Get presence environmental data
#'
#'   pres_env <- get_env_pres(coords = occurrences,
#'                            env = env)
#'
#' # Get background environmental data
#'
#'  bg_env <- get_env_bg(coords = occurrences,
#'                       env = env,width = 100000)
#'
#'
#' # Note that the functions to get the environmental data return lists,
#' # and only the "env" element of these is used in the fit function
#'
# rulsif_fit <- fit_density_ratio(presence = pres_env$env,
#                                background = bg_env$env,
#                                method = "rulsif")
#'
#' }
fit_density_ratio <- function(presence = NULL,
                              background = NULL,
                              method = NULL,
                              ...){
  #Check data and method

  current_modules <-     current_modules <- get_functions(type = "dr") |>
    gsub(pattern = "dr_",replacement = "")

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

##################################



