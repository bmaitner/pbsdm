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
#' @examples {
#'
#' # load packages
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
#'  # Thin down to unique occurrences
#'
#'  occurrences <- unique(occurrences[c("longitude","latitude")])
#'
#'  # Get bioclim data
#'
#'  env <- worldclim_global(var = "bio",
#'                          res = 10,
#'                          path = temp)
#'
#'  env <- env[[c(1,12)]]
#'
#'  # Get presence environmental data
#'
#'  pres_env <- get_env_pres(coords = occurrences,
#'                           env = env)
#'  # Get background environmental data
#'
#'  bg_env <- get_env_bg(coords = occurrences,
#'                       env = env)
#'
#'  # Note that the functions to get the environmental data return lists,
#'  # and only the "env" element of these is used in the fit function
#'
#'  ulsif_fit <- fit_density_ratio(presence = pres_env$env,
#'                                  background = bg_env$env,
#'                                  method = "ulsif")
#'}
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



