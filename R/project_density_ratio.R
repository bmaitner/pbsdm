#' @name project_density_ratio
#' @title Projects fitted density-ratio distribution models onto new covariates.
#' @description This function projects fitted density-ratio species distribution models onto new covariates.
#' @param dr_model A fitted density ratio model produced by `fit_density_ratio`
#' @param data covariate data
#' @return A vector of relative suitabilities evaluates at the covariates supplied in the data object.
#' @export
project_density_ratio <- function(dr_model, data) {

  #Check that pnp_model is the correct class
  if(class(dr_model) != "dr_model") {
    stop("Invalid dr_model supplied.")
  }

  #Fit model
    S_est <- do.call(what = paste('dr_', dr_model$method, sep = ""),
                      list(projection_data = data,
                           method = "predict",
                           object = dr_model$ratio))

  #Return
  return(S = S_est)



}
