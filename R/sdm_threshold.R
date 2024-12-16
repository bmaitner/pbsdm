#' @name sdm_threshold
#' @title Thresholds a continuous relative occurrence rate raster to create a binary raster.
#' @description This function thresholds a continuous relative occurrence rate raster to produce a binary presence/absence raster.
#' @param prediction_raster Raster containing continuous predictions of "suitability" to be thresholded
#' @param occurrence_sf An sf object containing presence locations. Should be in the projection of the prediction raster
#' @param quantile Numeric between 0 and 1. Quantile to use for thresholding (defaults to 0.05).  Set to 0 for minimum training presence.
#' @param return_binary LOGICAL. Should the raster returned be binary (presence/absence)?  If FALSE, predicted presences will retain their 'suitability" scores.
#' @export
#' @importFrom stats quantile
#' @author Cecina Babich Morrow (modified by Brian Maitner)
#' @examples {
#'
#'# load in sample data
#'
#' library(S4DM)
#' library(terra)
#'
#' # occurrence points
#'   data("sample_points")
#'   occurrences <- sample_points
#'
#' # environmental data
#'   env <- rast(system.file('ex/sample_env.tif', package="S4DM"))
#'
#' # rescale the environmental data
#'
#'   env <- scale(env)
#'
#'  bg_data <- get_env_bg(coords = occurrences,
#'                        env = env,
#'                        method = "buffer",
#'                        width = 100000)
#'
#'  pres_data <- get_env_pres(coords = occurrences,
#'                            env = env)
#'
#'  pnp_model <-fit_plug_and_play(presence = pres_data$env,
#'                    background = bg_data$env,
#'                    method = "gaussian")
#'
#'  pnp_continuous <- project_plug_and_play(pnp_model = pnp_model,
#'                                          data = bg_data$env)
#'
#'  #Make an empty raster to populate
#'  out_raster <- env[[1]]
#'  values(out_raster) <- NA
#'
#'  # use the bg_data for indexing
#'  out_raster[bg_data$bg_cells] <- pnp_continuous
#'
#'  plot(out_raster)
#'
#'  #convert to a binary raster
#'
#'  out_raster_binary <-
#'    sdm_threshold(prediction_raster = out_raster,
#'                occurrence_sf = pres_data$occurrence_sf,
#'                quantile = 0.05,
#'                return_binary = TRUE)
#'
#'  plot(out_raster_binary)
#'
#' }
sdm_threshold <- function(prediction_raster,
                          occurrence_sf,
                          quantile = 0.05,
                          return_binary = TRUE){

  predictions_at_occurrences <- extract(y = occurrence_sf,
                                        x = prediction_raster,
                                        ID = FALSE)

  threshold <- stats::quantile(x = predictions_at_occurrences,
                               probs = quantile,
                               na.rm = T)


  if(return_binary){

    prediction_raster[prediction_raster < threshold] <- NA
    prediction_raster[prediction_raster >= threshold] <- 1

  }else{

    prediction_raster[prediction_raster < threshold] <- NA

  }

  return(prediction_raster)

}

