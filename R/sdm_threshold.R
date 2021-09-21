#' @name sdm_threshold
#' @title Thresholds a continuous "suitability" raster to create a binary raster.
#' @description This function thresholds a continuous "suitability" raster to produce a binary presence/absence raster.
#' @param prediction_raster Raster containing continuous predictions of "suitability" to be thresholded
#' @param occurrence_sp A spatialpoints object containing presence location. in the projection of the prediciton raster
#' @param quantile Numeric between 0 and 1. Quantile to use for thresholding (defaults to 0.05).  Set to 0 for minimum training presence.
#' @param return_binary LOGICAL. Should the raster returned be binary (presence/absence)?  If FALSE, predicted presenced will retain their 'suitability" scores.
#' @keywords internal
#' @importFrom stats quantile
#' @author Cecina Babich Morrow (modified by Brian Maitner)
sdm_threshold <- function(prediction_raster,
                          occurrence_sp,
                          quantile = 0.05,
                          return_binary = TRUE){

  predictions_at_occurrences <- extract(y = occurrence_sp,
                                        x = prediction_raster)

  threshold <- stats::quantile(x = predictions_at_occurrences,
                               probs = quantile,
                               na.rm = T)

  if(return_binary){

    prediction_raster <- prediction_raster >= threshold
    prediction_raster[prediction_raster == 0] <- NA

  }else{

    prediction_raster[which(getValues(prediction_raster) < threshold)] <- NA

  }

  return(prediction_raster)

}

