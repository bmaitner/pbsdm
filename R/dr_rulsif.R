#' @name dr_rulsif
#' @title Density-ratio SDM estimation with RuLSIF
#' @param presence_data dataframe of covariates
#' @param background_data dataframe of covariates
#' @param projection_data dataframe of covariates
#' @param sigma Sigma parameter for RuLSIF. Default is the RuLSIF default.
#' @param lambda Lambda parameter for RuLSIF. Default is the RuLSIF default.
#' @param kernel_num kernel_number for RuLSIF. Default is the RuLSIF default.
#' @param alpha Relative parameter.  Defaults to RuLSIF default.
#' @param method one of either "fit" or "predict"
#' @param object fitted object returned by a dr_... function. Only needed when method = "predict"
#' @description dr_rulsif is an internal function for density-ratio estimation with RuLSIF
#' @importFrom densratio RuLSIF
#' @keywords internal
dr_rulsif <- function(presence_data = NULL,
                     background_data = NULL,
                     projection_data = NULL,
                     sigma = 10^seq(-3, 1, length.out = 9),
                     lambda = 10^seq(-3, 1, length.out = 9),
                     alpha = 0.1,
                     kernel_num = 100,
                     verbose = FALSE,
                     method,
                     object = NULL){

  #Code to check inputs
  if(method=="fit" & (is.null(presence_data) | is.null(background_data) )){
    stop("When fitting a rulsif, supply both presence and abscence data")

  }

  if(method=="predict" & (is.null(projection_data) )){
    stop("When predicting with rulsif, supply projection data")

  }


  #Code for fitting
  if(method == "fit"){

    ratio  <- densratio::RuLSIF(x1 = presence_data,
                               x2 = background_data,
                               sigma = sigma,
                               lambda = lambda,
                               alpha = alpha,
                               kernel_num = kernel_num,
                               verbose = verbose)

    model <- list(ratio = ratio,
                  method = "rulsif")

    class(model) <- "dr_estimate"
    return(model)

  }

  #Code for predicting

  if(method == "predict"){

    prediction <- object$ratio$compute_density_ratio(x = projection_data)

    return(prediction)
  }

}
