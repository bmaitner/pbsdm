#' @name dr_ulsif
#' @title Density-ratio SDM estimation with uLSIF
#' @param presence_data dataframe of covariates
#' @param background_data dataframe of covariates
#' @param projection_data dataframe of covariates
#' @param sigma Sigma parameter for uLSIF. Default is the uLSIF default.
#' @param lambda Lambda parameter for uLSIF. Default is the uLSIF default.
#' @param kernel_num kernel_number for uLSIF. Default is the uLSIF default.
#' @param method one of either "fit" or "predict"
#' @param object fitted object returned by a dr_... function. Only needed when method = "predict"
#' @description dr_ulsif is an internal function for density-ratio estimation with uLSIF
#' @importFrom densratio uLSIF
#' @keywords internal
dr_ulsif <- function(presence_data = NULL,
                     background_data = NULL,
                     projection_data = NULL,
                     sigma = 10^seq(-3, 1, length.out = 9),
                     lambda = 10^seq(-3, 1, length.out = 9),
                     kernel_num = 100,
                     verbose = FALSE,
                     method,
                     object = NULL){

  #Code to check inputs
  if(method=="fit" & (is.null(presence_data) | is.null(background_data) )){
  stop("When fitting a ulsif, supply both presence and abscence data")

  }

  if(method=="predict" & (is.null(projection_data) )){
    stop("When predicting with ulsif, supply projection data")

  }


  #Code for fitting
  if(method == "fit"){

    ratio  <- densratio::uLSIF(x1 = presence_data,
                       x2 = background_data,
                       sigma = sigma,
                       lambda = lambda,
                       kernel_num = kernel_num,
                       verbose = verbose)

    model <- list(ratio = ratio,
                  method = "ulsif")

    class(model) <- "dr_estimate"
    return(model)

  }

  #Code for predicting

  if(method == "predict"){

    prediction <- object$ratio$compute_density_ratio(x = projection_data)

    return(prediction)
  }

}
