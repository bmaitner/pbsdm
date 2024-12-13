#'Density-ratio SDM estimation with MAXNET
#'
#'dr_maxnet is an internal function for density-ratio estimation with MAXNET
#' @param presence_data dataframe of covariates
#' @param background_data dataframe of covariates
#' @param projection_data dataframe of covariates
#' @param f MAXNET formula to use. Default (NULL) will use the MAXNET default.
#' @param regmult MAXNET regularization multiplier. Default is 1.
#' @param regfun MAXNET regularization function. Default is the MAXNET default.
#' @param addsamplestobackground If TRUE (the default), any presences that aren't in the background will be added.
#' @param clamp If TRUE (the default), predictions will be limited to ranges seen in the training dataset.
#' @param type Type of response required.  Defaults to link, exponential, cloglog, and logistic.
#' @param method one of either "fit" or "predict"
#' @param object fitted object returned by a dr_... function. Only needed when method = "predict"
#' @import maxnet
#' @importFrom utils getFromNamespace
#' @note The options f, regmult, regfun, and addSamplestobackground are only used when method == "predict",
#' the options clamp and type are only used when method == "predict". See the much better documentation for maxnet for more details.
#' @keywords internal
dr_maxnet <- function(presence_data = NULL,
                      background_data = NULL,
                      projection_data = NULL,
                      f = NULL,
                      regmult = 1,
                      regfun = maxnet.default.regularization,
                      addsamplestobackground = T,
                      clamp = T,
                      verbose = FALSE,
                      method,
                      type = c("link", "exponential", "cloglog", "logistic"),
                      object = NULL){

  #Code to check inputs
  if(method=="fit" & (is.null(presence_data) | is.null(background_data) )){
    stop("When fitting maxnet, supply both presence and abscence data")

  }

  if(method=="predict" & (is.null(projection_data) )){
    stop("When predicting with maxnet, supply projection data")

  }


  #Code for fitting
  if(method == "fit"){

    #Generate presence/abscence vector

    pa_vector <- c(rep(1,nrow(presence_data)),
                   rep(0,nrow(background_data)))


    #Generate covariate data.frame

    pa_covariates <- as.data.frame(rbind(presence_data,
                                         background_data))

    # Specify formula
    if(is.null(f)) {
      f <- maxnet.formula(pa_vector, pa_covariates)
        }

    #Fit model
    ratio  <- maxnet(p = pa_vector,
                             data = pa_covariates,
                             f = f,
                             regmult = regmult,
                             regfun = regfun,
                             addsamplestobackground = addsamplestobackground)


    model <- list(ratio = ratio,
                  method = "maxnet")

    class(model) <- "dr_estimate"
    return(model)

  }

  #Code for predicting

  if(method == "predict"){

    predict.maxnet <- getFromNamespace("predict.maxnet", "maxnet")

    prediction <- predict.maxnet(object = object$ratio,
                                 newdata = projection_data,
                                 clamp = clamp,
                                 type="exponential")

    return(prediction)
  }

}



