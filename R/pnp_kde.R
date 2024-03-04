#' @name pnp_kde
#' @title Internal function for fitting KDE distributions in plug-and-play SDMs.
#' @description This function both fits Kernel Density Estimation (KDE) distributions and projects those distributions to new covariates..
#' @param data dataframe of covariates
#' @param method one of either "fit" or "predict"
#' @param bwmethod Bandwidth method to use.  One of 'normal-reference' (the default),'cv.ml', or 'cv.ls'
#' @param object fitted object returned by a pnp_... function. Only needed when method = "predict"
#' @keywords internal
#' @importFrom np npudens npudensbw
#' @importFrom stats fitted
pnp_kde <- function(data, method, bwmethod = 'normal-reference', object = NULL, ...){

  #Code to check inputs

  #Code for fitting
  if(method == "fit"){

    f <- np::npudens(data, bwmethod = bwmethod)

    model <- list(f = f,
                  method = "kde",
                  bwmethod = bwmethod)

    class(model) <- "pnp_estimate"
    return(model)

  }

  #Code for predicting

  if(method == "predict"){

    #log convert for consistency with other functions
    prediction <- log(stats::fitted(np::npudens(bws = object$f$bws,
                                            edat = data,
                                            bwmethod = object$bwmethod)))


    return(prediction)
  }

}
