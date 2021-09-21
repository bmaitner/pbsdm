#' @name pnp_gaussian
#' @title Internal function for fitting gaussian distributions in plug-and-play SDMs.
#' @description This function both fits distributions and projects those distributions to new covariates..
#' @param data dataframe of covariates
#' @param method one of either "fit" or "predict"
#' @param type one of either "classical", "robust", or "regularized" (the default)
#' @param object fitted object returned by a pnp_... function. Only needed when method = "predict"
#' @importFrom mvtnorm dmvnorm
#' @importFrom corpcor cov.shrink
#' @importFrom robust covRob
#' @keywords internal
pnp_gaussian <- function(data, method, type = "regularized", object = NULL){

  #Code to check inputs

  #Code for fitting
  if(method == "fit"){


    if(type == "classical"){
      mean <- colMeans(data) # estimated mean of presence points
      sigma <- stats::cov(data) # estimated covariance of presence points
    }

    if(type == "robust"){

      data.est <- robust::covRob(data)
      mean <- data.est$center        # robust estimated mean
      sigma <- data.est$cov          # robust estimated covariance of presence points
    }

    if(type == "regularized"){

      mean <- colMeans(data) # estimated mean of presence points
      data.est <- as.numeric(corpcor::cov.shrink(data)) # robust estimated covariance of presence points
      sigma <- matrix(as.numeric(data.est),
                      nrow = sqrt(length(data.est))) # reformat

    }

    model <- list(mean = mean,
                  sigma = sigma,
                  method = "gaussian")

    class(model) <- "pnp_estimate"
    return(model)

  }

  #Code for predicting

  if(method == "predict"){

    prediction <- mvtnorm::dmvnorm(data,
                                   mean = object$mean,
                                   sigma = object$sigma,
                                   log = TRUE)


    return(prediction)
  }

}
