#' @name pnp_vine
#' @title Internal function for fitting vine copula distributions in plug-and-play SDMs.
#' @description This function both fits distributions and projects those distributions to new covariates.
#' @param data dataframe of covariates
#' @param method one of either "fit" or "predict"
#' @param object fitted object returned by a pnp_... function. Only needed when method = "predict"
#' @keywords internal
#' @importFrom rvinecopulib vine
#' @importFrom stats fitted
pnp_vine <- function(data, method, object = NULL){

  #Code to check inputs

  #Code for fitting
  if(method == "fit"){

    f <- vine(data = data)

    model <- list(f = f,
                  method = "vine")

    class(model) <- "pnp_estimate"
    return(model)

  }

  #Code for predicting

  if(method == "predict"){

    #log convert for consistency with other functions
    # prediction <- log(dvine(x = data,
    #                         vine = object$f))

    prediction <- log(safe_dvine(x = data,
                                 vine = object$f))

    return(prediction)
  }

}

##############################################
#' @note This version of the dvine function was modified to omit a transpose step that was causing problems with 1-dimensional data
safe_dvine <-
function (x, vine, cores = 1) {
  stopifnot(inherits(vine, "vine_dist"))

  x <- rvinecopulib:::expand_factors(x)
  if (!is.null(vine$names)) {
    x <- x[, vine$names, drop = FALSE]
  }
  d <- ncol(x)
  if (!inherits(vine, "vine") & rvinecopulib:::depth(vine$margins) ==
      1) {
    vine$margins <- replicate(d, vine$margins, simplify = FALSE)
  }
  margvals <- rvinecopulib:::dpq_marg(x, vine, "d")
  if (!is.null(vine$copula)) {
    u <- rvinecopulib:::compute_pseudo_obs(x, vine)
    vinevals <- dvinecop(u, vine$copula, cores)
  }
  else {
    vinevals <- rep(1, nrow(x))
  }
  apply(cbind(margvals, vinevals), 1, prod)
}



