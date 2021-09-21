#' @name pnp_rangebagging
#' @title Internal function for rangebagging in plug-and-play SDMs.
#' @description This function both fits rangebagging distributions and projects those distributions to new covariates.
#' @param data dataframe of covariates
#' @param method one of either "fit" or "predict"
#' @param object fitted object returned by a pnp_... function. Only needed when method = "predict"
#' @param v Integer. Number of votes to use in the aggregation, default is 100.
#' @param d Integer. Number of dimensions (i.e. covariates) to use in aggregations, defaults is 1.
#' @param p Numeric.  Fraction of observations (i.e. occurrences) to use in each replicate aggregation. Default is 0.5
#' @details For fitting, an object is not required (and will be ignored). For prediction, parameters v,p,and d are not needed and will be ignored.
#' @import geometry
#' @keywords internal
pnp_rangebagging <- function(data, method, object = NULL, v = 100, d = 1, p = 0.5){

  #Code to check inputs

  #Code for fitting
  if(method == "fit"){
    models <- list()
    n <- dim(data)
    for(i in 1:v){

      #randomly sample d covariates
      vars <- sample.int(n[2],
                         size = d,
                         replace = FALSE)
      x0 <- data[,vars]

      if(d == 1) {

        #sample some proportion of occurrences, specified by p
        x1 <- x0[sample(n[1],
                        ceiling(p * n[1]),
                        replace = FALSE)]
        #for one d, model is just min/max
        models[[i]] <- list(vars = vars,
                            endpoints = c(min(x1),
                                          max(x1)),
                            data=x1)

      }else{

        x1 <- x0[sample(n[1],
                        ceiling(p*n[1]),
                        replace=FALSE),]

        #THIS DOESNT REALLY DO ANYTHING USEFUL SINCE WE REFIT CONVULL IN FXN RB
        idx <- unique(as.vector(convhulln(x1, options='Pp')))
        endpoints <- x1[idx,]
        models[[i]] <- list(vars=vars, endpoints=endpoints, data=unique(x1))
      }
    }

    model <- list(rangebag_models = models,
                  method = "rangebagging")

    class(model) <- "pnp_estimate"
    return(model)

  }

  #Code for predicting

  if(method == "predict"){

    #set parameters
    v <- length(object$rangebag_models)
    d <- ifelse(is.null(dim(object$rangebag_models[[1]]$endpoints)), 1, dim(object$rangebag_models[[1]]$endpoints)[2])
    n <- dim(data)

    #make empty output
    prediction <- numeric(n[1])

    #Do bagging
    for(i in 1:v){
      #    print(i) # counter for troubleshooting
      if(d == 1){

        test.pts <-
          (object$rangebag_models[[i]]$endpoints[1] <
             data[,object$rangebag_models[[i]]$vars]) &
          (data[,object$rangebag_models[[i]]$vars] <
             object$rangebag_models[[i]]$endpoints[2])

        prediction <- prediction + test.pts

      }else{

        test.dat <- as.matrix(data[,object$rangebag_models[[i]]$vars])

        tri.pts <- tsearchn(as.matrix(object$rangebag_models[[i]]$data),
                            delaunayn(object$rangebag_models[[i]]$data),
                            test.dat)

        #tri.pts <- tsearchn(as.matrix(models[[i]]$endpoints), delaunayn(models[[i]]$endpoints), test.dat)
        test.pts <- !is.na(tri.pts$p[,1])
        prediction <- prediction + test.pts

      }
    }

    prediction <- (prediction/v)

    return(log(prediction))
  }

}


