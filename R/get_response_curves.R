#' Generate Response Curves
#'
#' Given an environmental data set, fitted models, and a directory to output plots, this function generates response curves for each predictor in the model. The response curves depict the predicted change in probability of presence as a function of the environmental predictor while holding all other predictors constant at their mean values.
#'
#' @param env_bg Object returned by get_env_bg
#' @param env_pres Object returned by get_env_pres
#' @param pnp_model Object returned  by `fit_plug_and_play` or `fit_density_ratio`
#' @param n.int Number of points along which to calculate the response curve
#' @param envMeans A vector of means for each environmental predictor in the dataset. (not used)
#' @param envSDs A vector of standard deviations for each environmental predictor in the dataset.(not used)
#'
#' @return This function generates a set of marginal predictions for each environmental variable, holding other variables constant
#' @export
#' @author Cory Merow, modified by Brian Maitner
#' @importFrom stats approx density
#' @importFrom dplyr bind_rows
get_response_curves <- function(env_bg,
                                env_pres,
                                pnp_model,
                                n.int = 1000,
                                envMeans = NULL,
                                envSDs = NULL
){


  # get env means and SDs

    if(is.null(envMeans) & all(!is.na(env_bg$env_mean))){

      envMeans <- env_bg$env_mean

    }

    if(is.null(envSDs) & all(!is.na(env_bg$env_sd))){

      envSDs <- env_bg$env_sd

    }

  # make env gradients

    x.vals <- apply(env_bg$env,MARGIN = 2,
                    FUN = function(x){
                      mi <- quantile(x = x,probs = 0.03,na.rm=TRUE)
                      ma <- quantile(x = x,probs = 0.97,na.rm=TRUE)
                      ra <- ma-mi
                      seq(mi-.5*ra,ma+.5*ra, length = n.int)
                    }, simplify = FALSE)

  # make labels on real scale

  best.var <- names(x.vals)

  # if(!is.null(envMeans) & !is.null(envSDs)){
  #
  #   xTickLabs=lapply(seq_along(best.var),function(x){
  #     (x.vals[[x]]*as.numeric(envSDs[x][1])) + as.numeric(envMeans[x][1])
  #   })
  # } else { xTickLabs=x.vals }
  #
  # names(xTickLabs) <- best.var

  if(is.null(envMeans) | is.null(envSDs)) {
    print("Response curves only work if your predictors are standardized. There's no error here; you just won't get response curve plots.")
    return()
  }


  # these density estimates are only used if a pnp presence or abscence function is omitted

    pd <- apply(X = env_pres$env,
                MARGIN = 2,
                FUN = function(x){density(x,adjust=2)})


    bgd <- apply(X = env_bg$env,
                 MARGIN = 2,
                 FUN = function(x){density(x,adjust=2)})

    pd_newdat <- data.frame()
    bgd_newdat <- data.frame()

  # what would be useful:

    # value type background vs presence vs prediction
    # environmental variable
    # value

    marginal_vals <- NULL

  for(k in seq_along(best.var)){

    newdat <- data.frame(matrix(rep(rep(0, n.int),
                                    length(best.var)),
                                ncol = length(best.var)))
    names(newdat) <- best.var
    newdat[best.var[[k]]] <- x.vals[[best.var[k]]]

      if(inherits(pnp_model,"dr_model")){

        pr <- project_density_ratio(dr_model =  pnp_model,
                                    data = newdat)

      }

    if(inherits(pnp_model,"pnp_model")){

      pr <- project_plug_and_play(pnp_model = pnp_model,
                                  data = newdat)

    }


    # get background curve

    # Use different methods depending on whether presence and/or background density were part of the model

    if(inherits(pnp_model,"pnp_model")){

      # presence

      if(pnp_model$f1_bs) {
        #If bs WAS done, use a do.call for each iteration and average them
        f1_est <- 0

        #It would be good to include a foreach option
        for(i in 1:length(pnp_model$f1)){
          f1_est <-
            f1_est +
            do.call(what = paste('pnp_',pnp_model$f1_method,sep = ""),
                    list(data = newdat,
                         method = "predict",
                         object = pnp_model$f1[[i]]))


        }

        pd_newdat <- data.frame(y = f1_est/length(pnp_model$f1) %>%
                                  exp())


      }else{

        #If bs wasn't done, just use one do.call
        pd_newdat <- data.frame(y = do.call(what = paste('pnp_', pnp_model$f1_method, sep = ""),
                      list(data = newdat,
                           method = "predict",
                           object = pnp_model$f1)))

        if(length(pd_newdat) > 0){pd_newdat <- exp(pd_newdat)}


      }

      #background

      if(pnp_model$f0_bs) {
        #If bs WAS done, use a do.call for each iteration and average them
        f0_est <- 0

        #It would be good to include a foreach option
        for(i in 1:length(pnp_model$f0)){
          f0_est <-
            f0_est +
            do.call(what = paste('pnp_',pnp_model$f0_method,sep = ""),
                    list(data = newdat,
                         method = "predict",
                         object = pnp_model$f0[[i]]))


        }

        bgd_newdat <- data.frame(y = f0_est/length(pnp_model$f0) %>% exp())


      }else{

        #If bs wasn't done, just use one do.call
        bgd_newdat <- data.frame(y= do.call(what = paste('pnp_',pnp_model$f0_method,sep = ""),
                       list(data = newdat,
                            method = "predict",
                            object = pnp_model$f0)))

        if(length(bgd_newdat) > 0){bgd_newdat <- exp(bgd_newdat)}


      }



    }


    if(length(pd_newdat) == 0){

      # estimate presence density at new points

      pd_newdat <- approx(x = pd[[best.var[k]]]$x,
                          y = pd[[best.var[k]]]$y,
                          xout = newdat[best.var[[k]]] %>% unlist())

      pd_newdat$y[which(is.na(pd_newdat$y))] <- 0


    }

    if( length(bgd_newdat) == 0){


      # estimate background density at new points
      bgd_newdat <- approx(x = bgd[[best.var[k]]]$x,
                           y = bgd[[best.var[k]]]$y,
                           xout = newdat[best.var[[k]]] %>% unlist())

      bgd_newdat$y[which(is.na(bgd_newdat$y))] <- 0



    }



    out_k <- data.frame(variable = best.var[[k]],
               x_values = newdat[best.var[[k]]] %>% unname(),
               prediction = pr,
               background_density = bgd_newdat$y,
               presence_density = pd_newdat$y
               ) %>%`rownames<-`(NULL)

    # out_k %>%
    #   mutate(std_pred = prediction/sum(prediction)*sum(background_density))%>%
    #   ggplot(mapping = aes(x=x_values,
    #                        y=std_pred))+
    #   geom_line(color="blue",size=1.5)+
    #   geom_line(mapping = aes(x=x_values,y=presence_density))+
    #   geom_line(mapping = aes(x=x_values,y=background_density))+
    #   ylab(NULL)+
    #   xlab(best.var[[k]])

    marginal_vals <- marginal_vals %>%
      dplyr::bind_rows(out_k)

  }

  return(marginal_vals)

}


