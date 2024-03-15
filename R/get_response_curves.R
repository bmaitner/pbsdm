#' Generate Response Curves
#'
#' Given an environmental data set, fitted models, and a directory to output plots, this function generates response curves for each predictor in the model. The response curves depict the predicted change in probability of presence as a function of the environmental predictor while holding all other predictors constant at their mean values.
#'
#' @param dirs A list of directories that includes the algorithm directory and diagnostics directory.
#' @param env An \code{RasterStack} or \code{RasterBrick} of environmental predictors.
#' @param stats A data.frame that contains information about each model that was fit, including model names, species names, and which algorithm was used.
#' @param bg An \code{OccurrenceData} object containing the background data for each species.
#' @param toDo A list of parameters that specifies the details of the analysis to be run.
#' @param best.var A vector of character strings that specifies the environmental predictors to include in the response curves.
#' @param priors A list that contains prior distributions for each parameter in the model.
#' @param pres An \code{OccurrenceData} object containing the presence data for each species.
#' @param envMeans A vector of means for each environmental predictor in the dataset.
#' @param envSDs A vector of standard deviations for each environmental predictor in the dataset.
#' @param extrapLimits A list of limits to use for extrapolation of the response curves.
#' @param openFigs A logical value indicating whether to open the generated plots.
#'
#' @return This function generates a set of PDF files in the diagnostics directory for each species, with one file per model. The PDF files contain plots of the response curves for each environmental predictor in the model.
#'
#' @export
#' @author Cory Merow, modified by Brian Maitner

get_response_curves <- function(env_bg,
                                env_pres,
                                pnp_model,
                                n.int = 200,
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

  pd <- apply(X = env_pres$env,
        MARGIN = 2,
        FUN = function(x){density(x,adjust=2)})

  bgd <- apply(X = env_bg$env,
               MARGIN = 2,
               FUN = function(x){density(x,adjust=2)})


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


    pr <- project_plug_and_play(pnp_model = pnp_model,
                                data = newdat)


    # estimate background density at new points
      bgd_newdat <- approx(x = bgd[[best.var[k]]]$x,
                           y = bgd[[best.var[k]]]$y,
                           xout = newdat[best.var[[k]]] %>% unlist())

      bgd_newdat$y[which(is.na(bgd_newdat$y))] <- 0

    # estimate presence density at new points
      pd_newdat <- approx(x = pd[[best.var[k]]]$x,
                           y = pd[[best.var[k]]]$y,
                           xout = newdat[best.var[[k]]] %>% unlist())

      pd_newdat$y[which(is.na(pd_newdat$y))] <- 0

    out_k <- data.frame(variable = best.var[[k]],
               x_values = newdat[best.var[[k]]] %>% unname(),
               prediction = pr,
               background_density = bgd_newdat$y,
               presence_density = pd_newdat$y
               ) %>%`rownames<-`(NULL)

    out_k %>%
      mutate(std_pred = prediction/sum(prediction)*sum(background_density))%>%
      ggplot(mapping = aes(x=x_values,
                           y=std_pred))+
      geom_line(color="blue",size=1.5)+
      geom_line(mapping = aes(x=x_values,y=presence_density))+
      geom_line(mapping = aes(x=x_values,y=background_density))+
      ylab(NULL)+
      xlab(best.var[[k]])

    marginal_vals <- marginal_vals %>%
      dplyr::bind_rows(out_k)

  }

  return(marginal_vals)

}
