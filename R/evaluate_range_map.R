#' @name evaluate_range_map
#' @title Evalute PBSDM range map quality
#' @description This function uses 5-fold, spatially stratified, cross-validation to evaluate distribution model quality.
#' @param occurrences Presence coordinates in long,lat format.
#' @param env Environmental rasters
#' @param method Optional. If supplied, both presence and background density estimation will use this method.
#' @param presence_method Optional. Method for estimation of presence density.
#' @param background_method Optional. Method for estimation of background density.
#' @param bootstrap Character.  One of "none" (the default, no bootstrapping),
#' "numbag" (presence function is bootstrapped),
#' or "doublebag" (presence and background functions are bootstrapped).
#' @param bootstrap_reps Integer.  Number of bootstrap replicates to use (default is 100)
#' @param quantile Quantile to use for thresholding.  Default is 0.05 (5 pct training presence). Set to 0 for minimum training presence (MTP).
#' @param constraint_regions See get_env_bg documentation
#' @param background_buffer_width Numeric or NULL.  Width (meters or map units) of buffer to use to select background environment. If NULL, uses max dist between nearest occurrences.
#' @param standardize_preds Logical. Should environmental layers be scaled? Default is TRUE.
#' @param ... Additional parameters passed to internal functions.
#' @note Either `method` or both `presence_method` and `background_method` must be supplied.
#' @details Current plug-and-play methods include: "gaussian", "kde","vine","rangebagging", "lobagoc", and "none".
#' Current density ratio methods include: "ulsif", "rulsif".
#' @importFrom pROC roc auc
#' @importFrom stats cor na.omit
#' @importFrom terra setValues extract values<- varnames<-
#' @import sf
#' @export
#' @examples{
#'
#'
# load packages
#'  library(geodata)
#'
#'# make temp directory
#'
#'  temp <- tempdir()
#'
#'# Get some occurrence data
#'
#'  occurrences <- BIEN::BIEN_occurrence_species(species = "Trillium vaseyi",
#'                                               new.world = TRUE,
#'                                               cultivated = FALSE)
#'
#'# Thin down to unique occurrences
#'
#'  occurrences <- unique(occurrences[c("longitude","latitude")])
#'
#'# Get bioclim data
#'
#'  env <- worldclim_global(var = "bio",
#'                          res = 10,
#'                          path = temp)
#'
#'  env <- env[[c(1,12)]]
#'
#'# Evaluate a gaussian/gaussian model calculated with the numbag approach
#'# using 10 bootstrap replicates.
#'
#'  evaluate_range_map(occurrences = occurrences,
#'                     env = env,
#'                     method = NULL,
#'                     presence_method = "gaussian",
#'                     background_method = "gaussian",
#'                     bootstrap = "numbag",
#'                     bootstrap_reps = 10,
#'                     quantile = 0.05,
#'                     constraint_regions = NULL,
#'                     background_buffer_width = NULL)
#'
#'
#'
#' }
evaluate_range_map <- function(occurrences,
                               env,
                               method = NULL,
                               presence_method = NULL,
                               background_method = NULL,
                               bootstrap = "none",
                               bootstrap_reps = 100,
                               quantile = 0.05,
                               constraint_regions = NULL,
                               background_buffer_width = NULL,
                               standardize_preds = TRUE,
                               ...){

  #Little internal function to handle nulls in method

  robust_in <- function(element,set){
    if(is.null(element)){
      return(FALSE)
    }else{
      if(element %in% set){
        return(TRUE)
      }else{return(FALSE)}
    }
  }#end robust in


  # Check that methods were supplied

  if(is.null(method) & (is.null(presence_method) &
                        is.null(background_method))) {
    stop("Please supply either (1) method, or (2) both presence_method and background_method")
  }

  if(is.null(method)){method <- NA}

  # Assign methods if needed
  if(!is.na(method)) {

    presence_method <- method
    background_method <- method

  }

    # Note that this is overkill, since we don't need the presence data, just the sf file, but it prevents code duplication.


          # Make template

              template <- env[[1]]
              template[1:ncell(template)] <- 1:ncell(template)

              bg_data <- get_env_bg(coords = occurrences,
                                    env = env,
                                    method = "buffer",
                                    width = background_buffer_width,
                                    constraint_regions = constraint_regions,
                                    standardize = standardize_preds)


              presence_data <- get_env_pres(coords = occurrences,
                                            env = env,
                                            env_bg = bg_data)

              #Divide data into folds
              presence_data$occurrence_sf <- stratify_spatial(occurrence_sf = presence_data$occurrence_sf,
                                                              nfolds = NULL,
                                                              nsubclusters = NULL)




              # Make empty output

              out <- data.frame(fold = 1:length(unique(presence_data$occurrence_sf$fold)),
                                training_AUC = NA,
                                training_pAUC_specificity = NA,
                                training_pAUC_sensitivity = NA,
                                testing_AUC = NA,
                                testing_pAUC_specificity = NA,
                                testing_pAUC_sensitivity = NA,
                                testing_DOR = NA,
                                testing_prediction_accuracy = NA,
                                testing_sensitivity = NA,
                                testing_specificity = NA,
                                testing_correlation = NA,
                                testing_kappa = NA)

              out_full <- data.frame(full_AUC = NA,
                                     full_pAUC_specificity = NA,
                                     full_pAUC_sensitivity = NA,
                                     full_correlation = NA)

              #iterate through folds
              for(i in 1:length(unique(presence_data$occurrence_sf$fold))){

                #Fit models

                #If density ratio was supplied
                #if(method %in% c("ulsif", "rulsif")){
                if(robust_in(element = method,set = c("ulsif","rulsif","maxnet"))){

                  model <- fit_density_ratio(presence = presence_data$env[which(presence_data$occurrence_sf$fold != i),],
                                             background = bg_data$env,
                                             method = method)

                }else{


                  model <- fit_plug_and_play(presence = presence_data$env[which(presence_data$occurrence_sf$fold != i),],
                                             background = bg_data$env,
                                             method = method,
                                             presence_method = presence_method,
                                             background_method = background_method,
                                             bootstrap = bootstrap,
                                             bootstrap_reps = bootstrap_reps)


                }


                #Project model to background points


                #if(method %in% c("ulsif", "rulsif")){

                if(robust_in(element = method,set = c("ulsif","rulsif","maxnet"))){

                  predictions <- project_density_ratio(dr_model = model,
                                                       data = bg_data$env)

                }else{


                  predictions <- project_plug_and_play(pnp_model = model,
                                                       data = bg_data$env)


                }

                #Convert predictions to a raster

                prediction_raster <- setValues(env[[1]],
                                               values = NA)

                prediction_raster[bg_data$bg_cells] <- predictions

                names(prediction_raster) <- "suitability"
                varnames(prediction_raster) <- "suitability"


                #Model performance stats on withheld data
                  # AUC
                    #first, need to a dataframe describing suitability scores and whether they contain presences


                    fold_presence_cells <-
                      extract(x = template,
                              y = presence_data$occurrence_sf[which(presence_data$occurrence_sf$fold != i),],
                              ID = FALSE) %>%
                      unique() %>%
                      unlist() %>%
                      as.numeric()

                    fold_pseudoabscence_cells <- setdiff(x = bg_data$bg_cells,
                                                         y = fold_presence_cells)

                    fold_testing_cells <-
                      extract(x = template,
                              y = presence_data$occurrence_sf[which(presence_data$occurrence_sf$fold == i),],
                              ID = FALSE) %>%
                      unique() %>%
                      unlist() %>%
                      as.numeric()

                    #length(bg_data$bg_cells) == length(fold_presence_cells)+length(fold_pseudoabscence_cells)

                    fold_training_suitability_v_occurrence <-
                    rbind(data.frame(suitability = prediction_raster[fold_presence_cells],
                                     occurrence = 1),
                          data.frame(suitability = prediction_raster[bg_data$bg_cells],
                                     occurrence = 0))

                    fold_testing_suitability_v_occurrence <-
                      rbind(data.frame(suitability = prediction_raster[fold_testing_cells],
                                       occurrence = 1),
                            data.frame(suitability = prediction_raster[bg_data$bg_cells],
                                       occurrence = 0))



                    #then we feed the suitability and presence/pseudoabscence data into the AUC function to get an AUC

                    #out$AUC[i] <- get_auc(fold_suitability_v_occurrence = fold_suitability_v_occurrence)$AUC

                  #Training data

                    training_roc_obj <- roc(response = fold_training_suitability_v_occurrence$occurrence,
                                   predictor = fold_training_suitability_v_occurrence$suitability,
                                   level = c(0,1),
                                   direction = "<")

                    out$training_AUC[i] <- training_roc_obj$auc

                    out$training_pAUC_specificity[i] <- auc(roc = training_roc_obj,
                        partial.auc = c(.8, 1),
                        partial.auc.correct = TRUE,
                        partial.auc.focus = "specificity")[[1]]

                    out$training_pAUC_sensitivity[i] <- auc(roc = training_roc_obj,
                        partial.auc = c(.8, 1),
                        partial.auc.correct = TRUE,
                        partial.auc.focus = "sensitivity")[[1]]

                  #Testing data

                    testing_roc_obj <- roc(response = fold_testing_suitability_v_occurrence$occurrence,
                                            predictor = fold_testing_suitability_v_occurrence$suitability,
                                           level = c(0,1),
                                           direction = "<")

                    out$testing_AUC[i] <- testing_roc_obj$auc



                    out$testing_pAUC_specificity[i] <- auc(roc = testing_roc_obj,
                                                            partial.auc = c(.8, 1),
                                                            partial.auc.correct = TRUE,
                                                            partial.auc.focus = "specificity")[[1]]

                    out$testing_pAUC_sensitivity[i] <- auc(roc = testing_roc_obj,
                                                            partial.auc = c(.8, 1),
                                                            partial.auc.correct = TRUE,
                                                            partial.auc.focus = "sensitivity")[[1]]



                  # threshold-dependent measures of some sort?

                      binary_map <-
                      sdm_threshold(prediction_raster = prediction_raster,
                              occurrence_sf = presence_data$occurrence_sf,
                              quantile = 0.05,
                              return_binary = TRUE)

                      #Testing (no point in training, since this is driven by the quantile)
                        testing_values <- binary_map[fold_testing_cells]
                        bg_values <- binary_map[bg_data$bg_cells]
                        TP <- length(which(testing_values == 1))
                        FN <- length(which(is.na(testing_values)))
                        TN <- length(which(is.na(bg_values)))
                        FP <- length(which(bg_values == 1))

                        sensitivity <- TP / (TP + FN)
                        specificity <- TN / (FP + TN)
                        #precision <- TP / (TP + FP)
                        DOR <- (TP*TN)/(FP*FN)
                        #F1 <- 2*((precision * sensitivity)/(precision + sensitivity))
                        prediction_accuracy <- (TP+TN)/(TP+TN+FP+FN)
                        P_o <- (TP+TN)/(TP+TN+FP+FN)
                        Ppres <- ((TP+FP)/(TP+TN+FP+FN))*((TP+FN)/(TP+TN+FP+FN))
                        Pabs <- ((FN+TN)/(TP+TN+FP+FN))*((FP+TN)/(TP+TN+FP+FN))
                        P_e <- Ppres+Pabs
                        kappa <- (P_o - P_e)/(1-P_e)


                        out$testing_DOR[i] <- DOR
                        out$testing_prediction_accuracy[i] <- prediction_accuracy
                        out$testing_sensitivity[i] <- sensitivity
                        out$testing_specificity[i] <- specificity
                        out$testing_kappa[i] <- kappa

                        fold_testing_suitability_v_occurrence <- na.omit(fold_testing_suitability_v_occurrence)
                        out$testing_correlation[i] <- cor(fold_testing_suitability_v_occurrence$suitability,fold_testing_suitability_v_occurrence$occurrence)

                #Fit full model

                  #Calculate ROC
                        #Fit models

                        #If density ratio was supplied
                        #if(method %in% c("ulsif", "rulsif")){
                        if(robust_in(element = method,set = c("ulsif","rulsif","maxnet"))){

                          model <- fit_density_ratio(presence = presence_data$env,
                                                     background = bg_data$env,
                                                     method = method)

                        }else{


                          model <- fit_plug_and_play(presence = presence_data$env,
                                                     background = bg_data$env,
                                                     method = method,
                                                     presence_method = presence_method,
                                                     background_method = background_method,
                                                     bootstrap = bootstrap,
                                                     bootstrap_reps = bootstrap_reps)


                        }


                        #Project model to background points


                        # if(method %in% c("ulsif", "rulsif")){
                        if(robust_in(element = method,set = c("ulsif","rulsif","maxnet"))){

                          predictions <- project_density_ratio(dr_model = model,
                                                               data = bg_data$env)

                        }else{


                          predictions <- project_plug_and_play(pnp_model = model,
                                                               data = bg_data$env)


                        }

                        #Convert predictions to a raster
                        prediction_raster <- setValues(env[[1]],
                                                       values = NA)

                        prediction_raster[bg_data$bg_cells] <- predictions

                        full_suitability_v_occurrence <-
                          rbind(data.frame(suitability = extract(x = prediction_raster,
                                                                 y = presence_data$occurrence_sf,
                                                                 ID = FALSE)%>%
                                             unlist() %>%
                                             as.vector(),
                                           occurrence = 1),
                                data.frame(suitability = prediction_raster[bg_data$bg_cells]%>%
                                             unlist()%>%
                                             as.vector(),
                                           occurrence = 0))

                        full_roc_obj <- roc(response = full_suitability_v_occurrence$occurrence,
                                               predictor = full_suitability_v_occurrence$suitability,
                                            level = c(0,1),
                                            direction = "<")



                        out_full$full_pAUC_specificity <- auc(roc = full_roc_obj,
                                                               partial.auc = c(.8, 1),
                                                               partial.auc.correct = TRUE,
                                                               partial.auc.focus = "specificity")[[1]]

                        out_full$full_pAUC_sensitivity <- auc(roc = full_roc_obj,
                                                               partial.auc = c(.8, 1),
                                                               partial.auc.correct = TRUE,
                                                               partial.auc.focus = "sensitivity")[[1]]

                        out_full$full_AUC <- full_roc_obj$auc

                        full_suitability_v_occurrence <- na.omit(full_suitability_v_occurrence)

                        out_full$full_correlation <- cor(x = full_suitability_v_occurrence$suitability,
                                                         y = full_suitability_v_occurrence$occurrence,
                                                         method = "pearson")


                #Threshold full model


              }#i loop


              return(list(fold_results = out,
                          overall_results = out_full))



            }#end fx





