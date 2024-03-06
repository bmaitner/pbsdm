#' @title Split data for k-fold spatially stratified cross validation
#' @description Splitting tool for cross-validation
#' @details
#' See Examples.
#' @param occurrence_sf a sf object containing occurrence records
#' @param nfolds number of desired output folds.
# @keywords
#' @export
# @examples
#' @return Returns a sf dataframe containing fold designation for each point.
#' @author Cory Merow <cory.merow@@gmail.com>
stratify_random <- function(occurrence_sf,
                             nfolds = NULL){


    #Get number of occurrence records
    n <- nrow(occurrence_sf)

    #Check for sufficient records for CV

    if (n <= 5 | n < 5*nfolds) {

      occurrence_sf$fold = 1

      message('Get some more data if you want to use this many folds. All samples assigned to fold 1.')

      return(occurrence_sf)

    }

    #If there are enough records for CV, continue

    #Make vector of NAs
    folds <- rep(NA,n)

    # Randomly assign records to a fold (split is used to ensure similar sized groups)
    splits <- suppressWarnings(split(x = sample(x = 1:n,
                 size = n,
                 replace = F),
              f = 1:nfolds))

    #populate vector
    for( i in 1:length(splits)){

      folds[splits[[i]]] <- i

    }

    #cleanup
    rm(splits)

    #attach data

    occurrence_sf$fold = folds


    return(occurrence_sf)

}
