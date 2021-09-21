#' @title Split data for k-fold spatially stratified cross validation
#' @description Splitting tool for cross-validation
#' @details
#' See Examples.
#' @param occurrence_sp a SpatialPoints or SpatialPointsDataFrame object
#' @param nfolds number of desired output folds.
# @keywords
#' @export
# @examples
#' @return Returns a SpatialPoints dataframe with the data.frame containing fold designation for each point.
#' @author Cory Merow <cory.merow@@gmail.com>
stratify_random <- function(occurrence_sp,
                             nfolds = NULL){


    #Get number of occurrence records
    n <- length(occurrence_sp)

    #Check for sufficient records for CV
    if (n <= 5 | n < 5*nfolds) {
      folds <- rep(1, n)

      occurrence_sp <- SpatialPointsDataFrame(coords = occurrence_sp,
                                              data = data.frame(fold = folds))

      message('Get some more data if you want to use this many folds. All samples assigned to fold 1.')
      return(occurrence_sp)
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

      folds[splits[[i]]]<-i

    }

    #cleanup
    rm(splits)

    #attach data
    occurrence_sp <- SpatialPointsDataFrame(occurrence_sp,
                                            data.frame(fold = folds))

    return(occurrence_sp)

}
