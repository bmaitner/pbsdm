#' @title Split data for k-fold spatially stratified cross validation
#' @description Splitting tool for cross-validation
#' @details
#' See Examples.
#' @param occurrence_sf a sf object containing occurrence points
#' @param nfolds number of desired output folds. Default value of NULL makes a reasonable guess based on sample size.
#' @param nsubclusters intermediate number of clusters randomly split into nfolds. Default value of NULL makes a reasonable guess based on sample size.
#' If you specify this manually, it should be an integer multiple of nfolds.
# @keywords
#' @export
# @examples
#' @return Returns a SpatialPoints dataframe with the data.frame containing fold designation for each point.
#' @author Cory Merow <cory.merow@@gmail.com>
stratify_spatial <- function(occurrence_sf,
                            nfolds = NULL,
                            nsubclusters = NULL){

  out <- try({


    #Get number of occurrence records
    n <- nrow(occurrence_sf)

    #Check for sufficient records for CV
    if (n <= 5) {

      occurrence_sf$fold = 1

      message('Get some more data if you want to do spatial CV. All samples assigned to fold 1.')

      return(occurrence_sf)

    }

    #If there are enough records for CV, continue

    if(is.null(nfolds)) {

      #Small number of occs
      if (n > 5 & n <= 15) {

        folds.tmp <- kmeans(x = occurrence_sf %>% st_coordinates(),
                            centers = 5)

        folds.tmp1 <- flexclust::as.kcca(folds.tmp,
                                         data = occurrence_sf %>% st_coordinates())

        occurrence_sf$fold = folds.tmp$cluster

      }

      #Medium number
      if (n > 15 & n <= 30) {

        folds.tmp <- kmeans(x = occurrence_sf %>% st_coordinates(),
                            centers = 10)

        folds.tmp1 <- flexclust::as.kcca(folds.tmp,
                                         data= occurrence_sf %>% st_coordinates())

        folds <- folds2 <- folds.tmp$clust

        combine.folds <- matrix(sample(x = 1:nrow(folds.tmp$centers),
                                       size =  10,
                                       replace=F),
                                ncol = 2)

        for(ii in 1:nrow(combine.folds)){
          folds[folds2 %in% c(combine.folds[ii,])] <- ii
        }

        occurrence_sf$fold = folds

      }

      #Large
      if (n > 30 & n <= 45) {

        folds.tmp <- kmeans(x = occurrence_sf %>% st_coordinates(),
                            centers = 15)

        folds.tmp1 <- flexclust::as.kcca(folds.tmp,
                                         data = occurrence_sf %>% st_coordinates())

        folds <- folds2 <- folds.tmp$clust

        combine.folds <- matrix(sample(x = 1:nrow(folds.tmp$centers),
                                       size = 15,
                                       replace = F),
                                ncol=3)
        for(ii in 1:nrow(combine.folds)){
          folds[folds2 %in% c(combine.folds[ii,])] <- ii
        }

        occurrence_sf$fold = folds

      }

      #Extra large yields 5 folds
      if (n > 45 & n <= 60) {

        folds.tmp <- kmeans(x = occurrence_sf %>% st_coordinates(),
                            centers = 20)

        folds.tmp1 <- flexclust::as.kcca(folds.tmp,
                                         data = occurrence_sf %>% st_coordinates())

        folds <- folds2 <- folds.tmp$clust

        combine.folds <- matrix(sample(x = 1:nrow(folds.tmp$centers),
                                       size = 20,
                                       replace = F),
                                ncol = 4)

        for(ii in 1:nrow(combine.folds)){
          folds[folds2 %in% c(combine.folds[ii,])] <- ii
        }

        occurrence_sf$fold = folds

        }

      #Truckstop soda size

      if (n > 60) {

        folds.tmp <- kmeans(occurrence_sf %>% st_coordinates(),25)

        folds.tmp1 <- flexclust::as.kcca(folds.tmp,
                                         data = occurrence_sf %>% st_coordinates())

        folds <- folds2 <- folds.tmp$clust

        combine.folds <- matrix(sample(x = 1:nrow(folds.tmp$centers),
                                       size = 25,
                                       replace = F),
                                ncol = 5)

        for(ii in 1:nrow(combine.folds)){
          folds[folds2%in%c(combine.folds[ii,])] <- ii
        }

        occurrence_sf$fold = folds

      }

    } else { # end default splitting rules

      if(is.null(nsubclusters)){
        stop('you must specify nsubclusters')
      }

      folds.tmp <- kmeans(x = occurrence_sf %>% st_coordinates(),
                          centers = nsubclusters)

      folds <- folds2 <- folds.tmp$clust

      combine.folds <- matrix(sample(1:nrow(folds.tmp$centers),
                                     nsubclusters, replace = F),
                              nrow = nsubclusters,
                              ncol = nfolds)
      for(ii in 1:nrow(combine.folds)){
        folds[folds2 %in% c(combine.folds[ii,])] <- ii
        }

      occurrence_sf$fold = folds

    }

    return(occurrence_sf)
  })
  return(out)
}
