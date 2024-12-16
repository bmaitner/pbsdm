#' @name get_env_pres
#' @title Extract presence data for SDM fitting.
#' @description This function extracts presence data at known presence records.
#' @param coords Coordinates (long,lat) to extract values for
#' @param env Environmental rasterstack in any projection
#' @param env_bg Background data produced by `get_env_bg`, used for re-scaling
#' @importFrom terra extract
#' @importFrom sf st_as_sf st_crs st_transform
#' @export
#' @examples  {
#'
#'# load in sample data
#'
#'  library(S4DM)
#'  library(terra)
#'
#'  # occurrence points
#'    data("sample_points")
#'    occurrences <- sample_points
#'
#'  # environmental data
#'    env <- rast(system.file('ex/sample_env.tif', package="S4DM"))
#'
#'  # rescale the environmental data
#'
#'    env <- scale(env)
#'
#' env_pres <- get_env_pres(coords = occurrences,
#'                         env = env)
#'
#' }
get_env_pres <- function(coords, env, env_bg = NULL) {

  #check for bad coords

    if(max(coords[,1]) > 180 | min(coords[,1]) < -180){
      message("Problematic coords")
      }

    if(max(coords[,2]) > 90 | min(coords[,2]) < -90){
      message("Problematic coords")
      }

 # convert coordinates to spatial dataset

    coords <- st_as_sf(x = coords,coords = c(1,2))

    st_crs(coords) <- st_crs("WGS84")

  # transform crs to match environmental data

    coords <-
    coords %>%
      st_transform(crs = st_crs(env))

  # Get env data

    env_data <- extract(x = env,
                        y = coords,
                        ID = FALSE)

  # Check for NAs

    nas <- which(apply(X = env_data,
                       MARGIN = 1,
                       FUN = function(x){
                         any(is.na(x))
                         }
                       ))

    if(length(nas) > 0){
      message(length(nas)," point(s) with NA values, removing from data \n")
      }

    # Toss any NAs from both the environmental data and coordinates

      env_data <- env_data[setdiff(x = 1:nrow(env_data),y = nas),,drop=FALSE]

      coords <- coords[setdiff(x = 1:nrow(coords),y = nas),]

    # rescale if background was rescaled

    if(!is.null(env_bg)){

      if(all(!is.na(env_bg$env_mean)) & all(!is.na(env_bg$env_sd))){

        env_data <- rescale_w_objects(data = env_data,
                                      mean_vector = env_bg$env_mean,
                                      sd_vector = env_bg$env_sd
                                      )

      }

      #if some values are NA, but not all, throw an error

        if( (any(is.na(env_bg$env_mean)) | any(is.na(env_bg$env_sd))) &
            !(all(is.na(env_bg$env_mean)) & all(is.na(env_bg$env_sd))) ){
          stop("Some environmental value means and/or SDs are NAs")
          }

      # if a bg was included, but rescaling wasn't done, no need for further action

    }


    #return output

    return(out <- list(env = env_data,
                        occurrence_sf = coords))


}# end fx

