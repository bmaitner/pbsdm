#' @name get_env_pres
#' @title Extract presence data for SDM fitting.
#' @description This function extracts presence data at known presence records.
#' @param coords Coordinates (long,lat) to extract values for
#' @param env Environmental rasterstack in any projection
#' @importFrom terra extract
#' @importFrom sf st_as_sf st_crs st_transform
#' @examples  {
#'
#' # load packages
#'  library(geodata)
#'
#' # make temp directory
#'
#'  temp <- tempdir()
#'
#' # Get some occurrence data
#'
#' occurrences <- BIEN::BIEN_occurrence_species(species = "Xanthium strumarium",
#'                                              new.world = T,
#'                                              cultivated = F)
#'
#' # Thin down to unique occurrences
#'
#' occurrences <- unique(occurrences[c("longitude","latitude")])
#'
#' # Get bioclim data
#'
#' env <- worldclim_global(var = "bio",
#'                         res = 10,
#'                         path = temp)
#'
#'
#' env <- env[[c(1,12)]]
#'
#'
#' env_pres <- get_env_pres(coords = occurrences,
#'                         env = env)
#'
#' }
get_env_pres <- function(coords, env) {

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

      env_data <- env_data[setdiff(x = 1:nrow(env_data),y = nas),]

      coords <- coords[setdiff(x = 1:nrow(coords),y = nas),]

    #return output

    return(out <- list(env = env_data,
                        occurrence_sf = coords))


}# end fx

