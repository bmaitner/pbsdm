#' @name get_env_pres
#' @title Extract presence data for SDM fitting.
#' @description This function extracts presence data at known presence records.
#' @param coords Coordinates (long,lat) to extract values for
#' @param env Environmental rasterstack in any projection
#' @importFrom raster extract
get_env_pres <- function(coords, env) {

  #check for bad coords

    if(max(coords[,1]) > 180 | min(coords[,1]) < -180){
      message("Problematic coords")
      }

    if(max(coords[,2]) > 90 | min(coords[,2]) < -90){
      message("Problematic coords")
      }

coords <- sp::SpatialPoints(coords = coords,
                            proj4string = CRS(projargs = "EPSG:4326"))

coords <- spTransform(x = coords,CRSobj = env@crs)


env_data <- raster::extract(y = coords,x = env)

nas <- which(apply(X = env_data,
                   MARGIN = 1,
                   FUN = function(x){
                     any(is.na(x))
                     }
                   ))

if(length(nas)>0){message(length(nas)," point(s) with NA values, removing from data \n")}

  env_data <- env_data[setdiff(x = 1:nrow(env_data),y = nas),]

  coords <- coords[setdiff(x = 1:length(coords),y = nas),]

return(test <- list(env = env_data,
                    occurrence_sp = coords))


}# end fx

