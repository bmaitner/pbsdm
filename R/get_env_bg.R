#' @name get_env_bg
#' @title Extract background data for SDM fitting.
#' @description This function extracts background data around known presence records.
#' @param coords Coordinates (long,lat) to extract values for
#' @param env Environmental rasterstack in any projection
#' @param method Methods for getting bg points. Current option is buffer
#' @param width Numeric or NULL.  Width (meters or map units) of buffer. If NULL, uses max dist between nearest occurrences.
#' @param constraint_regions An optional spatialpolygons* object that can be used to limit the selection of background points.
#' @param returns A list containing 1) the background data, 2) the cell indices for which the background was taken
#' @note If supplying constraint_regions, any polygons in which the occurrences fall are considered fair game for background selection.
#' This background selection is, however, still limited by the buffer as well.
#' @importFrom raster intersect buffer
get_env_bg <- function(coords, env, method = "buffer", width = NULL, constraint_regions = NULL) {

  #check for bad coords

  if(max(coords[,1]) > 180 | min(coords[,1]) < -180){
    message("Problematic coords")
  }

  if(max(coords[,2]) > 90 | min(coords[,2]) < -90){
    message("Problematic coords")
  }

  #Convert to spatialpoints
  coords <- sp::SpatialPoints(coords = coords,
                              proj4string = CRS(projargs = "EPSG:4326"))

  #convert to env raster projection
  coords <- spTransform(x = coords,
                        CRSobj = env@crs)

  #set buffer size based on coordinate distances if distance null
  if(is.null(width)){

  dists <- spDists(coords)

    #This commented out code can be used to get max nearest-neighbor distance instead
    # max(apply(X = spDists(coords),
    #       MARGIN = 1,
    #       FUN = function(x){
    #         min(x[which(x>0)])
    #         }
    #       ))

    width <- max(dists[which(dists > 0)])

    #Transform the distance to meters if not projected, as needed by buffer
    if(!is.projected(coords)){width <- width*1000}
    rm(dists)

  }


  #make buffer
  buff <-
  raster::buffer(x = coords,
         width = width)

  # plot(env[[1]])
  # plot(buff,add=TRUE)



  #Optionally, limit by polygons supplied

  if(!is.null(constraint_regions)){

    #convert to env raster projection
    constraint_regions <- spTransform(x = constraint_regions,
                          CRSobj = env@crs)

    buff <- raster::intersect(x = buff,
                           y = constraint_regions[coords,])

    buff <- raster::aggregate(buff)

    }




  #remove any partial NAs from buffer(since we don't want to use them)
  buff_rast <- rasterize(y = env,
                         x = buff)
  buffer_cells <- which(getValues(buff_rast) == 1)

  env <- do.call(rbind,
                 raster::extract(y = buff,x = env))

  na_or_not <-
    apply(X = env,
          MARGIN = 1,
          FUN = function(x){
            any(is.na(x))

          }
    )

  buffer_cells <- buffer_cells[which(!na_or_not)]

  env <- env[which(!na_or_not),]


  if(dim(env)[1]!=length(buffer_cells)){stop("Something wrong with get_env_bg")}


  return(test <- list(env = env,
                      bg_cells = buffer_cells))


}# end fx
