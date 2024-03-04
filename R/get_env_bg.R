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
#' @examples
#'

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
#' range <- BIEN::BIEN_ranges_load_species(species = "Xanthium strumarium")
#'
#'
#' bg_data <- get_env_bg(coords = occurrences,
#'                       env = env,
#'                       method = "buffer",width = 100,
#'                       constraint_regions = range)
#'
#'
#' }






get_env_bg <- function(coords, env, method = "buffer", width = NULL, constraint_regions = NULL) {

  #check for bad coords

  if(max(coords[,1]) > 180 | min(coords[,1]) < -180){
    message("Problematic coords")
  }

  if(max(coords[,2]) > 90 | min(coords[,2]) < -90){
    message("Problematic coords")
  }

  #Convert to sf

    coords <- st_as_sf(x = coords,
                       coords = c(1,2))

    st_crs(coords) <- st_crs("WGS84")


  #convert to env raster projection

    coords <-
      coords %>%
      st_transform(crs = st_crs(env))

  #set buffer size based on coordinate distances if distance null

  if(is.null(width)){

    dists <- st_distance(coords)

    #width <- max(dists[which(dists > 0)]) #commenting this out because I don't remember why I had that > 0 in there...

    width <- max(dists) %>% as.numeric()

    rm(dists)

  }

  #make buffer

    #this is faster than st_buffer, so using this
    buff <- terra::buffer(x = vect(coords),
                  width = width)


  #Optionally, limit by polygons supplied

    if(!is.null(constraint_regions)){

      #convert to env raster projection

        constraint_regions <-
        constraint_regions %>%
          st_transform(crs = st_crs(env)) %>%
          st_make_valid()

        suppressWarnings(
        buff <- st_intersection(x = buff %>%
                          st_as_sf(),
                        y = constraint_regions[coords,]) %>% vect()
        )

      } # end constraint region bit


    env_vals  <- terra::extract(x = env,
                   y = buff,
                   cells=TRUE,
                   ID=FALSE)

    # toss and cells that have NA values

      env_vals <- env_vals[complete.cases(env_vals),]

    # output should be a (1) the environmental variables returned and (2) a vector of cell IDS in the buffers.

      buffer_cells <- env_vals$cell

    # remove the cell column now that it isn't needed

      env_vals$cell <- NULL

  return(test <- list(env = env_vals,
                      bg_cells = buffer_cells))


}# end fx
