#' @name get_env_bg
#' @title Extract background data for SDM fitting.
#' @description This function extracts background data around known presence records.
#' @param coords Coordinates (long,lat) to extract values for
#' @param env Environmental rasterstack in any projection
#' @param method Methods for getting bg points. Current option is buffer
#' @param width Numeric or NULL.  Width (meters or map units) of buffer. If NULL, uses max dist between nearest occurrences.
#' @param constraint_regions An optional spatialpolygons* object that can be used to limit the selection of background points.
#' @param standardize Logical. If TRUE, the variables will be scaled and centered
#' @returns A list containing 1) the background data, 2) the cell indices for which the background was taken
#' @note If supplying constraint_regions, any polygons in which the occurrences fall are considered fair game for background selection.
#' This background selection is, however, still limited by the buffer as well.
#' @importFrom  terra buffer vect extract
#' @importFrom stats complete.cases
#' @export
#' @examples {
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
#' occurrences <- BIEN::BIEN_occurrence_species(species = "Trillium vaseyi",
#'                                              new.world = TRUE,
#'                                              cultivated = FALSE)
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
get_env_bg <- function(coords,
                       env,
                       method = "buffer",
                       width = NULL,
                       constraint_regions = NULL,
                       standardize = TRUE) {

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
    buff <- buffer(x = vect(coords),
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


    env_vals  <- extract(x = env,
                   y = buff,
                   cells=TRUE,
                   ID=FALSE)

    # toss and cells that have NA values

      env_vals <- env_vals[complete.cases(env_vals),]

    # output should be a (1) the environmental variables returned and (2) a vector of cell IDS in the buffers.

      buffer_cells <- env_vals$cell

    # remove the cell column now that it isn't needed

      env_vals$cell <- NULL

    #scale and center if needed

      if(standardize){

        env_mean <- colMeans(env_vals)

        env_sd <- apply(X = env_vals,MARGIN = 2,FUN = sd)

        env_vals <- rescale_w_objects(data = env_vals,
                                      mean_vector = env_mean,
                                      sd_vector = env_sd)


      }else{

        # set as NA if scaling not done

        env_mean <- NA
        env_sd <- NA

      }

  return(test <- list(env = env_vals,
                      bg_cells = buffer_cells,
                      env_mean = env_mean,
                      env_sd = env_sd))


}# end fx

#' @name rescale_w_objects
#' @title Rescale a dataset using vectors of means and SDs
#' @author Brian Maitner
#' @description
#' A little function to rescale data using vectors of means and sds
rescale_w_objects <- function(data, mean_vector, sd_vector){

  #?sweep #option
  # out <- sweep(data, 2L,mean_vector , "-") |>
  #   sweep(2L,sd_vector , "/")

  out <- t((t(data) - mean_vector)/sd_vector)

  return(out)

}
#' @name descale_w_objects
#' @title Return scaled variables to the original scale using means and SDs
#' @author Brian Maitner
#' @description
#' A little function to rescale data using vectors of means and sds
descale_w_objects <- function(data, mean_vector, sd_vector){

  #?sweep #option
  # out <- sweep(2L,sd_vector , "*") |>
  #   sweep(data, 2L,mean_vector , "+")

  out <- t((t(data) * sd_vector) + mean_vector)

  return(out)

}

