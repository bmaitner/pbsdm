## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig.show='hold'----------------------------------------------------------


# Load libraries

library(BIEN)
library(geodata)
library(ggplot2)
library(pbsdm)
library(sf)
library(terra)
library(tidyterra)

# Make a temporary directory to store climate data

  temp <- tempdir()

# Get some occurrence data

  tv <- BIEN_occurrence_species(species = "Trillium vaseyi")

# Get environmental data
  
  env <- worldclim_global(var = "bio",
                           res = 10,
                           path = temp)

# To make things a bit faster and easier, we'll limit ourselves to the 2 variables (mean temperature and annual precipitation)

  
  env <- env[[c(1,12)]]
  
# And we'll rescale the variables as well

env <- scale(env)

# Just to take a look to make sure we didn't mess anything up

plot(env)


## ----echo=FALSE, results='asis'-----------------------------------------------

tv_rangebagged <- 
make_range_map(occurrences = tv[c("longitude","latitude")],
               env = env,
               presence_method = "rangebagging",
               background_method = "none",
               background_buffer_width = 100000)


#Lets see what it looks like

#convert to polygon for easier visualization

tv_rangebagged_polygon <-
  tv_rangebagged |>
  as.polygons() |>
  st_as_sf()

# get a bbox for plotting

tv_bbox <-
tv_rangebagged_polygon |>
  st_buffer(dist = 500000) |>
  st_bbox()

#Now, we'll plot the standardized temperature raster, along with the occurrence records and the range map 

ggplot(env)+
  geom_raster(mapping = aes(x=x,y=y,fill=wc2.1_10m_bio_1))+
  scale_fill_viridis_c(name="Temp. C", na.value = "transparent")+
  scale_x_continuous(expand=c(0,0),
                     limits = c(tv_bbox[1],tv_bbox[3]))+
    scale_y_continuous(limits = c(tv_bbox[2],tv_bbox[4]),
                     expand=c(0,0))+
  theme_bw()+
  geom_sf(data = tv_rangebagged_polygon,
          fill = "grey",
          size=2,
          alpha=0.5)+
    geom_point(data = tv,
             mapping = aes(x=longitude,y=latitude))




## -----------------------------------------------------------------------------


# Here, we'll use the same data as before for Trillium vaseyi.

#First, we'll select the background data

tv_bg <- get_env_bg(coords = tv[c("longitude","latitude")],
                    env = env,
                    width = 50000,
                    standardize = TRUE) #note that we used a small set of background points to expedite model fitting

# The returned object 'xs_bg' contains two objects:
  # 1) tv_bg$env a matrix of environmental covariates. This is what we need for modeling.
  # 2) tv_bg$bg_cells a vector containing the environmental raster cell IDs that are present in tv_bg$env. This is useful for mapping the results.

# Next, we get the presence data:

  tv_presence <- get_env_pres(coords = tv[c("longitude","latitude")],
                              env = env,
                              env_bg = tv_bg)

#The returned object 'tv_presence' contains two objects:

  # 1) tv_presence$env a matrix of environmental covariates. This is what we need for modeling.
  # 2) tv_presence$occurrence_sf a sf object containing the coordinate data. This is useful for conducting spatially stratified cross-validation.




# Now, we can fit the model.  Previously we used rangebagging, this time we'll use a simple KDE estimation

  tv_kde_kde <- fit_plug_and_play(presence = tv_presence$env,
                                  background = tv_bg$env,
                                  method = "kde")


# The object that was returned is of the class "pnp_model", which is essentially a list of model fits and associated metadata.

# To view this data on a map, we can project it to the background data we used in fitting (or we could project to a new location entirely). In either case, we use the function `project_plu_and_play`.


  tv_kde_kde_predictions <- project_plug_and_play(pnp_model = tv_kde_kde,
                                                  data = tv_bg$env)



#Now we can make a blank raster to store our predictions
  
  tv_kde_kde_raster <- env[[1]]

  values(tv_kde_kde_raster) <- NA

#Add our predictions to the raster

  tv_kde_kde_raster[tv_bg$bg_cells] <-  tv_kde_kde_predictions

#Now, we can plot our raster

  plot(tv_kde_kde_raster,
       xlim = c(tv_bbox[1],tv_bbox[3]),
       ylim = c(tv_bbox[2],tv_bbox[4]))
  points(tv[c("longitude","latitude")])
  

## ----thresholding-------------------------------------------------------------

#To threshold this continuous raster to yield a binary raster

tv_kde_kde_raster <- sdm_threshold(prediction_raster = tv_kde_kde_raster,
                                   occurrence_sf = tv_presence$occurrence_sf,
                                   quantile = 0.05,
                                   return_binary = T)


# As before, we'll plot this on top of temperature and occurrence records to see how well we did

  # convert to polygon for easier visualization
  
    tv_kde_kde_polygon <-
      tv_kde_kde_raster |>
      as.polygons()|>
      st_as_sf()

# Now, we'll plot the standardized temperature raster, along with the occurrence records and the range map 

  ggplot(env)+
    geom_raster(mapping = aes(x=x,y=y,fill=wc2.1_10m_bio_1))+
    scale_fill_viridis_c(name="Temp. C", na.value = "transparent")+
    scale_x_continuous(expand=c(0,0),
                       limits = c(tv_bbox[1],tv_bbox[3]))+
      scale_y_continuous(limits = c(tv_bbox[2],tv_bbox[4]),
                       expand=c(0,0))+
    theme_bw()+
    geom_sf(data = tv_kde_kde_polygon,
            fill = "grey",
            size=2,
            alpha=0.5)+
      geom_point(data = tv,
               mapping = aes(x=longitude,y=latitude))




## -----------------------------------------------------------------------------

# We'll rely on the same data as last time for simplicity.
# Since we're using different methods for estimating the presence and background distributions, we need to specify these separately:

tv_gaussian_kde <- fit_plug_and_play(presence = tv_presence$env,
                                     background = tv_bg$env,
                                     presence_method = "gaussian",
                                     background_method = "kde")

tv_gaussian_kde_predictions <- project_plug_and_play(pnp_model = tv_gaussian_kde,
                                                data = tv_bg$env)

# Now, we again convert everything to a raster and then to a polygon

  tv_gaussian_kde_raster <- env[[1]]

  values(tv_gaussian_kde_raster) <- NA

  tv_gaussian_kde_raster[tv_bg$bg_cells] <-  tv_gaussian_kde_predictions

# Now, we can plot our raster
  
  plot(tv_gaussian_kde_raster,
       xlim = c(tv_bbox[1],tv_bbox[3]),
       ylim = c(tv_bbox[2],tv_bbox[4]))
  points(tv[c("longitude","latitude")])
  

## ----thresholding gk----------------------------------------------------------
# To threshold this continuous raster to yield a binary raster

  tv_gaussian_kde_raster <- sdm_threshold(prediction_raster = tv_gaussian_kde_raster,
                                     occurrence_sf = tv_presence$occurrence_sf,
                                     quantile = 0.05,
                                     return_binary = T)


# As before, we'll plot this on top of temperature and occurrence records to see how well we did


# Convert the raster to a polygon for visualization

  # convert to polygon for easier visualization
  
    tv_gaussian_kde_polygon <-
      tv_gaussian_kde_raster |>
      as.polygons()|>
      st_as_sf()

# Now, we'll plot the standardized temperature raster, along with the occurrence records and the range map 

  ggplot(env)+
    geom_raster(mapping = aes(x=x,y=y,fill=wc2.1_10m_bio_1))+
    scale_fill_viridis_c(name="Temp. C", na.value = "transparent")+
    scale_x_continuous(expand=c(0,0),
                       limits = c(tv_bbox[1],tv_bbox[3]))+
      scale_y_continuous(limits = c(tv_bbox[2],tv_bbox[4]),
                       expand=c(0,0))+
    theme_bw()+
    geom_sf(data = tv_gaussian_kde_polygon,
            fill = "grey",
            size=2,
            alpha=0.5)+
      geom_point(data = tv,
               mapping = aes(x=longitude,y=latitude))



## ----maxnet-------------------------------------------------------------------


  tv_maxnet <- fit_density_ratio(presence = tv_presence$env,
                                 background = tv_bg$env,
                                 method = "maxnet")
  
  
  tv_maxnet_predictions <- project_density_ratio(dr_model = tv_maxnet,
                                                 data = tv_bg$env)


#Now, we again convert everything to a raster and then to a polygon
  
  tv_maxnet_raster <- env[[1]]

  values(tv_maxnet_raster) <- NA

  tv_maxnet_raster[tv_bg$bg_cells] <-  tv_maxnet_predictions


#Now, we can plot our raster
  
    plot(tv_maxnet_raster,
       xlim = c(tv_bbox[1],tv_bbox[3]),
       ylim = c(tv_bbox[2],tv_bbox[4]))
  points(tv[c("longitude","latitude")])





