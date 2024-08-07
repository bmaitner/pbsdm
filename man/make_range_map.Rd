% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_range_map.R
\name{make_range_map}
\alias{make_range_map}
\title{Make a range map using plug-and-play modeling.}
\usage{
make_range_map(
  occurrences,
  env,
  method = NULL,
  presence_method = NULL,
  background_method = NULL,
  bootstrap = "none",
  bootstrap_reps = 100,
  quantile = 0.05,
  background_buffer_width = NULL,
  constraint_regions = NULL,
  verbose = FALSE,
  standardize_preds = TRUE,
  ...
)
}
\arguments{
\item{occurrences}{Presence coordinates in long,lat format.}

\item{env}{Environmental rasters}

\item{method}{Optional. If supplied, both presence and background density estimation will use this method.}

\item{presence_method}{Optional. Method for estimation of presence density.}

\item{background_method}{Optional. Method for estimation of background density.}

\item{bootstrap}{Character.  One of "none" (the default, no bootstrapping),
"numbag" (presence function is bootstrapped),
or "doublebag" (presence and background functions are bootstrapped).}

\item{bootstrap_reps}{Integer.  Number of bootstrap replicates to use (default is 100)}

\item{quantile}{Quantile to use for thresholding.  Default is 0.05 (5 pct training presence). Set to 0 for minimum trainin presence (MTP), set to NULL to return continuous raster.}

\item{background_buffer_width}{The width (in m for unprojected rasters and map units for projected rasters) of the buffer to use for background data.
Defaults to NULL, which will take the maximum distance between occurrence records.}

\item{constraint_regions}{See get_env_bg documentation}

\item{verbose}{Logical. If TRUE, prints progress messages.}

\item{standardize_preds}{Logical. Should environmental layers be scaled? Default is TRUE.}

\item{...}{Additional parameters passed to internal functions.}
}
\description{
This function produces range maps using plug-and-play modeling with either presence-background or density-ratio approaches.
}
\details{
Current plug-and-play methods include: "gaussian", "kde","vine","rangebagging", "lobagoc", and "none".
Current density ratio methods include: "ulsif", "rulsif",and "maxnet".
}
\note{
Either \code{method} or both \code{presence_method} and \code{background_method} must be supplied.
}
\examples{
{

# load packages
   library(geodata)

   # make temp directory

   temp <- tempdir()

   # Get some occurrence data
   occurrences <- BIEN::BIEN_occurrence_species(species = "Xanthium strumarium",
                                                new.world = TRUE,
                                                cultivated = FALSE)

   # Thin down to unique occurrences
   occurrences <- unique(occurrences[c("longitude","latitude")])

   # Get bioclim data

   env <- worldclim_global(var = "bio",
                           res = 10,
                           path = temp)


   env <- env[[c(1,12)]]

   map <- make_range_map(occurrences = occurrences,
                         env = env,
                         method = "gaussian",
                         presence_method = NULL,
                         background_method = NULL,
                         bootstrap = "none",
                         bootstrap_reps = 100,
                         quantile = 0.05,
                         background_buffer_width = 100000)

   plot(map)


}
}
