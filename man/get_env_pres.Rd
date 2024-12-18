% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_env_pres.R
\name{get_env_pres}
\alias{get_env_pres}
\title{Extract presence data for SDM fitting.}
\usage{
get_env_pres(coords, env, env_bg = NULL)
}
\arguments{
\item{coords}{Coordinates (long,lat) to extract values for}

\item{env}{Environmental rasterstack in any projection}

\item{env_bg}{Background data produced by \code{get_env_bg}, used for re-scaling}
}
\description{
This function extracts presence data at known presence records.
}
\examples{
 {

# load in sample data

 library(S4DM)
 library(terra)

 # occurrence points
   data("sample_points")
   occurrences <- sample_points

 # environmental data
   env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

 # rescale the environmental data

   env <- scale(env)

env_pres <- get_env_pres(coords = occurrences,
                        env = env)

}
}
