% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_env_bg.R
\name{rescale_w_objects}
\alias{rescale_w_objects}
\title{Rescale a dataset using vectors of means and SDs}
\usage{
rescale_w_objects(data, mean_vector, sd_vector)
}
\arguments{
\item{data}{dataframe or matrix for rescaling}

\item{mean_vector}{vector of means to use for rescaling.  Should be one value for each column in the data}

\item{sd_vector}{vector of sds to use for rescaling.  Should be one value for each column in the data}
}
\description{
A little function to rescale data using vectors of means and sds
}
\author{
Brian Maitner
}
\keyword{internal}
