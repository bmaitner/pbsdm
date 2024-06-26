% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_response_curves.R
\name{get_response_curves}
\alias{get_response_curves}
\title{Generate Response Curves}
\usage{
get_response_curves(
  env_bg,
  env_pres,
  pnp_model,
  n.int = 1000,
  envMeans = NULL,
  envSDs = NULL
)
}
\arguments{
\item{env_bg}{Object returned by get_env_bg}

\item{env_pres}{Object returned by get_env_pres}

\item{pnp_model}{Object returned  by \code{fit_plug_and_play} or \code{fit_density_ratio}}

\item{n.int}{Number of points along which to calculate the response curve}

\item{envMeans}{A vector of means for each environmental predictor in the dataset. (not used)}

\item{envSDs}{A vector of standard deviations for each environmental predictor in the dataset.(not used)}
}
\value{
This function generates a set of marginal predictions for each environmental variable, holding other variables constant
}
\description{
Given an environmental data set, fitted models, and a directory to output plots, this function generates response curves for each predictor in the model. The response curves depict the predicted change in probability of presence as a function of the environmental predictor while holding all other predictors constant at their mean values.
}
\author{
Cory Merow, modified by Brian Maitner
}
