% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zooHUCtoFeather.R
\name{zooHUCtoFeather}
\alias{zooHUCtoFeather}
\title{Convert to feather}
\usage{
zooHUCtoFeather(zoo.obj,
  featherPath = "C:/Users/ssaxe/Documents/Projects/Model Evaluation/RShiny/FeatherFiles")
}
\arguments{
\item{zoo.obj}{Either output from downscale_HUC10() function (list object of zoo-format dataframes) or
vector of paths to locations of csv files.}

\item{featherPath}{Path to containing folder for feather files to be used by Shiny application.}
}
\value{
List of tables of HUC 2-8 means in 'zoo' format.  Time-step x HUC-10.
}
\description{
This function converts the monthly mean HUC values to 'feather' format to enable faster importing than .csv 
or .txt formats.  Necessary for rapid RShiny app processing.  Requires 'feather' package.
}
\examples{
zooHUCtoFeather()
}
