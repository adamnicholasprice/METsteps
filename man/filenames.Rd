% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filenames.R
\name{filenames}
\alias{filenames}
\title{Filename Extraction and Creation}
\usage{
filenames(fnames = NULL, dataName = NULL, HUC = NULL, timeStep = NULL,
  startDate = NULL, infoReturn = c("dataName", "HUC", "timeStep",
  "startDate"), returnList = FALSE)
}
\arguments{
\item{fnames}{Character; List of filenames of feather files from which to extract data name, HUC number, timestep, and starting date.}

\item{dataName}{Character; Name of dataset.}

\item{HUC}{Character or numeric; Number of HUC unit of dataset.}

\item{timeStep}{Character; Time step between measurements.  Can be 'day', 'week', 'month', 'year', etc.}

\item{startDate}{Character; Starting date for measurements in 'YYYY-M-D' format.  Set 'D' to 1 for time steps > weekly.}

\item{infoReturn}{Character; Data to return when extracting from filenames.  List of objects 'dataName', 'HUC', 'timeStep', and 'startDate'.
Defaults to returning all data.}

\item{returnList}{Logical; Option whether to return data in list format or not.  Defaults to FALSE, not returning a list.}
}
\value{
Number of cells in object.
}
\description{
Function to return total number of cells in objects such as vectors, matrices, dataframes, or lists.  dataName, HUC, timestep, and 
startDate can be lists as long as matching lengths.  If any filenames supplied, other inputs ignored.
}
