% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_classification.R
\name{classif_access}
\alias{classif_access}
\title{Classify Accessibility of Greenspaces}
\usage{
classif_access(x)
}
\arguments{
\item{x}{A basemap sf object with the GI attribute}
}
\value{
The basemap sf object with new attribute GIpublic
}
\description{
Adds a GIpublic attribute indicating whether a greenspace is public, private or has restricted access (only accessible at certain times or to certain groups of people e.g. through membership). Not meant to be called directly; is called in classify_habitats().
}
