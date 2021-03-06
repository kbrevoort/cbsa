% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapping.R
\name{adjust_states}
\alias{adjust_states}
\title{Adjust States}
\usage{
adjust_states(shapes, state, geo, year, resolution)
}
\arguments{
\item{shapes}{A `SpatialPolygonsDataFrame`}

\item{state}{Character scalar equal to either 'alaska' or 'hawaii'}

\item{geo}{Character vector giving the geography of the shapefile (e.g., 'state',
'cbsa')}

\item{resolution}{Character scalar given the resolution of the shape file}
}
\description{
Extracts the polygons in the supplied shape file that correspond to the state
specified, and adjusts them to appear to the lower-left of the continental U.S.
}
