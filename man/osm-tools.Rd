\name{osm-tools}
\alias{osm.xy2ll}
\alias{osm.ll2xy}
\title{
Tools converting from lat/lon coordinates to OSM tiles and back.
}
\description{
  \code{osm.ll2xy} converts lat/lon coordinates to OpenStreetMap tile
  numbers. \code{osm.xy2ll} performs the inverse conversion.
}
\usage{
osm.xy2ll(x, y, zoom = 16)
osm.ll2xy(lon, lat, zoom = 16)
}
\arguments{
  \item{x}{number of the tile in the x direction}
  \item{y}{number of the tile in the y direction}
  \item{lon}{longitude}
  \item{lat}{latitude}
  \item{zoom}{zoom factor of the tiles}
}
\details{
  \code{osm.ll2xy} computes tile numbers for given latitude and
  longitude, \code{osm.xy2ll} does the inverse.
}
\value{
  \code{osm.ll2xy} returns a list with the components \code{x} and \code{y}

  \code{osm.xy2ll} returns a list with the components \code{lon} and \code{lat}
}
\seealso{
\code{\link{osmap}}
}
%\examples{
%}
\keyword{manip}
