\name{gpx}
\alias{gpx}
\title{
Create a GPX (GPS Exchange Format) output.
}
\description{
\code{gpx} creates XML output in the GPX format (GPS Exchange Format)
from latitude, longitude and time.
}
\usage{
gpx(lat, lon, time, file = NULL)
}
\arguments{
  \item{lat}{latitude of the points}
  \item{lon}{longitude of the points}
  \item{time}{time of the points (optional)}
  \item{file}{destination - can be a character string naming the output
    file or a connection to use for output or \code{NULL} in which case
    the function return a character vector with teh output.}
}
\details{
  The resulting output is in GPX format contining exactly one track with
  the specified values (\code{NA}s are currently not supported!). If the
  \code{time} value is present then the track entries will include a
  time nodes as well. No checking is done on time entries so the user
  must ensure that they are exactly of the form
  \code{YYYY-MM-DD HH:MM:SS}, assumed to be in UTC.

  (Note that OSM requires time stamps to be present in uploaded tracks.)
}
\value{
If \code{file} is \code{NULL} then the value is a character vector
containing the lines of the GPX output.
}
%\references{
%}
\examples{
lat <- c(40.779, 40.777)
lon <- c(-74.428,-74.418)
cat(gpx(lat, lon), sep='\n')
cat(gpx(lat, lon, Sys.time()), sep='\n')
}
\keyword{interface}
