\name{osmap}
\alias{osmap}
\title{
Draw tiles from OpenStreetMap.
}
\description{
\code{osmap} fetches and draws OpenStreetMap tiles (or any compatible
tiles) in the current graphics device assuming latitude and longitude
coordinates.
}
\usage{
osmap(alpha = 1, zoom, area = par()$usr, tiles.url, cache.dir)
}
\arguments{
  \item{alpha}{in theory this is the desired opacity of the map but in
    practice the final plot is faded to white by this alpha value (1 =
    no fading, 0 = entriely white)}
  \item{zoom}{zoom level of the map to draw (optional). If missing the
    zomo level is determined automatically to cover the area with about
    five tiles in the horizontal direction}
  \item{area}{minimal are to cover - the required tiles are computed to
    cover at least this area}
  \item{tiles.url}{URL to the tiles server (excluding the
    \code{zoom/x/y.png} part). If missing, \code{osm.tiles.url} option
    is consulted and if also missing then an OSM tile server is used.}
  \item{cache.dir}{if set, the tiles are first looked up in this
    directory and the directory is used for caching downloaded tiles}
}
%\details{
%}
%\value{
%}
\examples{
  par(mar=rep(0,4))
  # plot any lat/lon data - here just an area around the AT&T Labs
  plot(c(-74.44, -74.39), c(40.76, 40.79), type='n')
  # draw the map (needs internet connection to get the tiles)
  osmap()
}
\keyword{hplot}
