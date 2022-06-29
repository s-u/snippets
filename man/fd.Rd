\name{fd}
\alias{fd}
\alias{fd.matrix}
\alias{fd.table}
\title{
Fluctuation diagram
}
\description{
Draws a fluctuation diagram.
}
\usage{
fd(x, ...)
\method{fd}{matrix}(x, add = FALSE, vals = FALSE, at.x, at.y, axes = TRUE,
frame.plot = FALSE, main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
zmax = max(x, na.rm = TRUE), xlim, ylim, asp = 1, panel.first = NULL,
panel.last = NULL, ann = par("ann"), col = "grey", border = "black",
\dots)
\method{fd}{table}(x, add = FALSE, vals = FALSE, at.x, at.y, axes = TRUE,
frame.plot = FALSE, main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
zmax = max(x, na.rm = TRUE), xlim, ylim, asp = 1, panel.first = NULL,
panel.last = NULL, ann = par("ann"), col = "grey", border = "black",
\dots)
}
\arguments{
  \item{x}{object to draw fluctuation diagram of (most commonly a table)}
  \item{add}{a logical value indicating whether to add to an existing
    plot (\code{TRUE}) or to create a new plot (\code{FALSE}).}
  \item{vals}{a logical value indicating whether to draw values into the
    rectangles (discouraged and unimplemented).}
  \item{at.x}{locations of the colums (by default \code{1:ncol})}
  \item{at.y}{locations of the rows (by default \code{1:nrow})}
  \item{axes}{a logical value indicating whether both axes should be
    drawn on the plot.  Use graphical parameter \code{xaxt} or
    \code{yaxt} to suppress just one of the axes.}
  \item{frame.plot}{a logical indicating whether a box should be drawn
    around the plot.}
  \item{main}{a main title for the plot, see also \code{\link{title}}.}
  \item{sub}{a subtitle for the plot.}
  \item{xlab}{a label for the \code{x} axis.}
  \item{ylab}{a label for the \code{y} axis.}
  \item{zmax}{value representing the total size of an allocated box.}
  \item{xlim}{the x limits (x1, x2) of the plot. The default is the
    range of \code{at.x} with an additional 0.5 margin of the ends.}
  \item{ylim}{the y limits of the plot.}
  \item{asp}{the y/x aspect ratio, see \code{\link{plot.window}}.}
  \item{panel.first}{
    an expression to be evaluated after the plot axes are set
    up but before any plotting takes place.  This can be useful
    for drawing background grids}
  \item{panel.last}{
    an expression to be evaluated after plotting has taken
    place.}
  \item{ann}{see \code{"ann"} graphical parameter}
  \item{col}{color of the boxes to be filled with, will be recycled to
    match the shape of \code{x}.}
  \item{border}{color of the box borders - only scalar value is
    supported at the moment.}
  \item{\dots}{additional graphical parameters}
}
%\details{
%}
\value{
  Returns (invisibly) a data frame describing the sparse representation
  of the boxes as location and radius.
}
%\references{
%}
%\author{
%}
%\note{
%}
\examples{
## this is best viewed on a wide-screen device...
par(mfrow=c(1,2))
for (sex in dimnames(HairEyeColor)$Sex)
  fd(HairEyeColor[,,sex], main=sex)
}
\keyword{hplot}
