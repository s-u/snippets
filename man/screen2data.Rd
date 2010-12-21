\name{screen2data}
\alias{screen2data}
\alias{data2screen}
\title{
Functions converting between pixel and data coordinates.
}
\description{
\code{screen2data} converts between screen (pixel) coordinates of device
output and the data coordinates.

\code{data2screen} performs the inverse conversion.

Both functions can be parametrized manually but default to obtaining all
necessary parameters from the active graphics device. It is most useful
when used with bitmap dvices such as the Cairo device (see \code{Cairo}
package) for the purpose of adding interactivity (e.g., via JavaScript).
}
\usage{
screen2data(x, y, width = par("fin")[1] * dpi, height = par("fin")[2] * dpi,
            dpi = 72, plt = par("plt"), usr = par("usr"), flip = FALSE)
data2screen(x, y, width = par("fin")[1] * dpi, height = par("fin")[2] * dpi,
            dpi = 72, plt = par("plt"), usr = par("usr"), flip = FALSE)
}
\arguments{
  \item{x}{x coordinates of locations to convert or a two-column matrix
    (if \code{y} is missing)}
  \item{y}{y coordinates of locations to convert (if missing \code{x}
    must be a matrix and the second column of \code{x} is interpreted as
    \code{y})}
  \item{width}{width of the figure region (usually the size of the
    resulting file in pixels)}
  \item{height}{height of the figure region (usually the size of the
    resulting file in pixels)}
  \item{dpi}{resolution (only used to compute the width and height from
    figure size if they are not specified}
  \item{plt}{the `plt' parameter}
  \item{usr}{the `usr' parameter}
  \item{flip}{if set to \code{TRUE} then \code{y} axis in pixels is
    assumed to be flipped (0 on top)}
}
%\details{
%}
\value{
  The result is a two-column matrix with the columns \code{x} and
  \code{y}. The special case of one row input is returned as a named
  vector of length two.
}
%\references{
%}
%\author{
%}
\note{
  If \code{x} and \code{y} are vectors they are recycled to match.
}
%\seealso{
%}
\examples{
plot(0:1,0:1)
## where on the bitmap is the data point 0,0 ?
data2screen(0, 0)
## if I click on 200, 100 with flipped coordinates, what coordinates do
## I hit?
screen2data(200, 100, flip=TRUE)
## go there and back
screen2data(data2screen(c(0, 0.5), c(1, 0.5)))
}
\keyword{dplot}
