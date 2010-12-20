\name{add.layout}
\alias{add.layout}
\alias{add.labels}
\title{
Functions designed to create non-overlaping layout of rectangles or labels.
}
\description{
\code{add.layout} creates or adds to a layout of non-overlapping
rectangles.

\code{add.labels} is a front-end to \code{add.layout} which creates a
layout suitable for labels allowing to adjust the placement of the label
with respect to the original points and to add extra margins.
}
\usage{
add.layout(x, y, w, h, rs = 0.01, as = 0.22, l = NULL)
add.labels(x, y, txt, w, h, adj = 0.5, mar = 0.1, ...)
}
\arguments{
  \item{x}{x coordinates of the points to add. For rectangles it is the
    left edge.}
  \item{y}{y coordinates of the points to add. For rectangled it is the
    bottom edge.}
  \item{w}{width of the rectangles (optional for labels where it
    defaults to the label widths)}
  \item{h}{height of the rectangles (optional for labels where it
    defaults to the label height)}
  \item{rs}{radius step for the placement algorithm}
  \item{as}{angle step for the placement algorithm}
  \item{l}{layout to add to or \code{NULL} if a new layout is to be
    created}
  \item{adj}{adjustment of the text position with respect to the
    supplied points (see \code{adj} in \code{link{text}})}
  \item{mar}{additional margins around the text (relative to the width
    or height). If present, it will be recycled to length two for
    horizontal and vertical margins respectively.}
  \item{...}{additional parameters passed through to \code{add.layout}}
}
\details{
  The layout attempts to sequentially place rectangles of the given
  width and height at the specified points. If a placement of a new
  rectangle would overlap with any existing rectangle, it is moved until
  it no longer overlaps. The current algorithm defines the movement as a
  clockwise spiral.  
}
\value{
  \code{add.layout} returns an obejct of the class \code{box.layout}
  which consists of a list with elements \code{x}, \code{y}, \code{w}
  and \code{h}. If the input was non-overlapping it would be equivalent
  to the (recycled) arguments. If there are overlaps the \code{x} and
  \code{y} coordinates will differ accordingly.

  If \code{l} is specified on input, it is expected to be
  \code{box.layput} as well and the layout is extended by adding the
  rectangles defined in the arguments.

  \code{add.labels} returns an object of the class \code{label.layout}
  which is a subclass of \code{box.layout}. It adds the components
  \code{lx} and \code{ly} which are the label coordinates (as opposed to
  the rectangle coordinates). If \code{adj} is zero the label and box
  coordinates are equal.
}
%\references{
%}
%\author{
%}
%\note{
%}
%\seealso{
%}
\examples{
x = rnorm(100)
y = rnorm(100)
txt = sprintf("%.2f", rnorm(100))
plot(x, y, pch=3, col=2)
l = add.labels(x, y, txt, adj=0.5, mar=0.2, rs=0.01)
rect(l$x, l$y, l$x + l$w, l$y + l$h, col="#00000020", border=NA)
text(l$lx, l$ly, txt, col=4)
segments(x, y, l$lx, l$ly, col=3)
}
\keyword{dplot}
