data2screen <- function(x, y, width = par("fin")[1] * dpi, height = par("fin")[2] * dpi, dpi = 72, plt = par("plt"), usr = par("usr"), flip=FALSE) {
  if (missing(y) && length(dim(x)) > 1) {
    ml = dim(x)[1]
    y = x[,2]
    x = x[,1]
  } else {
    if (missing(y)) stop("y is missing and x is not a matrix")
    ml = max(length(x), length(y))
    if (length(x) != ml) x = rep(x, length.out=ml)
    if (length(y) != ml) y = rep(y, length.out=ml)
  }
  x0 = width * plt[1]
  y0 = height * plt[3]
  dx = x0 + (x - usr[1]) * (width * (plt[2] - plt[1])) / (usr[2] - usr[1])
  dy = y0 + (y - usr[3]) * (height * (plt[4] - plt[3])) / (usr[4] - usr[3])
  if (flip) dy = height - dy
  if (ml > 1) matrix(c(dx, dy),,2,dimnames=list(NULL,c("x","y"))) else c(x=dx, y=dy)
}

screen2data <- function(x, y, width = par("fin")[1] * dpi, height = par("fin")[2] * dpi, dpi = 72, plt = par("plt"), usr = par("usr"), flip=FALSE) {
  if (missing(y) && length(dim(x)) > 1) {
    ml = dim(x)[1]
    y = x[,2]
    x = x[,1]
  } else {
    if (missing(y)) stop("y is missing and x is not a matrix")
    ml = max(length(x), length(y))
    if (length(x) != ml) x = rep(x, length.out=ml)
    if (length(y) != ml) y = rep(y, length.out=ml)
  }
  x0 = width * plt[1]
  y0 = height * plt[3]
  if (flip) y = height - y
  dx = usr[1] + (x - x0) / (width * (plt[2] - plt[1])) * (usr[2] - usr[1])
  dy = usr[3] + (y - y0) / (height * (plt[4] - plt[3])) * (usr[4] - usr[3])
  if (ml > 1) matrix(c(dx, dy),,2,dimnames=list(NULL,c("x","y"))) else c(x=dx, y=dy)
}
