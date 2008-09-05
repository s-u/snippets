# base for all diverging mixing
col.base <- function(x, alpha=1, lim=c(0, 1), center = mean(lim), fit=FALSE, trans="id", na.col=NA, FN) {
  if (all(is.na(x))) return(rep(na.col, length(x)))
  nas <- is.na(x)
  if (any(nas)) x[nas] <- x[which(!nas)[1]] # replace all NAs with the first non-NA value (we'll fix it later)
  if (fit) x <- (x - min(x))/(max(x) - min(x)) * (lim[2] - lim[1]) + lim[1]
  x[x > lim[2]] <- lim[2]
  x[x < lim[1]] <- lim[1]
  r <- (center - x) / (center - lim[1])
  r[r < 0] <- 0
  if (is.function(trans)) r <- trans(r) else
  if (trans == "sin") r <- sin(r * pi / 2) else
  if (trans == "asin") r <- asin(r) / pi * 2
  b <- (x - center) / (lim[2] - center)
  b[b < 0] <- 0
  if (is.function(trans)) b <- trans(b) else
  if (trans == "sin") b <- sin(b * pi / 2) else
  if (trans == "asin") b <- asin(b) / pi * 2
  res <- FN(r, b, alpha)
  if (any(nas)) res[nas] <- na.col # replace all NAs with na.col
  res
}

col.bbr <- function(x, alpha=1, lim=c(0, 1), center = mean(lim), fit=FALSE, trans="id", na.col=NA)
  col.base(x, alpha, lim, center, fit, trans, na.col, function(r,b,a) rgb(b, 0, r, a))

col.bwr <- function(x, alpha=1, lim=c(0, 1), center = mean(lim), fit=FALSE, trans="id", na.col=NA)
  col.base(x, alpha, lim, center, fit, trans, na.col, function(r,b,a) rgb(1 - r, 1 - (r + b), 1 - b, a))

col.br <- function(x, alpha=1, lim=c(0, 1), center = mean(lim), fit=FALSE, trans="id", na.col=NA)
  col.base(x, alpha, lim, center, fit, trans, na.col, function(r,b,a) rgb((b + (1 - r)) / 2, 0, ((1 - b) + r) / 2, a))

col.bw <- function(x, alpha=1, lim=c(0, 1), center = mean(lim), fit=FALSE, trans="id", na.col=NA)
  col.base(x, alpha, lim, center, fit, trans, na.col, function(r,b,a) rgb((b + (1 - r)) / 2, (b + (1 - r)) / 2, (b + (1 - r)) / 2, a))

col.q <- function(x, alpha=1, lim=c(0, 1), center = mean(lim), fit=FALSE, trans="id", na.col=NA)
  col.base(x, alpha, lim, center, fit, trans, na.col, function(r,b,a) a * ((b + (1 - r)) / 2))

# not good - need to look at Marty's scheme ...
#col.yg <- function(x, alpha=1, lim=c(0, 1), center = mean(lim), fit=FALSE, trans="id", na.col=NA)
#  col.base(x, alpha, lim, center, fit, trans, na.col, function(r,b,a) rgb(((1 - b) + r) / 2 , 1, 0, a))
