rfBin <- function(filename, what=1L, ...) {
  if (!is.character(filename)) stop("filename must be a character vector")
  if (length(filename) > 1L) {
    l <- lapply(filename, rfBin, what, ...)
    return (do.call("c", l))
  }
  con <- file(filename, "rb")
  on.exit(close(con))
  sm <- match(storage.mode(what), c("integer", "double"))
  if (any(is.na(sm))) stop("invalid `what' - must be a double or interger")
  sz <- c(4L, 8L)[sm]
  n <- file.info(filename)$size / sz
  readBin(con, what, n, ...)
}
