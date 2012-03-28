split.at <- function(x, cutpoints, exclusive = FALSE) .Call("split_at", x, cutpoints, exclusive, PACKAGE="snippets")
