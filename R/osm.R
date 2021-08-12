# convert lon/lat (in deg) to OSM tile numbers (x,y)
osm.ll2xy <- function(lon, lat, zoom=16) {
  n = 2 ^ zoom
  lat.rad = lat / 180 * pi
  list(x = (((lon + 180) / 360) * n) %% n, y = ((1 - (log(tan(pi/4 + lat.rad/2)) / pi)) / 2 * n) %% n)
}

# convert OSM tile numbers to lon/lat (in deg)
osm.xy2ll <- function(x, y, zoom=16) {
  n = 2 ^ zoom
  lon.deg = x / n * 360.0 - 180.0
  lat.rad = atan(sinh(pi * (1 - 2 * y / n)))
  lat.deg = lat.rad * 180.0 / pi
  list(lon = lon.deg, lat = lat.deg)
}

# fill the area with OSM map (by default the area is the whole current device)
# tiles are expected to be <zoom>/<x>-<y>.png files (and already present)
# or the tile.url has to icnlude %x %y and %z which will be replaced by the coordinates
# (requires "png" R package for readPNG and R capable of rasterImage())
osmap <- function(alpha=1, zoom, area = par()$usr, tiles.url, cache.dir, tile.coord=FALSE) {
  if (missing(tiles.url)) tiles.url <- getOption("osm.tiles.url")
  if (missing(cache.dir)) cache.dir <- getOption("osm.cache.dir")
  if (is.null(tiles.url)) tiles.url <- "http://a.tile.openstreetmap.org/"
  if (length(area) != 4L || !is.numeric(area)) stop("invalid area specification")
  tc <- FALSE
  if (!identical(tile.coord, FALSE)) {
    tc <- TRUE
    tc.zoom <- as.integer(tile.coord)
    if (missing(zoom))
      zoom <- log(5 * 2^tc.zoom / abs(area[2] - area[1])) / log(2)
  }
  if (missing(zoom)) # get some reasonable zoom estimation based on the covered area
    zoom <- log(5 * 360 / abs(area[2] - area[1])) / log(2)
  if (zoom > 19) zoom <- 19L
  if (zoom < 0) zoom <- 0L
  zoom <- as.integer(zoom)
  if (tc)
    tcf <- 2^tc.zoom / 2^zoom
  if (!is.null(cache.dir)) cache.dir <- path.expand(cache.dir)
  # tempfile is unreliable when used in multicore so force a random name
  my.tmp <- tempfile(sprintf("R.tile.%f.",runif(1)))
  fixed.url <- !isTRUE(grepl("%x", tiles.url, fixed=TRUE))
  get.tile <- function(x, y, zoom) {
    x <- x %% (2^zoom)
    y <- y %% (2^zoom)
    ## cat("get (",x,",",y,"zoom",zoom,")\n")
    cached <- FALSE
    if (!is.null(cache.dir)) {
      if (cache.dir == "") cache.dir <- "."
      cache.fn <- paste(cache.dir, "/", zoom, "/", x, "-", y, ".png", sep='')
      if (file.exists(cache.fn)) {
        img <- try(readPNG(cache.fn, native=TRUE), silent=TRUE)
        if (!inherits(img,"try-error")) return(img)
        warning("tile", cache.fn," is corrupt in the cache, re-fetching")
      }
      if (!file.exists(paste(cache.dir, "/", zoom, sep='')))
        dir.create(paste(cache.dir, "/", zoom, sep=''), recursive=TRUE)
      tmp <- cache.fn
      cached <- TRUE
    } else tmp <- my.tmp
    url <- if (fixed.url) paste0(tiles.url, zoom, "/", x, "/", y, ".png") else gsub("%x", x, gsub("%y", y, gsub("%z", zoom, tiles.url, fixed=TRUE), fixed=TRUE), fixed=TRUE)
    if (download.file(url , tmp, quiet=TRUE, mode="wb") != 0L || isTRUE(is.na(sz <- file.info(tmp)$size))) {
      warning("unable to download tile ", url)
      return (NULL)
    }
    f <- file(tmp, "rb")
    raw <- readBin(f, raw(), sz)
    close(f)
    img <- if (length(raw) < 32 || raw[1L] != 0x89 || raw[2L] != 0x50 || raw[3L] != 0x4E) {
      ## not a PNG file, try JPEG since some tile servers use JPEG tagged as PNG for satellite tiles
      if (length(grepRaw("JFIF", raw[1:32], fixed=TRUE)))
        readJPEG(raw, native=TRUE) else stop("Invalid file format, neither PNG nor JPEG file (see ", tmp, ")")
    } else readPNG(raw, native=TRUE)
    if (!cached) unlink(tmp)
    img
  }
  if (tc) {
    lo <- c(area[1], -area[4]) / tcf
    hi <- c(area[2], -area[3]) / tcf
  } else {
    lo <- unlist(osm.ll2xy(area[1], area[4], zoom=zoom))
    hi <- unlist(osm.ll2xy(area[2], area[3], zoom=zoom))
  }
  if (lo[1] > hi[1])
      hi[1] <- hi[1] + 2^zoom
  if (lo[2] > hi[2])
      hi[2] <- hi[2] + 2^zoom
  lo <- as.integer(floor(lo))
  hi <- as.integer(ceiling(hi))
  ## print(lo); print(hi)
  failed <- 0L
  for (x in seq.int(lo[1],hi[1]))
    for (y in seq.int(lo[2],hi[2])) {
      q <- get.tile(x, y, zoom)
      if (!is.null(q)) {
        if (tile.coord)
          rasterImage(q, x * tcf, (-y - 1.004) * tcf, (x + 1.004) * tcf, -y * tcf)
        else {
          tl <- osm.xy2ll(x,y,zoom=zoom)
          br <- osm.xy2ll(x+1.004,y+1.004,zoom=zoom)
          rasterImage(q, tl$lon, br$lat, br$lon, tl$lat)
        }
      } else failed <- failed + 1L
    }

  if (alpha < 1)
    rect(-180, -90, 180, 90, col = rgb(1, 1 , 1 , 1 - alpha))

  if (failed) stop("total of ", failed, " tiles could not be retrieved")
}
