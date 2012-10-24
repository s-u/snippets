# convert lon/lat (in deg) to OSM tile numbers (x,y)
osm.ll2xy <- function(lon, lat, zoom=16) {
  n = 2 ^ zoom
  lat.rad = lat / 180 * pi
  list(x = ((lon + 180) / 360) * n, y = (1 - (log(tan(lat.rad) + 1/cos(lat.rad)) / pi)) / 2 * n)
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
# (requires "png" R package for readPNG and R capable of rasterImage())
osmap <- function(alpha=1, zoom, area = par()$usr, tiles.url, cache.dir) {
  if (missing(tiles.url)) tiles.url <- getOption("osm.tiles.url")
  if (missing(cache.dir)) cache.dir <- getOption("osm.cache.dir")
  if (is.null(tiles.url)) tiles.url <- "http://a.tile.openstreetmap.org/"
  if (length(area) != 4L || !is.numeric(area)) stop("invalid area specification")
  if (missing(zoom)) # get some reasonable zoom estimation based on the covered area
    zoom <- log(5 * 360 / abs(area[2] - area[1])) / log(2)
  if (zoom > 19) zoom <- 19L
  if (zoom < 0) zoom <- 0L
  zoom <- as.integer(zoom)
  if (!is.null(cache.dir)) cache.dir <- path.expand(cache.dir)
  # tempfile is unreliable when used in multicore so force a random name
  my.tmp <- tempfile(sprintf("R.tile.%f.",runif(1)))
  get.tile <- function(x, y, zoom) {
    cached <- FALSE
    if (!is.null(cache.dir)) {
      if (cache.dir == "") cache.dir <- "."
      cache.fn <- paste(cache.dir, "/", zoom, "/", x, "-", y, ".png", sep='')
      if (file.exists(cache.fn)) return(readPNG(cache.fn, native=TRUE))
      if (!file.exists(paste(cache.dir, "/", zoom, sep='')))
        dir.create(paste(cache.dir, "/", zoom, sep=''), recursive=TRUE)
      tmp <- cache.fn
      cached <- TRUE
    } else tmp <- my.tmp
    url <- paste(tiles.url, zoom, "/", x, "/", y, ".png", sep='')
    if (download.file(url , tmp, quiet=TRUE) != 0L)
      stop("unable to download tile ", url)
    img <- readPNG(tmp, native=TRUE)
    if (!cached) unlink(tmp)
    img
  }
  lo <- as.integer(unlist(osm.ll2xy(area[1], area[4], zoom=zoom)))
  hi <- as.integer(unlist(osm.ll2xy(area[2], area[3], zoom=zoom))) + 1L
  for (x in lo[1]:hi[1])
    for (y in lo[2]:hi[2]) {
      q <- get.tile(x,y,zoom)
      tl <- osm.xy2ll(x,y,zoom=zoom)
      br <- osm.xy2ll(x+1.004,y+1.004,zoom=zoom)
      rasterImage(q, tl$lon, br$lat, br$lon, tl$lat)
    }

  if (alpha < 1)
    rect(-180, -90, 180, 90, col = rgb(1, 1 , 1 , 1 - alpha))
}
