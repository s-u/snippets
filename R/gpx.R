gpx <- function(lat, lon, time, file=NULL) {
  o <- c('<gpx version="1.1" creator="R">','<trk>','<trkseg>')
  if (missing(time))
    o <- c(o, paste('<trkpt lat="',lat,'" lon="',lon,'" />', sep=''))
  else
    o <- c(o, paste('<trkpt lat="',lat,'" lon="',lon,'"><time>',paste(gsub(' ','T', as.character(time)), 'Z', sep=''),'</time></trkpt>', sep=''))
  o <- c(o, '</trkseg>', '</trk>', '</gpx>')
  if (is.character(file) || inherits(file, "connection"))
    writeLines(o, file)
  else
    o
}
