#' Calculates the separation distance between two geopoints considering the earth curvature.
#'
#' @param lat_from Origin lat
#' @param lon_from Origin lng
#' @param lat_to final lat
#' @param lon_to final lng
#' @param r Default for haversine distance
#'
#' @return Separation in km
#' @export
#'
#' @examples
#' dt.haversine(19.2313,-95.12123,18.312312,-96.23123)

dt.haversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137){
  radians <- pi/180
  lat_to <- lat_to * radians
  lat_from <- lat_from * radians
  lon_to <- lon_to * radians
  lon_from <- lon_from * radians
  dLat <- (lat_to - lat_from)
  dLon <- (lon_to - lon_from)
  a <- (sin(dLat/2)^2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon/2)^2)
  return(2 * atan2(sqrt(a), sqrt(1 - a)) * r)
}
