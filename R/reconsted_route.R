#
#' reconstred_route reconstruccion de rutas con geoposicoones en (lon,lat)
#'
#' @param data
#'
#' @return
#' @importFrom foreach foreach `%do%`
#' @export
#'
#' @examples

`%do%` <- foreach::`%do%`
reconsted_route <- function(data){
  route <- foreach::foreach(j = 1:(dim(data)[1]-1)) %do% {
    from <- c(data$lng[j], data$lat[j])
    to <- c(data$lng[j + 1], data$lat[j + 1])
    tryCatch(osrm::osrmRoute(src = from, dst = to,
                             returnclass = "sf",
                             overview = "full"),
             error = function(e) NULL)
  }
  route[sapply(route, is.null)] <- NULL
  route <- do.call(rbind, route)
  return(route)
}
