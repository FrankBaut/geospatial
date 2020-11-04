#' bounding_box
#'
#' @param lat Central latitude
#' @param lon Central longitude
#' @param dist Distance of separation of the central point (in miles or km)
#' @param in.miles Bolean value, by default is miles
#'
#' @return
#' @export
#'
#' @examples
#' bounding_box(lat=19.01212,log=-96.19216,dist =100,in.miles = F)
bounding_box <- function(lat, lon, dist, in.miles = TRUE) {
    ## Helper functions
    if (in.miles) {
      ang_rad <- function(miles) miles/3958.756
    } else {
      ang_rad <- function(miles) miles/1000
    }
    `%+/-%` <- function(x, margin){x + c(-1, +1)*margin}
    deg2rad <- function(x) x/(180/pi)
    rad2deg <- function(x) x*(180/pi)
    lat_range <- function(latr, r) rad2deg(latr %+/-% r)
    lon_range <- function(lonr, dlon) rad2deg(lonr %+/-% dlon)
    r <- ang_rad(dist)
    latr <- deg2rad(lat)
    lonr <- deg2rad(lon)
    dlon <- asin(sin(r)/cos(latr))
    m <- matrix(c(lon_range(lonr = lonr, dlon = dlon),
                  lat_range(latr=latr, r=r)), nrow=2, byrow = TRUE)
    dimnames(m) <- list(c("lng", "lat"), c("min", "max"))
    m
  }
