#' Title georadius
#'
#' @param x
#' @param y
#' @param radius
#' @param nPoints
#'
#' @return cricleDF
#' @export
#'
#' @examples geospatial::georadius(x=-96.9236,y=-19.527196,radius = 1,nPoints = 100)
#'
georadius<-function(x,y,radius,nPoints){
  centers<-cbind(x,y) %>% as.data.frame()
  colnames(centers)<- c("lng","lat")
  meanLat <- mean(centers$lat)#Radio en kilometros
  radiusLon <- radius /111 / cos(meanLat/57.3)
  radiusLat <- radius / 111
  angle <- seq(0,2*pi,length.out = nPoints)
  lng <- unlist(lapply(centers$lng, function(x) x + radiusLon * cos(angle)))
  lat <- unlist(lapply(centers$lat, function(x) x + radiusLat * sin(angle)))
  circleDF<-cbind(lng,lat) %>% as.data.frame()
  return(circleDF)
}
