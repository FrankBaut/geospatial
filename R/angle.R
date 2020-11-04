#' angle
#'
#' @param from_lng Origin longitude
#' @param from_lat Origin latitude
#' @param to_lng Final longitude
#' @param to_lat Final latitude
#' @param respecto Axis where the angles start to be measured, by default is x-axis with angle 0 (respecto=0)
#'
#' @return angle in degrees
#' @export
#'
#' @examples
#' x=-96.9236
#' y=-19.527196
#' X=-96.91404
#' Y=-19.5272
#' angle(x,y,X,Y,respecto=90) #axis y in 90 degrees
angle<-function(from_lng,from_lat,to_lng,to_lat,respecto=0){
  (respecto - atan2(from_lat - to_lat,from_lng - to_lng) * 180/pi ) %%360
}

