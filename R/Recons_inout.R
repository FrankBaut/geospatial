#' It joins several gepositions by means of the shortest route in a road network both back and forth.
#'
#' @param from_lng Origin lng
#' @param from_lat Origin lat
#' @param data points away from the origin in a data frame with lng inthe firts columns and lat in the second one.
#' @param type reconstructed route from poinr A to B (out) or B to A (in)
#'
#' @return sf file
#' @export
#'
#' @examples
#' x=-96.9236
#' y=-19.527196
#' data<-geospatial::georadius(x,y,radius = 1,nPoints = 100)
#' recons_inout(x,y,data,type="out")
recons_inout<-function(from_lng,from_lat,data,type=c("in","out")){
  data$from_lng<-rep(x,dim(data)[1])
  data$from_lat<-rep(y,dim(data)[1])
  if(type=="out"){
    rutas<-foreach(i=1:dim(data)[1]) %do%
      osrm::osrmRoute(src = c(data$from_lng[i],data$from_lat[i]),
                dst = c(data$lng[i],data$lat[i]),
                returnclass = "sf",
                overview = "full")
    R_completes<-do.call(rbind,rutas)
    R_completes<-cbind(R_completas)
  }
  else{
    rutas<-foreach(i=1:dim(data)[1]) %do%
      osrm::osrmRoute(src = c(data$lng[i],data$lat[i]),
                dst = c(data$from_lng[i],data$from_lat[i]),
                returnclass = "sf",
                overview = "full")
    R_completes<-do.call(rbind,rutas)
    R_completes<-cbind(R_completas)
  }
  return(R_complets)
}
