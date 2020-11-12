#' Get the nearby points given a radius
#'
#' @param data data frame with lng and lat columns
#' @param sepDistance interval of geopositions in meters
#'
#' @return data frame
#' @export
#'
#' @examples
#' data<-geospatial::georadius(-96.1312,18.1232,radius = 100,1000)
#' geospatial::geo_nearestPoints(data,sepDistance = 120)
geo_nearestPoints<-function(data,sepDistance){
  xy <-data[,c(1,2)]
  spdf <- sp::SpatialPointsDataFrame(coords = xy,
                                     data = data,
                                     proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  d<-geosphere::distm(spdf)
  min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])
  newdata <- cbind(data, data[min.d,], apply(d, 1, function(x) sort(x, decreasing=F)[2]))
  colnames(newdata) <- c(colnames(data),"n.lat", 'n.lng', "distance(meters)")
  newdata <- newdata %>%dplyr::filter("distance(meters)">sepDistance) %>% dplyr::select(lng,lat)
}

