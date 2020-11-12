#' Filter geopoints that fall in a bounding box
#'
#' @param data data frame with lng and lat colunm names
#' @param x paraneter lng1 of bounding box
#' @param y paraneter lat1 of bounding box
#' @param X paraneter lng2 of bounding box
#' @param Y paraneter lat2 of bounding box
#'
#' @return Filter geopoints
#' @export
#'
#' @examples
#' example
filter_bbox<-function(data,x,y,X,Y){
  data<-data %>% dplyr::filter(between(lat, y, Y))
  data<- data %>% dplyr::filter(between(lng,x,X))
}
