#' InPolypoints
#' Fill a polygon with random points inside.
#' @param shapefile A polygon file
#' @param nPoints  Number of points inside the polygon
#' @param type  character; "random" for completely spatial random;
#' "regular" for regular (systematically aligned) sampling;
#' "stratified" for stratified random (one single random location in each "cell");
#' "nonaligned" for nonaligned systematic sampling (nx random y coordinates, ny random x coordinates);
#'  "hexagonal" for sampling on a hexagonal lattice;
#'  "clustered" for clustered sampling;
#'  "Fibonacci" for Fibonacci sampling on the sphere (see references).
#'
#' @return Spatial points
#' @export
#'
#' @examples
InPolypoints<-function(shapefile,nPoints,type){
  bb<-bbox(shapefile) #%>% t()%>% as.data.frame()
  bb<-rbind(bb[,1],c(bb[1,1],bb[2,2]),bb[,2],c(bb[1,2],bb[2,1]),bb[,1])
  bb = Polygon(bb)
  bb = SpatialPolygons(list(Polygons(list(bb), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  pts <- sp::spsample(bb,nPoints , type = type)
  p1<-over(pts, shapefile)
  indx<-which(!is.na(p1[,1]), arr.ind=TRUE)
  pts1<-pts[indx,]
}
