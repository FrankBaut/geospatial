
#reconstruccion de rutas con geoposicoones en (lon,lat)
reconsted_route<-function(data){
  route<-foreach(j= 1:(dim(data)[1]-1)) %do% {
    from <- c(data$lng[j],data$lat[j])
    to <-c(data$lng[j+1],data$lat[j+1])
    tryCatch(osrmRoute(src = from, dst = to,returnclass = "sf",overview = "full"),
             error = function(e) NULL)
  }
  route[sapply(route, is.null)] <- NULL
  route<-do.call(rbind, route)
  return(route)
}
