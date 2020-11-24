#' Given the disconnected components of a network, the network returns with connections
#'
#' @param g igrap network
#'
#' @return igrap network
#' @export
#'
#' @examples
#' make example
maxigraphComponet<-function(g){
  bigger <- induced_subgraph(
    g, V(g)[components(g)$membership == which.max(components(g)$csize)]
  )
  return(bigger)
}
