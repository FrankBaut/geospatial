#'A function to produce an “igraph” object with the “nodelist” and “edgelist”, which could be returned by the function readshpnw.
#'
#' @param nodelist A “nodelist” object
#' @param edgelist An “edgelist” object
#' @param weight A numberic vector to weight all the edges in the “edgelist”, of which the length equals to the number of edges;
#' @param eadf A data frame of attributes corresponding to all the edges;
#' @param Directed  TRUE if edges are directed, FALSE otherwise;
#'
#' @return 1. The weighting vector, “weight”, will be used as default for any weigted calculations with edges in the “igraph” object.
#'         2. The coordinate of each node is attached as attributes “X” and “Y”, which could be retrived via the function “get.vertex.attribute” from the package igraph.
#' @export
#'
#' @examples
#'
nodedge_list2igraph <- function (nodelist, edgelist, weight = NULL, eadf = NULL, Directed = FALSE) {
  nodes <- nodelist[, 1]
  Ne <- length(edgelist[, 1])
  Nn <- length(nodes)
  for (i in 1:Nn) {
    kk <- nodelist[i,][[1]]
    edgelist[which(edgelist[,c(2)]==kk),2] <- i
    edgelist[which(edgelist[,c(3)]==kk),3] <- i
    nodelist[i,][[1]] <- i
  }
  if (!is.null(weight)) {
    if (length(weight) != Ne && is.numeric(weight))
      stop("Please give right edge weight, which must be numeric and the same length as edges elment")
  }
  if (!is.null(eadf)) {
    if (length(eadf[, 1]) != Ne)
      stop("The eadf must be numeric and the same length as edges elment")
  }
  gr <- igraph::graph.edgelist(unique(edgelist[, c(2, 3)]), directed = T)
  gr <- igraph::set.vertex.attribute(gr, "x", V(gr), Nodes.coordinates(nodelist)[,1])
  gr <- igraph::set.vertex.attribute(gr, "y", V(gr), Nodes.coordinates(nodelist)[,2])
  gr.es <- E(gr)
  if (!is.null(weight))
    gr <- igraph::set.edge.attribute(gr, "weight", gr.es, weight)
  if (!is.null(eadf)) {
    eanms <- colnames(eadf)
    n <- length(eanms)
    for (i in 1:n) gr <- set.edge.attribute(gr, eanms[i],
                                            gr.es, eadf[, i])
  }
  gr
}
