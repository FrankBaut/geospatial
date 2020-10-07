#' Title
#'
#' @param nodelist
#'
#' @return
#' @export
#'
#' @examples
Nodes.coordinates<-function (nodelist = list())
{
  Nn <- length(nodelist[, 1])
  Nodesx <- vector(mode = "double", length = 0)
  Nodesy <- vector(mode = "double", length = 0)
  for (i in 1:Nn) {
    Nodesx <- c(Nodesx, as.double(nodelist[i, 2][[1]][1]))
    Nodesy <- c(Nodesy, as.double(nodelist[i, 2][[1]][2]))
  }
  Nodesxy <- cbind(Nodesx, Nodesy)
  colnames(Nodesxy) <- c("X", "Y")
  Nodesxy
}
