#' Title
#'
#' @param Edgelist
#' @param edgeid
#' @param fromid
#' @param toid
#' @param ROL
#' @param Detailed
#'
#' @return
#' @export
#'
#' @examples
Update.edgelist<-function (Edgelist, edgeid, fromid, toid, ROL, Detailed)
{
  if (Detailed) {
    eid <- as.integer((length(Edgelist)%/%4) + 1)
    edge <- c(eid, edgeid, fromid, toid, ROL)
  }
  else edge <- c(edgeid, fromid, toid)
  Edgelist <- rbind(Edgelist, edge)
  Edgelist
}
