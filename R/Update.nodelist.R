#' Title
#'
#' @param nodexlist
#' @param nodeylist
#' @param nodelist
#' @param nx
#' @param ny
#'
#' @return
#' @export
#'
#' @examples
Update.nodelist<-function (nodexlist, nodeylist, nodelist, nx, ny)
{
  Nid <- length(nodexlist)
  if (Nid == 0) {
    Nid <- Nid + 1
    id <- as.integer(Nid)
    isUpdate <- TRUE
  }
  else {
    tag <- as.integer(-1)
    tag <- .C("nodeExisted", nodexlist, nodeylist, as.integer(Nid),
              nx, ny, tag)[[6]]
    if (tag != -1) {
      id <- tag
      isUpdate <- FALSE
    }
    else {
      Nid <- Nid + 1
      id <- as.integer(Nid)
      isUpdate <- TRUE
    }
  }
  res <- list(id, isUpdate)
  res
}
