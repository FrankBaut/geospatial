#datos de apatial linesdataframe a una lista de nodos y aristas
#shp a nodedge_list
shp2nodedge_list<-function (ntdata, ELComputed = FALSE, longlat = FALSE, Detailed = FALSE,
                            ea.prop = NULL) {
  if (!is(ntdata, "SpatialLinesDataFrame"))
    stop("Input data is not a proper spatial network data frame, here only SpatialLinesDataFrame is accepted.")
  Coords <- sp::coordinates(ntdata)
  numEdges <- length(Coords)
  nodelist <- c()
  edgelist <- c()
  Eadf <- data.frame(ntdata)
  Eadf.names <- names(Eadf)
  edgeID.vec <- 1:numEdges
  Eadf <- data.frame(edgeID.vec, data.frame(ntdata))
  names(Eadf) <- c("EdgeID", Eadf.names)
  id.idx <- 1
  if (Detailed) {
    if (is.null(ea.prop))
      stop("If a detailed graph is to be built, the properties of its attributes has to be specified")
    else {
      if (length(ea.prop) != dim(Eadf)[2] - 1)
        stop("All the properties of attributs should be specified except EdgeID")
    }
  }
  nodexlist <- vector(mode = "double", length = 0)
  nodeylist <- vector(mode = "double", length = 0)
  edgelength <- vector(mode = "double", length = 0)
  fromid <- 0
  toid <- 0
  if (Detailed) {
    for (i in 1:numEdges) {
      M <- dim(Coords[[i]][[1]])[1]
      SEl <- as.double(10)
      SEl <- .C("edgelength", as.double(Coords[[i]][[1]][,1]), as.double(Coords[[i]][[1]][, 2]),
                as.integer(M),
                SEl, as.integer(longlat))[[4]]
      edgeid <- edgeID.vec[i]
      nx <- as.double(Coords[[i]][[1]][1, 1])
      ny <- as.double(Coords[[i]][[1]][1, 2])
      res1 <- Update.nodelist(nodexlist, nodeylist, nodelist,nx, ny)
      fromid <- res1[[1]]
      if (res1[[2]]) {
        node <- list(res1[[1]], c(nx, ny))
        nodelist <- rbind(nodelist, node)
        nodexlist <- c(nodexlist, nx)
        nodeylist <- c(nodeylist, ny)
      }
      for (j in 2:M) {
        nx <- as.double(Coords[[i]][[1]][j,1])
        ny <- as.double(Coords[[i]][[1]][j,2])
        res1 <- Update.nodelist(nodexlist, nodeylist,nodelist, nx, ny)
        toid <- res1[[1]]
        if (res1[[2]]) {
          node <- list(res1[[1]], c(nx, ny))
          nodelist <- rbind(nodelist, node)
          nodexlist <- c(nodexlist, nx)
          nodeylist <- c(nodeylist, ny)
        }
        El <- as.double(0)
        El <- .C("edgelength", as.double(Coords[[i]][[1]][c(j-1, j), 1]),
                 as.double(Coords[[i]][[1]][c(j- 1, j), 2]),
                 as.integer(2), El, as.integer(longlat))[[4]]
        ROL <- El/SEl
        edgelist <- Update.edgelist(edgelist, edgeid,
                                    fromid, toid, ROL, Detailed = Detailed)
        if (ELComputed) {
          edgelength <- c(edgelength, El)
        }
        fromid <- toid
      }
    }
  }
  else {
    for (i in 1:numEdges) {
      M <- dim(Coords[[i]][[1]])[1]
      nx <- as.double(Coords[[i]][[1]][1, 1])
      ny <- as.double(Coords[[i]][[1]][1, 2])
      res1 <- Update.nodelist(nodexlist, nodeylist, nodelist,nx, ny)
      fromid <- res1[[1]]
      if (res1[[2]]) {
        node <- list(res1[[1]], c(nx, ny))
        nodelist <- rbind(nodelist, node)
        nodexlist <- c(nodexlist, nx)
        nodeylist <- c(nodeylist, ny)
      }
      nx <- as.double(Coords[[i]][[1]][M, 1])
      ny <- as.double(Coords[[i]][[1]][M, 2])
      res1 <- Update.nodelist(nodexlist, nodeylist, nodelist,
                              nx, ny)
      toid <- res1[[1]]
      if (res1[[2]]) {
        node <- list(res1[[1]], c(nx, ny))
        nodelist <- rbind(nodelist, node)
        nodexlist <- c(nodexlist, nx)
        nodeylist <- c(nodeylist, ny)
      }
      edgeid <- edgeID.vec[i]
      edgelist <- Update.edgelist(edgelist, edgeid, fromid,
                                  toid, Detailed = Detailed)
      El <- as.double(0)
      if (ELComputed) {
        El <- .C("edgelength", as.double(Coords[[i]][[1]][,1]), as.double(Coords[[i]][[1]][, 2]),
                 as.integer(M),
                 El, as.integer(longlat))[[4]]
        edgelength <- c(edgelength, El)
      }
    }
  }
  if (Detailed) {
    res <- extend.eadf(edgelist, Eadf, ea.prop)
    edgelist <- res[[1]]
    Eadf <- res[[2]]
  }
  rownames(edgelist) <- NULL
  rownames(nodelist) <- NULL
  res <- list(Detailed, nodelist, edgelist, edgelength, Eadf,
              nodexlist, nodeylist)
}
