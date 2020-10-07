#' reconstred_route reconstruccion de rutas con geoposicoones en (lon,lat)
#'
#' @param data
#'
#' @return
#' @importFrom tibble as_tibble
#' @importFrom dplyr row_number
#' @importFrom osrm osrmRoute
#' @importFrom purrr map2 reduce
#' @export
#'
#' @examples

reconsted_route_fn <- function(route){
  dataRoute <- route %>%
    tibble::as_tibble() %>%
    dplyr::mutate(id = dplyr::row_number())

  routetail <- tail(dataRoute, -1) %>% split(.$id)
  routehead <- head(dataRoute, -1) %>% split(.$id)


  test_route <- purrr::map2(routehead, routetail, function(head, tail){
    osrm::osrmRoute(src = head, dst = tail,
                    returnclass = "sf",
                    overview = "full")
  }) %>%
    purrr::reduce(rbind)
  return(test_route)
}
