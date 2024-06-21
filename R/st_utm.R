#' Convert Coordinates to UTM
#'
#' This function transforms an `sf` object to UTM coordinates based on its centroid.
#'
#' @param x An `sf` object.
#' @return The transformed `sf` object with UTM coordinates.
#' @examples
#' library(sf)
#' # Example usage with a simple polygon
#' poly <- st_sfc(st_polygon(list(matrix(c(0, 0, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE))))
#' sf_poly <- st_sf(geometry = poly, crs = 4326)
#' utm_poly <- st_utm(sf_poly)
#' print(utm_poly)
st_utm <- function(x) {
  # Check if input is an sf object
  if (!inherits(x, "sf")) {
    stop("Input must be an sf object")
  }

  # Calculate centroid coordinates
  centroid_coords <- x |>
    sf::st_union() |>
    sf::st_transform(4326) |>
    sf::st_centroid()|>
    sf::st_coordinates()|>
    tibble::as_tibble()

  # Calculate UTM zone
  zone_utm <- floor((centroid_coords$X + 180) / 6) + 1

  # Calculate UTM north/south code
  north_south <- ifelse(centroid_coords$Y >= 0, 32600, 32700)

  # Return UTM value
  x[[ attr(x, "sf_column") ]] <- sf::st_transform(sf::st_geometry(x), zone_utm + north_south)
  return(x)
}
