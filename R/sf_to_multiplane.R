#' Convert SF Points to MultiPlane Coordinates
#'
#' This function converts spatial points and a benchmark into a format suitable for MultiPlane, including distance calculations and elevation values.
#'
#' @param sf_points An `sf` object containing the points to be converted.
#' @param sf_benchmark An `sf` object containing a single benchmark point.
#' @param z_col A string specifying the column name in `sf_points` for elevation values.
#' @param boundary_col A string specifying the column name in `sf_points` for boundary codes.
#' @param dest_file A string specifying the destination file path for the output.
#' @param elev_bm A numeric value for the elevation of the benchmark point. Default is 0.
#' @param imperial A logical value indicating whether to convert distances to imperial units (feet). Default is FALSE.
#' @return A `tibble` containing the formatted data.
#' @examples
#' library(sf)
#' # Example usage with sample data
#' points <- st_sfc(st_point(c(1, 1)), st_point(c(2, 2)), crs = 4326)
#' benchmark <- st_sfc(st_point(c(0, 0)), crs = 4326)
#' sf_points <- st_sf(id = 1:2, elevation = c(10, 20), boundary = c("A", "B"), geometry = points)
#' sf_benchmark <- st_sf(geometry = benchmark)
#' result <- sf_to_multiplane(sf_points, sf_benchmark, "elevation", "boundary", "output.txt")
#' print(result)
sf_to_multiplane <- function(sf_points, sf_benchmark, z_col, boundary_col, dest_file, elev_bm = 0, imperial = FALSE) {

  # Ensure the benchmark is a single point
  if (nrow(sf_benchmark) != 1) {
    stop("Benchmark must be a single point.")
  }

  # Transform coordinates to WGS 84
  sf_points <- sf::st_transform(sf_points, 4326)
  sf_benchmark <- sf::st_transform(sf_benchmark, 4326)

  # Extract coordinates
  coords_points <- sf::st_coordinates(sf_points)
  coords_benchmark <- sf::st_coordinates(sf_benchmark)

  # Calculate distances
  distances <- apply(coords_points, 1, function(point) {
    dist_x <- geosphere::distVincentyEllipsoid(
      c(coords_benchmark[1], coords_benchmark[2]), c(point[1], coords_benchmark[2])
    )
    if (point[1] < coords_benchmark[1]) dist_x <- -dist_x

    dist_y <- geosphere::distVincentyEllipsoid(
      c(coords_benchmark[1], coords_benchmark[2]), c(coords_benchmark[1], point[2])
    )
    if (point[2] < coords_benchmark[2]) dist_y <- -dist_y

    if (imperial) {
      c(dist_x, dist_y) * 3.28084
    } else {
      c(dist_x, dist_y)
    }
  })

  # Convert to tibble
  distance_tibble <- tibble::as_tibble(t(distances), .name_repair = "minimal")
  names(distance_tibble) <- c("distance_X", "distance_Y")
  distance_tibble <- tibble::tibble(distance_X = 0, distance_Y = 0) %>%
    rbind(distance_tibble)
  distance_tibble$z <- c(elev_bm, sf_points[[z_col]])
  distance_tibble$code <- c("MB", sf_points[[boundary_col]])

  # Format data
  formatted_data <- distance_tibble %>%
    dplyr::mutate(
      ID = dplyr::row_number(),
      ID = dplyr::if_else(ID == 1, sprintf("%04d", ID), as.character(ID)),
      distance_X = ifelse(ID == "0001", formatC(distance_X, format = "f", digits = 3), formatC(distance_X, format = "f", digits = 2)),
      distance_Y = ifelse(ID == "0001", formatC(distance_Y, format = "f", digits = 3), formatC(distance_Y, format = "f", digits = 2)),
      z = formatC(z, format = "f", digits = 6)
    ) %>%
    dplyr::relocate(ID)

  # Helper function to format coordinates
  format_coord <- function(coord, type) {
    degrees <- abs(coord) %/% 1
    minutes <- (abs(coord) %% 1 * 60) %/% 1
    seconds <- (((abs(coord) %% 1) * 60) %% 1) * 60
    formatted_coord <- paste0(
      type, format(degrees, width = 2, flag = "0"), ":",
      formatC(minutes, width = 2, format = "d", flag = "0"), ":",
      formatC(seconds, width = 6, digits = 3, format = "f", flag = "0")
    )
    return(formatted_coord)
  }

  benchmark_coord <- paste0(
    format_coord(coords_benchmark[2], ifelse(coords_benchmark[2] >= 0, "N", "S")), " / ",
    format_coord(coords_benchmark[1], ifelse(coords_benchmark[1] >= 0, "E", "W")), "\t0.000"
  )

  formatted_data <- formatted_data %>%
    dplyr::mutate(
      benchmark_coord = dplyr::if_else(ID == "0001", benchmark_coord, "")
    )

  # Write to a text file
  write.table(
    formatted_data,
    file = dest_file,
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )

  return(formatted_data)
}
