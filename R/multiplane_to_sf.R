#' Convert MultiPlane Text File to SF Object
#'
#' This function reads a MultiPlane text file and converts it back to an `sf` object.
#'
#' @param file_path A string specifying the path to the MultiPlane text file.
#' @param imperial A logical value indicating whether the distances in the file are in imperial units (feet). Default is FALSE.
#' @return An `sf` object representing the points.
#' @importFrom sf st_sf st_sfc st_point
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom geosphere destVincentyEllipsoid
#' @examples
#' # Assume we have a MultiPlane text file saved as "output.txt"
#' # sf_object <- multiplane_to_sf("output.txt")
#' # print(sf_object)
multiplane_to_sf <- function(file_path,  imperial = FALSE) {
  # Read the text file
  data <- read.table(file_path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)

  # Extract the first line as benchmark coordinates
  benchmark_info <- strsplit(data[1,6], " ")[[1]]
  # benchmark_coords <- strsplit(benchmark_info[1], "/")[[1]]

  # Parse benchmark latitude and longitude
  parse_coord <- function(coord) {
    parts <- strsplit(coord, ":")[[1]]

    degrees <- as.numeric(substr(parts[1], 2, nchar(parts[1])))
    minutes <- as.numeric(parts[2])
    seconds <- as.numeric(parts[3])
    coord_val <- degrees + minutes / 60 + seconds / 3600
    if (substr(parts[1], 1, 1) %in% c("S", "W")) {
      coord_val <- -coord_val
    }
    return(coord_val)
  }

  benchmark_lat <- parse_coord(benchmark_info[1])
  benchmark_lon <- parse_coord(benchmark_info[3])

  # Initialize data frame to store the parsed information
  formatted_data <- data[-1, ]
  colnames(formatted_data) <- c("ID", "distance_X", "distance_Y", "z", "code", "benchmark_coord")
  formatted_data <- formatted_data[, c("distance_X", "distance_Y", "z", "code")]

  # Convert distances to numeric
  formatted_data <- formatted_data |>
    dplyr::mutate(
      distance_X = as.numeric(distance_X),
      distance_Y = as.numeric(distance_Y),
      z = as.numeric(z)
    )

  # Convert distances from feet to meters if necessary
  if (imperial) {
    formatted_data <- formatted_data |>
      mutate(
        distance_X = distance_X / 3.28084,
        distance_Y = distance_Y / 3.28084
      )
  }

  # Calculate original coordinates from distances
  calculate_coords <- function(dist_x, dist_y, benchmark_lon, benchmark_lat) {
    point_x <- geosphere::destPoint(c(benchmark_lon, benchmark_lat), 90, dist_x)
    point_y <- geosphere::destPoint(c(benchmark_lon, benchmark_lat), 0, dist_y)
    return(c(point_x[1], point_y[2]))
  }

  coords <- t(apply(formatted_data[, c("distance_X", "distance_Y")], 1, function(dist) {
    calculate_coords(dist[1], dist[2], benchmark_lon, benchmark_lat)
  }))

  # Create an sf object from the coordinates
  points <- lapply(1:nrow(coords), function(i) {
    sf::st_point(c(coords[i, 1], coords[i, 2]))
  })
  sf_points <- sf::st_sf(
    elevation = formatted_data$z,
    code = formatted_data$code,
    geometry = sf::st_sfc(points, crs = 4326)
  )

  return(sf_points)
}
