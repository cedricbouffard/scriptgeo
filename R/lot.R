#' Retrieve Spatial Data for a Lot Number
#'
#' This function queries the GIS service to retrieve spatial data for a given lot number.
#' It constructs a query based on the lot number, sends a request to the GIS service,
#' and returns the data as a spatial object.
#'
#' @param numero_lot Numeric or character. The lot number to query.
#' @return An `sf` object containing the spatial data for the specified lot.
#' @examples
#' \dontrun{
#' # Example usage:
#' lot_data <- lot(123456)
#' plot(lot_data)
#' }
#' @export
lot <- function(numero_lot) {
  # Format the lot number with spaces as thousand separators
  numlot <- formatC(numero_lot, big.mark = " ", format = "f", digits = 0)

  # SQL template for the query
  sql.template <- "NO_LOT IN (%s)"

  # Construct the query string
  query <- sprintf(sql.template, toString(sprintf("'%s'", numlot)))

  # Parse the base URL for the GIS service
  url <- httr:::parse_url("https://services3.arcgis.com/0lL78GhXbg1Po7WO/ArcGIS/rest/services")

  # Append the specific path for the desired service
  url$path <- paste(url$path, "cadastre_bd_allegee/FeatureServer/0/query", sep = "/")

  # Add query parameters to the URL
  url$query <- list(
    where = query,
    outFields = "*",
    returnGeometry = "true",
    f = "geojson"
  )

  # Build the complete URL for the request
  request <- httr::build_url(url)

  # Retrieve and read the data as a spatial object
  sf::st_read(request)
}
