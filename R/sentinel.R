#' Retrieve and Process Sentinel-2 Data
#'
#' This function queries the Planetary Computer API for Sentinel-2 satellite imagery
#' data within a specified date range and cloud cover percentage. It processes the
#' imagery to calculate the Normalized Difference Vegetation Index (NDVI) for the
#' specified geographic region.
#'
#' @param pol An `sf` object representing the geographic region of interest.
#' @param date_debut Character. The start date for the query in 'YYYY-MM-DD' format. Default is '2018-01-01'.
#' @param date_fin Character. The end date for the query in 'YYYY-MM-DD' format. Default is '2024-12-31'.
#' @param max_nuage Numeric. The maximum allowed cloud cover percentage. Default is 50.
#' @return A list of `SpatRaster` objects, each representing NDVI for different dates.
#' @examples
#' \dontrun{
#' # Example usage:
#' library(sf)
#' # Define a simple polygon as the region of interest
#' shp <- st_as_sf(data.frame(
#'   geometry = st_sfc(st_polygon(list(rbind(
#'     c(-1, 50), c(-1, 51), c(1, 51), c(1, 50), c(-1, 50)
#'   )))),
#'   crs = 4326
#' ))
#' # Retrieve and process Sentinel-2 data
#' ndvi_list <- sentinel(shp)
#' }
#' @export
sentinel <- function(pol, date_debut = '2018-01-01', date_fin = '2024-12-31', max_nuage = 50, resample =F, month = NULL) {
  # Initialize the Planetary Computer STAC API
  planetary_computer <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")

  # Combine geometries and ensure CRS is set to WGS84 (EPSG:4326)
  polygone <- pol |> sf::st_union()
  if (is.na(sf::st_crs(polygone))) {
    sf::st_crs(polygone) <- 4326
  }
  polygone <- polygone |> sf::st_transform(4326) |> sf::st_geometry()

  # Define STAC query for Sentinel-2 data
  stac_query <- planetary_computer |>
    rstac::ext_filter(
      collection %in% c("sentinel-2-l2a") &&
        t_intersects(datetime, {{rstac::cql2_interval(date_debut, date_fin)}}) &&
        s_intersects(geometry, {{rstac::cql2_bbox_as_geojson(polygone |> sf::st_bbox())}}) &&
        `eo:cloud_cover` < {{max_nuage}}
    ) |>
    rstac::post_request()

  # Function to create VSICURL URLs for accessing assets
make_vsicurl_url <- function(base_url) {
  paste0(
    "/vsicurl", 
    "?pc_url_signing=yes",
    "&pc_collection=sentinel-2-l2a",
    "&url=",
    base_url
  )
}

  # Retrieve URLs for relevant Sentinel-2 bands
  rouge <- rstac::assets_url(stac_query, "B04")
  NIR <- rstac::assets_url(stac_query, "B08")
  SCL <- rstac::assets_url(stac_query, "SCL")

  

  # Extract dates from the STAC query results
  a <- tibble::tibble(date = rstac::items_datetime(stac_query)) |>
    dplyr::mutate(date = substr(date, 1, 10))
    if(!is.null(month)){

    a=a |> 
    dplyr::mutate(m = lubridate::month(lubridate::as_date(date))) |> 
    dplyr::filter(m %in% month)
}
  # Function to process Sentinel-2 imagery
   s2 <- function(x) {
      # Transform polygon to match the CRS of the Sentinel-2 data
  p <- polygone |>
    sf::st_transform(terra::crs(terra::rast(make_vsicurl_url(x)))) |>
    terra::vect()

   r= terra::mask(terra::crop(terra::rast(make_vsicurl_url(x)), p), p)
  resolution = terra::res(r)
  r=r |> terra::project(terra::crs(terra::rast(make_vsicurl_url(rouge[1]))))
   r2=r

   terra::res(r2)=resolution 
  terra::resample(r, r2)
  
  }
  # Initialize a list to store NDVI rasters
  l <- list()
n=NULL
  # Process imagery for each unique date
  for (i in as.character(sort(lubridate::as_date(unique(a$date))))) {
    print(paste0("Processing date: ", i))
    cloud <- SCL[which(a$date == i)] |>
      lapply(s2) |>
      terra::sprc() |>
      terra::mosaic()
    
    cloud <- cloud == 3 | cloud == 7 | cloud == 8 | cloud == 9 | cloud == 10 | cloud == 11
    p <- polygone |>
    sf::st_transform(terra::crs(cloud)) |>
    terra::vect()

    cloudpct = as.numeric(terra::zonal(cloud, p, na.rm=TRUE))
  
    if(!(((cloudpct)*100)>max_nuage)){

    r1 <- rouge[which(a$date == i)] |>
      lapply(s2) |>
      terra::sprc() |>
      terra::mosaic()
    nir1 <- NIR[which(a$date == i)] |>
      lapply(s2) |>
      terra::sprc() |>
      terra::mosaic()
      
    # Calculate NDVI
    ndvi <- (nir1 - r1) / (nir1 + r1)
  if(resample){
        s = ndvi
    terra::res(s)=1
    ndvi <- terra::resample(ndvi, s, method="bilinear")
    p <- polygone |>
    sf::st_transform(terra::crs(ndvi)) |>
    terra::vect()
    ndvi=terra::mask(ndvi, p)
    }

    # Mask out clouds from the NDVI raster
    cloud <- terra::resample(cloud, ndvi)
    ndvi <- terra::mask(ndvi, !cloud)
    names(ndvi)=i
    l[[i]]<-ndvi
    n= c(n,i)
  }}

  names(l) <- n
  return(l)
}
