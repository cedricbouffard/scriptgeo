# scriptgeo
**scriptgeo** is a  R package offering a mix of geospatial scripts for querying, processing, and visualizing spatial data. It includes functions to retrieve quebec cadastrial information from GIS services and compute the Normalized Difference Vegetation Index (NDVI) from Sentinel-2 satellite imagery.

## Table of Contents
- [Installation](#installation)
- [Functions](#functions)
  - [lot()](#lot)
  - [sentinel()](#sentinel)

## Installation

To install **scriptgeo**, you can use the `devtools` package in R to install it directly from GitHub:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install scriptgeo from GitHub
devtools::install_github("cedricbouffard/scriptgeo")
```
## Functions

### `lot()`

Retrieve spatial data for specified lot numbers from a GIS service.

**Parameters**:
- `numero_lot`: Numeric or character. The lot number to query.

**Returns**:
- An `sf` object containing spatial data for the specified lot.

### `sentinel()`

Query and process Sentinel-2 satellite imagery to compute NDVI for a given geographical region over a specified time period.

**Parameters**:
- `pol`: An `sf` object representing the geographic region of interest.
- `date_debut`: Character. The start date for the query in 'YYYY-MM-DD' format. Default is '2018-01-01'.
- `date_fin`: Character. The end date for the query in 'YYYY-MM-DD' format. Default is '2024-12-31'.
- `max_nuage`: Numeric. The maximum allowed cloud cover percentage. Default is 50.

**Returns**:
- A list of `SpatRaster` objects, each representing NDVI for different dates.

**Usage Example**:
```r
library(scriptgeo)
library(sf)

# Define a simple polygon as the region of interest
pol <- st_as_sf(data.frame(
  geometry = st_sfc(st_polygon(list(rbind(
    c(-1, 50), c(-1, 51), c(1, 51), c(1, 50), c(-1, 50)
  )))),
  crs = 4326
))

# Retrieve and process Sentinel-2 data
ndvi_list <- sentinel(pol)

# Plot the NDVI
terra::plot(ndvi_list)
