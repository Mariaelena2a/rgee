#' Extract values for EE ImageCollections objects
#'
#' Extract values for an Image or ImageCollection spatial object
#' at the locations of geometry object. You can use ee.Geometries,
#' ee.Features, ee.FeatureCollection and sf objects.
#'
#' @param x ee$Image or ee$ImageCollection.
#' @param y ee$Geometry, ee$Feature, ee$FeatureCollection or sf objects.
#' @param fun ee$Reducer object. Function to summarize the values. See more details.
#' @param scale A nominal scale in meters to work with.
#' @param id Character. Column name to be used as a geometry index.
#' @param ... reduceRegions additional parameters. See
#' reticulate::ee_help(ee$Image()$reduceRegions) for more details.
#' @importFrom sf st_geometry st_geometry<-
#' @details
#' The fun arguments just admit Reduced objects that return one value.
#' These are:
#' \itemize{
#' \item  \strong{allNonZero}: Returns a Reducer that returns 1 if all the
#' inputs are non-zero, 0 otherwise. \cr
#' \item \strong{anyNonZero}: Returns a Reducer that returns 1 if any of the
#' inputs are non-zero, 0 otherwise. \cr
#' \item \strong{bitwiseAnd}: Returns a Reducer that computes the bitwise-and
#' the inputs are summed up.
#' \item \strong{bitwiseOr}: Returns a Reducer that computes the bitwise-or
#' the inputs are summed up.
#' \item \strong{count}: Returns a Reducer that computes the number of
#' non-null inputs.
#' \item \strong{first}: Returns a Reducer that returns the first of the inputs.
#' \item \strong{firstNonNull}: Returns a Reducer that returns the first of
#' the non-null inputs.
#' \item \strong{kurtosis}: Returns a Reducer that Computes the kurtosis of
#' the inputs.
#' \item \strong{last}: Returns a Reducer that returns the last of the inputs.
#' \item \strong{lastNonNull}: Returns a Reducer that returns the last of the
#' non-null inputs.
#' \item \strong{max}: Creates a reducer that returns the maximum value of
#' the (first) input. If numInputs is greater than one, also returns the
#' corresponding values of the additional inputs.
#' \item \strong{mean}: Returns a Reducer that computes the (weighted)
#' arithmetic mean of the inputs.
#' \item \strong{median}: Create a reducer that will compute the median of
#' the inputs. For a small number of inputs (up to maxRaw), the median will be
#' computed directly; for a larger number of inputs, the median will be derived
#' from the histogram.
#' \item \strong{min}: Creates a reducer that returns the minimum value
#' of the (first) input.  If numInputs is greater than one, also returns
#' additional inputs.
#' \item \strong{mode}: Create a reducer that will compute the mode of the
#' inputs.  For a small number of inputs (up to maxRaw) the mode will be
#' computed directly; for a larger number of inputs the mode will be derived
#' from a histogram.
#' \item \strong{product}: Returns a Reducer that computes the product of
#' the inputs.
#' \item \strong{sampleStdDev}: Returns a Reducer that computes the
#' standard deviation of the inputs.
#' \item \strong{sampleVariance}: Returns a Reducer that computes the
#' variance of the inputs.
#' \item \strong{stdDev}: Returns a Reducer that computes the standard
#' deviation of the inputs.
#' \item \strong{sum}: Returns a Reducer that computes the (weighted) sum
#' of the inputs.
#' \item \strong{variance}: Returns a Reducer that computes the variance
#' of the inputs.
#' }
#' @examples
#' library(rgee)
#' library(sf)
#'
#' # authenticate and initialize Earth Engine
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize()
#'
#' # get monthly precipitation from terraclimate
#' terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
#'   filterDate("2000-01-01", "2001-01-01")$
#'   map(function(x) x$select("pr"))
#'
#' # extract values with polygons
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
#'   st_transform(4326)
#' ee_nc_rain <- ee_extract(
#'   x = terraclimate, y = nc, fun = ee$Reducer$max(),
#'   id = "FIPS"
#' )
#' ee_nc_rain <- merge(nc, ee_nc_rain, by = "FIPS")
#' plot(ee_nc_rain["X200006"],
#'   main = "2001 Jan Precipitation - Terraclimate",
#'   reset = FALSE
#' )
#'
#' ## plot time series
#' ee_nc <- ee_nc_rain
#' st_geometry(ee_nc) <- NULL
#' time_serie <- as.numeric(ee_nc[1, sprintf("X%s", 200001:200012)])
#' main <- sprintf("2001 Precipitation - %s", ee_nc$NAME[1])
#' plot(time_serie, ylab = "pp (mm/month)", type = "l", lwd = 1.5, main = main)
#' points(time_serie, pch = 20, lwd = 1.5, cex = 1.5)
#' @export
ee_extract <- function(x, y, fun = ee$Reducer$mean(), scale = 1000,
                       id = NULL, ...) {
  oauth_func_path <- system.file("python/ee_extract.py", package = "rgee")
  extract_py <- ee_source_python(oauth_func_path)
  if (any(c("sf", "sfc", "sfg") %in% class(y))) y <- sf_as_ee(y)
  if (is.null(id)) {
    y <- ee$FeatureCollection(y)$map(
      function(x) x$set("ee_ID", x$get("system:index"))
    )
  } else {
    y <- ee$FeatureCollection(y)$map(function(x) x$set("ee_ID", x$get(id)))
  }
  fun_name <- gsub("Reducer.", "", fun$getInfo()["type"])
  triplets <- ee$ImageCollection(x)$map(function(image) {
    image$reduceRegions(
      collection = y,
      reducer = fun,
      scale = scale
    )$map(function(f) f$set("imageId", image$id()))
  })$flatten()
  table <- extract_py$
    table_format(triplets, "ee_ID", "imageId", fun_name)$
    map(function(feature) {
    feature$setGeometry(NULL)
  })

  table_geojson <- ee_py_to_r(table$getInfo())
  class(table_geojson) <- "geo_list"
  table_sf <- geojson_sf(table_geojson)
  st_geometry(table_sf) <- NULL
  table_sf <- table_sf[, order(names(table_sf))]
  table_sf["id"] <- NULL
  if (!is.null(id)) {
    names(table_sf)[names(table_sf) == "ee_ID"] <- id
  }
  table_sf
}
