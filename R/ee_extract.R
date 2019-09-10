#' Extract values from EE ImageCollections objects
#'
#' Extract values from a Image or ImageCollection spatial object at the locations
#' of geometry object. You can use ee.Geometries, ee.Features,
#' ee.FeatureCollection and sf objects.
#' @param x ee$Image or ee$ImageCollection.
#' @param y ee$Geometry, ee$Feature, ee$FeatureCollection or sf objects.
#' @param fun ee$Reducer object. Function to summarize the values. See details.
#' @param scale A nominal scale in meters of the projection to work in.
#' @param sf Logical. TRUE returns a sf object
#' @param id Character. Name of the column to be used as a geometry index.
#' @param ... reduceRegions aditional parameters. See ee_help(ee$Image()$reduceRegions) for details.
#' @details
#' The fun arguments just admit Reducer objects that return one value. These are:
#' \itemize{
#' \item  \strong{allNonZero}: Returns a Reducer that returns 1 if all of its inputs
#' are non-zero, 0 otherwise. \cr
#' \item \strong{anyNonZero}: Returns a Reducer that returns 1 if any of its inputs are
#' non-zero, 0 otherwise. \cr
#' \item \strong{bitwiseAnd}: Returns a Reducer that computes the bitwise-and summation
#' of its inputs.
#' \item \strong{bitwiseOr}: Returns a Reducer that computes the bitwise-or summation of
#' its inputs.
#' \item \strong{count}: Returns a Reducer that computes the number of non-null inputs.
#' \item \strong{first}: Returns a Reducer that returns the first of its inputs.
#' \item \strong{firstNonNull}: Returns a Reducer that returns the first of its non-null inputs.
#' \item \strong{kurtosis}: Returns a Reducer that Computes the kurtosis of its inputs.
#' \item \strong{last}: Returns a Reducer that returns the last of its inputs.
#' \item \strong{lastNonNull}: Returns a Reducer that returns the last of its non-null inputs.
#' \item \strong{max}: Creates a reducer that outputs the maximum value of its (first) input.
#' If numInputs is greater than one, also outputs the corresponding values of the additional
#' inputs.
#' \item \strong{mean}: Returns a Reducer that computes the (weighted) arithmetic mean of its inputs.
#' \item \strong{median}: Create a reducer that will compute the median of the inputs.
#' For small numbers of inputs (up to maxRaw) the median will be computed directly; for
#' larger numbers of inputs the median will be derived from a histogram.
#' \item \strong{min}: Creates a reducer that outputs the minimum value of its (first) input.  If
#' numInputs is greater than one, also outputs the corresponding values of the
#' additional inputs.
#' \item \strong{mode}: Create a reducer that will compute the mode of the inputs.  For small
#' numbers of inputs (up to maxRaw) the mode will be computed directly; for
#' larger numbers of inputs the mode will be derived from a histogram.
#' \item \strong{product}: Returns a Reducer that computes the product of its inputs.
#' \item \strong{sampleStdDev}: Returns a Reducer that computes the sample standard deviation of its inputs.
#' \item \strong{sampleVariance}: Returns a Reducer that computes the sample variance of its inputs.
#' \item \strong{stdDev}: Returns a Reducer that computes the standard deviation of its inputs.
#' \item \strong{sum}: Returns a Reducer that computes the (weighted) sum of its inputs.
#' \item \strong{variance}: Returns a Reducer that computes the variance of its inputs.
#' }
#' @examples
#' library(rgee)
#' library(sf)
#' ee_Initialize()
#'
#' terraclimate = ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
#'   filterDate("2000-01-01","2001-01-01")$
#'   map(function(x) x$select("pr"))
#'
#' nc = st_read(system.file("shape/nc.shp", package="sf"),quiet = TRUE) %>%
#'   st_transform(4326) %>%
#'   sf_as_ee()
#' fun <- ee$Reducer$max()
#' eenc_rain = ee_extract(x = terraclimate, y = nc, fun =  fun, id = "FIPSNO")
#' plot(eenc_rain[2],main="2001 Jan Precipitation - Terraclimate", reset = FALSE)
#'
#' ## time series
#' dev.off()
#' ee_nc <- ee_nc_rain
#' st_geometry(ee_nc) = NULL
#' time_serie <- as.numeric(ee_nc[1,2:12])
#' plot(time_serie, ylab = "pp (mm/month)", type = "l", lwd = 1.5, main = 'Precipitation')
#' points(time_serie, pch = 20, lwd = 1.5, cex = 1.5)
#' @export
ee_extract <- function(x, y, fun = ee$Reducer$mean(), scale = 1000, id = NULL, sf = TRUE, ...) {
  oauth_func_path <- system.file("python/ee_extract.py", package = "rgee")
  extract_py <- rgee:::ee_source_python(oauth_func_path)
  if (any(c("sf", "sfc", "sfg") %in% class(y))) y <- sf_as_ee(y)
  if (is.null(id)) {
    y <- ee$FeatureCollection(y)$map(function(x) x$set("ID", x$get("system:index")))
  } else {
    y <- ee$FeatureCollection(y)$map(function(x) x$set("ID", x$get(id)))
  }
  fun_name <- gsub("Reducer.", "", fun$getInfo()["type"])
  triplets <- ee$ImageCollection(x)$map(function(image) {
    image$reduceRegions(
      collection = y,
      reducer = fun,
      scale = scale
    )$map(function(f) f$set("imageId", image$id()))
  })$flatten()
  table <- extract_py$table_format(triplets, "ID", "imageId", fun_name)
  if (sf) {
    table_geojson <- ee_py_to_r(table$getInfo())
    class(table_geojson) <- "geo_list"
    table_sf <- geojson_sf(table_geojson)
    geom_table <- st_geometry(table_sf)
    st_geometry(table_sf) <- NULL
    table_sf <- table_sf[, order(names(table_sf))]
    table_sf <- st_sf(table_sf, geometry = geom_table)
    if (!is.null(id)) {
      names(table_sf)[names(table_sf) == "id"] <- id
    }
    return(table_sf)
  } else {
    return(table)
  }
}