#' Upload sf or stars objects into a GEE asset
#'
#' Upload images and vectors into Google Earth Engine asset
#'
#' @param x Character, sf or stars object to be uploaded into a GEE asset.
#' @param filename Character. Asset in the destination path,
#' e.g. users/pinkiepie/myponycollection.
#' @param bucket bucketname which you are uploading. See details.
#' @param properties List. Set of parameters to be set up as a property of
#' an EE object. See details.
#' @param start_time Character. Timestamp associated with the asset. The
#' initial time stamp is set up as a nominal image acquisition time for single
#' scenes.
#' @param end_time Character. Useful for assets that correspond to a certain
#' interval of time. The ending time stamp is set up as a nominal image
#' acquisition time for single scenes.
#' @param selenium_params List. Optional parameters when bucket is NULL.
#' For parameters to set selenium. See details.
#' @param clean Logical; If is TRUE the cache is cleaned, see Description.
#' @param reinit Logical; run ee_Initialize(gcs=TRUE) before uploading
#' @param quiet Logical. Suppress info message.
#' @param ... ignored
#' @importFrom methods is as
#' @importFrom sf write_sf
#' @details
#' For uploading process, it is necessary an authorization to read & write
#' into a Google Cloud Storage (GCS) bucket. Earth Engine provides a
#' provisional authorization for free space on GCS through gs://earthengine-uploads/. If
#' the bucket argument is absent, this function will use Selenium driver to
#' get access to the URL mentioned below, see \link{ee_upload_file_to_gcs}
#' for details. To install and check the Selenium drivers in Google Chrome is
#' as follow:\cr
#' - rgee::ee_install_drivers()\cr
#' - rgee::ee_check_drivers()\cr
#'
#' The properties' arguments is just available for image uploaded. If you are
#' interesting to set properties in FeatureCollection, please use
#' \link{ee_manage_set_properties} and then \link{ee_upload}.
#'
#' The selenium_params argument is a three-element list which consists of:\cr
#'  - gmail_account: Google account. If it is not specified, it will
#'  be obtained from ee$data$getAssetRoots().\cr
#'  - showpassword: Logical. Once google account is filled into
#'  \link[getPass]{getPass}, should be shown?.
#'  - cache: Logical. TRUE will use cookies stored on the /temp directory.
#'
#' With respect to the variables time_start and time_end, both needs
#' to be specified in seconds (since the early 1970s). Assumed to
#' be in time zone - UTC.
#' @name ee_upload
#' @examples
#' library(rgee)
#' library(stars)
#' library(sf)
#'
#' username <- 'aybar1994' #change according to username.
#' gcs_bucket <- 'bag_csaybar'
#' ee_check_drivers()
#' ee_Initialize(user_gmail = username, gcs = TRUE)
#'
#' # Create a folder in Earth Engine Asset
#' filename <- sprintf("users/%s/rgee_upload/", username)
#' ee_manage_create(filename)
#'
#' # Select an image to upload
#' tif = system.file("tif/geomatrix.tif", package = "stars")
#' geomatrix = read_stars(tif) %>% st_warp(crs=st_crs(4326))
#'
#' # Uploading to earth egnine
#' ee_upload(x = geomatrix,
#'           filename = paste0(filename,"geomatrix"),
#'           bucket = gcs_bucket)
#'
#' # Read uploaded image
#' asset_geomatrix <- paste0(filename,"geomatrix")
#' ee_geomatrix <- ee$Image(asset_geomatrix)
#' ee_map(ee_geomatrix, zoom_start = 18)
#' ## OPTIONAL: add properties
#' ee_manage_set_properties(
#'   path_asset = asset_geomatrix,
#'   properties = list(message='hello-world',language = 'R'))
#'
#' # Clean EE asset and GCS
#' ee_manage_delete(dirname(asset_geomatrix))
#' googleCloudStorageR::gcs_global_bucket(gcs_bucket)
#' buckets <- googleCloudStorageR::gcs_list_objects()
#' mapply(googleCloudStorageR::gcs_delete_object, buckets$name)
#' @export
ee_upload <- function(x, ...) {
  UseMethod("ee_upload")
}

#' @name ee_upload
#' @export
ee_upload.character <- function(x, ... ,
                                filename,
                                bucket = NULL,
                                properties = NULL,
                                start_time = '1970-01-01',
                                end_time = '1970-01-01',
                                selenium_params = getOption(
                                  "rgee.selenium.params"
                                ),
                                clean = FALSE,
                                reinit = FALSE,
                                quiet = FALSE) {
  user_gmail <- getOption("rgee.selenium.params")$user_gmail
  if (is.null(user_gmail)) {
    stop('ee_upload needs that "user_gmail" ',
         'argument be specified in ee$Initialize().',
         "\nExample: ee_Initialize(user_gmail = 'XXXX@gmail.com')")
  }

  filename <- ee_verify_filename(path_asset = filename,
                                        strict = FALSE)
  gs_uri <- ee_upload_file_to_gcs(x = x,
                                  bucket = bucket,
                                  selenium_params = selenium_params,
                                  clean = clean,
                                  reinit = reinit)

  if (image_or_vector(x) == "sf") {
    ee_gcs_to_asset(x = x,
                    gs_uri = gs_uri,
                    filename = filename,
                    type = 'table' ,
                    properties=NULL)
  } else if (image_or_vector(x) == "stars") {
    ee_gcs_to_asset(x = read_stars(x),
                    gs_uri = gs_uri,
                    filename = filename,
                    type = 'image',
                    properties=properties,
                    start_time = '1970-01-01',
                    end_time = '1970-01-01')
  } else {
    stop(sprintf("%s needs to be either a GeoTIFF or ESRI SHAPEFILE file", x))
  }
  invisible(TRUE)
}

#' @name ee_upload
#' @export
ee_upload.sf <- function(x, ...,
                         filename,
                         bucket = NULL,
                         selenium_params = getOption("rgee.selenium.params"),
                         clean = FALSE,
                         reinit = FALSE,
                         quiet = FALSE) {
  ee_temp <- tempdir()
  filename <- ee_verify_filename(path_asset = filename,strict = FALSE)
  shp_dir <- sprintf("%s/%s.shp", ee_temp, basename(filename))
  write_sf(x,shp_dir)
  gs_uri <- ee_upload_file_to_gcs(x = shp_dir,
                                  bucket = bucket,
                                  selenium_params = selenium_params,
                                  clean = clean,
                                  reinit = reinit)
  ee_gcs_to_asset(x = x,
                  gs_uri = gs_uri,
                  filename = filename,
                  type = 'table',
                  properties=NULL)
  invisible(TRUE)
}

#' @name ee_upload
#' @export
ee_upload.stars <- function(x, ...,
                            filename,
                            bucket = NULL,
                            properties = NULL,
                            start_time = '1970-01-01',
                            end_time = '1970-01-01',
                            selenium_params = getOption("rgee.selenium.params"),
                            clean = FALSE,
                            reinit = FALSE,
                            quiet = FALSE) {
  ee_temp <- tempdir()
  filename <- ee_verify_filename(path_asset = filename,strict = FALSE)
  tif_dir <- sprintf("%s/%s.tif", ee_temp, basename(filename))
  write_stars(x, tif_dir)
  gs_uri <- ee_upload_file_to_gcs(x = tif_dir,
                                  bucket = bucket,
                                  selenium_params = selenium_params,
                                  clean = clean,
                                  reinit = reinit)
  ee_gcs_to_asset(x = x,
                  gs_uri = gs_uri,
                  filename = filename,
                  type = 'image',
                  properties = properties)
  invisible(TRUE)
}

#' @name ee_upload
#' @export
ee_upload.stars_proxy <- function(x, ...,
                                  filename,
                                  bucket = NULL,
                                  properties = NULL,
                                  start_time = '1970-01-01',
                                  end_time = '1970-01-01',
                                  selenium_params = getOption(
                                    "rgee.selenium.params"
                                  ),
                                  clean = FALSE,
                                  reinit = FALSE,
                                  quiet = FALSE) {
  filename <- ee_verify_filename(path_asset = filename,strict = FALSE)
  #x <- x[[1]]
  gs_uri <- ee_upload_file_to_gcs(x = x[[1]],
                                  bucket = bucket,
                                  selenium_params = selenium_params,
                                  reinit = reinit)
  ee_gcs_to_asset(x = x,
                  gs_uri = gs_uri,
                  filename = filename,
                  type = 'image',
                  properties=properties)
  invisible(TRUE)
}
