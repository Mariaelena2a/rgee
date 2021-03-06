% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ee_map.R
\name{ee_map}
\alias{ee_map}
\alias{ee_map.default}
\alias{ee_map.ee.geometry.Geometry}
\alias{ee_map.ee.feature.Feature}
\alias{ee_map.ee.featurecollection.FeatureCollection}
\alias{ee_map.ee.image.Image}
\alias{ee_map.ee.imagecollection.ImageCollection}
\title{Adds a given Earth Engine (EE) spatial object to mapview as a layer.}
\usage{
ee_map(eeobject, ...)

\method{ee_map}{default}(eeobject, ...)

\method{ee_map}{ee.geometry.Geometry}(eeobject, vizparams, center,
  zoom_start = 8, objname = "map", quiet = FALSE, ...)

\method{ee_map}{ee.feature.Feature}(eeobject, vizparams, center,
  zoom_start = 8, objname = "map", quiet = FALSE, ...)

\method{ee_map}{ee.featurecollection.FeatureCollection}(eeobject,
  vizparams, center, zoom_start = 8, objname = "map", quiet = FALSE,
  ...)

\method{ee_map}{ee.image.Image}(eeobject, vizparams, center,
  zoom_start = 8, objname = "map", quiet = FALSE, ...)

\method{ee_map}{ee.imagecollection.ImageCollection}(eeobject, vizparams,
  center, zoom_start = 8, objname = "map", max_nimage = 10,
  quiet = FALSE, ...)
}
\arguments{
\item{eeobject}{An EE spatial object.}

\item{...}{Ignored.}

\item{vizparams}{A list that contains the visualization parameters. See
details.}

\item{center}{The longitude and latitude of the map center. If it is not
defined, ee_map will try to estimate the centroid of the spatial EE object.}

\item{zoom_start}{zoom level.}

\item{objname}{character vector. Name of the map, or maps in case that the
EE object be an ImageCollection.}

\item{quiet}{logical; suppress info messages.}

\item{max_nimage}{Max number of Image to display.}
}
\description{
Create interactive visualizations of spatial EE objects
(Geometry, Image, Feature, FeatureCollection or ImageCollection)
through \code{\link[mapview]{mapview}}.
}
\details{
\code{ee_map} takes advantage of the ee$Image()$getMapId python function for
fetch and return a mapid and token that is suitable for use in a
\code{\link[mapview]{mapview}}. To achieve desirable visualization
effects, it will depend on the type of spatial EE object . For neither
Image or ImageCollection, you can provide visualization parameters to
ee_map by the parameter vizparams. The
\href{https://developers.google.com/earth-engine/image_visualization}{
parameters} available are:

\tabular{lll}{
\strong{Parameter}\tab \strong{Description}  \tab \strong{Type}\cr
\strong{bands}    \tab  Comma-delimited list of three band names to be
mapped to RGB     \tab  list \cr
\strong{min}      \tab  Value(s) to map to 0 \tab  number or list of three
numbers, one for each band \cr
\strong{max}      \tab  Value(s) to map to 1 \tab  number or list of three
numbers, one for each band \cr
\strong{gain}     \tab  Value(s) by which to multiply each pixel value \tab
number or list of three numbers, one for each band \cr
\strong{bias}     \tab  Value(s) to add to each Digital Number (DN)
value \tab number or list of three numbers, one for each band \cr
\strong{gamma}    \tab  Gamma correction factor(s) \tab  number or list of
three numbers, one for each band \cr
\strong{palette}  \tab  List of CSS-style color strings
(single-band images only) \tab  comma-separated list of hex strings \cr
\strong{opacity}   \tab  The opacity of the layer (0.0 is fully transparent
and 1.0 is fully opaque) \tab  number \cr
}

If you add an Image or ImageCollection to the map without any additional
parameters, by default \code{ee_map} assigns the first three bands to red,
green and blue, respectively. The default stretch is based on the min-max
range.  For Geometry, Feature or FeatureCollection. The available
vizparams are:
\itemize{
\item \strong{color}: A hex string in the format RRGGBB specifying the
color to use for drawing the features. By default 000000.
\item \strong{pointRadius}: The radius of the point markers. By default 3.
\item \strong{strokeWidth}: The width of lines and polygon borders. By
default 3.
}
}
\examples{

library(rgee)
ee_reattach() # reattach ee as a reserve word
ee_Initialize()

# Case: Geometry*
geom <- ee$Geometry$Point(list(-73.53522, -15.75453))
m1 <- ee_map(
  eeobject = geom,
  vizparams = list(pointRadius = 10, color = "FF0000"),
  objname = "Geometry-Arequipa"
)
m1

# Case: Feature
eeobject_fc <- ee$FeatureCollection("users/csaybar/DLdemos/train_set")$
  first()
m2 <- ee_map(eeobject = ee$Feature(eeobject_fc),
             objname = "Feature-Arequipa")
m2 + m1

# Case: FeatureCollection
eeobject_fc <- ee$FeatureCollection("users/csaybar/DLdemos/train_set")
m3 <- ee_map(eeobject = eeobject_fc, objname = "FeatureCollection")
m3 + m2 + m1

# Case: Image
image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
m4 <- ee_map(
  eeobject = image,
  vizparams = list(
    bands = c("B4", "B3", "B2"),
    max = 10000
  ),
  objname = "SF",
  zoom_start = "8"
)
m4

# Case: ImageCollection
collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filter(ee$Filter()$eq("WRS_PATH", 44))$
  filter(ee$Filter()$eq("WRS_ROW", 34))$
  filterDate("2014-01-01", "2015-01-01")$
  sort("CLOUD_COVER")

m5 <- ee_map(
  eeobject = collection,
  vizparams = list(bands = c("B4", "B3", "B2"), max = 1),
  objname = c("Scene_2019", "Scene_2016", "Scene_2011"),
  max_nimage = 3,
  zoom_start = 10
)
m5
}
