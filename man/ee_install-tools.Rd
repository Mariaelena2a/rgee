% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ee_install.R
\name{ee_install-tools}
\alias{ee_install-tools}
\alias{ee_install_drivers}
\alias{ee_install_rgee_python_packages}
\title{Interface to install non-R rgee dependencies}
\usage{
ee_install_drivers(GoogleChromeVersion)

ee_install_rgee_python_packages(method = c("auto", "virtualenv",
  "conda"), conda = "auto", ee_version = "0.1.210", envname = NULL,
  extra_packages = c("selenium", "bs4", "pysmartDL", "requests_toolbelt",
  "oauth2client"), restart_session = TRUE,
  conda_python_version = "3.6", quiet = FALSE, ...)
}
\arguments{
\item{GoogleChromeVersion}{Google Chrome version of this system.}

\item{method}{Installation method. By default, "auto" automatically
finds a method that will work in the local environment. Change the
default to force a specific installation method. Note that the
"virtualenv" method is not available on Windows (as this isn't
supported by rgee). Note also that since this command runs
without privilege the "system" method is available only on Windows.}

\item{conda}{Path to conda executable (or "auto" to find conda
using the PATH and other conventional install locations).}

\item{ee_version}{earthengine-api version to install. Specify "default" to
install the latest version.}

\item{envname}{Name of Python environment to install.}

\item{extra_packages}{Additional Python packages to install along with rgee.}

\item{restart_session}{Restart R session after installing (note this will
only occur within RStudio).}

\item{conda_python_version}{the Python version installed in the created conda
environment. Python 3.6 is installed by default.}

\item{quiet}{logical. Suppress info message}

\item{...}{other arguments passed to \code{\link[reticulate:conda_install]{reticulate::conda_install()}} or
\code{\link[reticulate:virtualenv_install]{reticulate::virtualenv_install()}}.}
}
\description{
R functions to install the necessary third-party
python packages for rgee and removing Python
Selenium Chromedriver and credentials
}
\details{
It is neccessary restart R to observe change when installing a
Python packages. rgee only is compatible with Python version 3.5 >=.
}
\examples{
\dontrun{
library(rgee)
library(reticulate)

# Recommended way to use rgee
## 1. Create a virtualenv
# virtualenv_remove("rgee")
# virtualenv_create("rgee", python = "python3.7")
use_virtualenv("rgee")
# rstudioapi::restartSession() # Restart R

## 2. Check dependencies
# Full checking dependencies
ee_check()

# Install rgee Python packages
ee_install_rgee_python_packages()

# Install selenium drivers (see ee_upload)
ee_install_drivers()

# Install GCS and DRIVE credentials (optional)
ee_Initialize(drive = TRUE, gcs = TRUE)
ee_check()
}
}
