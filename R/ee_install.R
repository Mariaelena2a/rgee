#' Interface to install non-R rgee dependencies
#'
#' R functions to install the necessary third-party
#' python packages for rgee and removing Python
#' Selenium Chromedriver and credentials
#'
#' @param GoogleChromeVersion Google Chrome version of this system.
#' @param method Installation method. By default, "auto" automatically
#' finds a method that will work in the local environment. Change the
#' default to force a specific installation method. Note that the
#' "virtualenv" method is not available on Windows (as this isn't
#' supported by rgee). Note also that since this command runs
#' without privilege the "system" method is available only on Windows.
#' @param conda Path to conda executable (or "auto" to find conda
#' using the PATH and other conventional install locations).
#' @param ee_version earthengine-api version to install. Specify "default" to
#' install the latest version.
#' @param envname Name of Python environment to install.
#' @param extra_packages Additional Python packages to install along with rgee.
#' @param restart_session Restart R session after installing (note this will
#' only occur within RStudio).
#' @param conda_python_version the Python version installed in the created conda
#' environment. Python 3.6 is installed by default.
#' @param ... other arguments passed to [reticulate::conda_install()] or
#' [reticulate::virtualenv_install()].
#' @param quiet logical. Suppress info message
#' @importFrom reticulate source_python py_install
#' @importFrom rstudioapi restartSession hasFun
#' @details It is neccessary restart R to observe change when installing a
#' Python packages. rgee only is compatible with Python version 3.5 >=.
#' @name ee_install-tools
#' @examples
#' \dontrun{
#' library(rgee)
#' library(reticulate)
#'
#' # Recommended way to use rgee
#' ## 1. Create a virtualenv
#' # virtualenv_remove("rgee")
#' # virtualenv_create("rgee", python = "python3.7")
#' use_virtualenv("rgee")
#' # rstudioapi::restartSession() # Restart R
#'
#' ## 2. Check dependencies
#' # Full checking dependencies
#' ee_check()
#'
#' # Install rgee Python packages
#' ee_install_rgee_python_packages()
#'
#' # Install selenium drivers (see ee_upload)
#' ee_install_drivers()
#'
#' # Install GCS and DRIVE credentials (optional)
#' ee_Initialize(drive = TRUE, gcs = TRUE)
#' ee_check()
#' }
#' @export
ee_install_drivers <- function(GoogleChromeVersion) {
  if (missing(GoogleChromeVersion)) {
    stop(
      "The GoogleChromeVersion argument was not defined.",
      " Find the appropriate version of Google Chrome visiting:\n",
      "- chrome://settings/help \n",
      "- After that run: rgee::ee_install_drivers(77)"
    )
  }

  oauth_func_path <- system.file("python/ee_check_utils.py", package = "rgee")
  ee_check_utils <- ee_source_python(oauth_func_path)
  directory <- path.expand("~/.config/earthengine/")

  os_type <- switch(Sys.info()[["sysname"]],
    Windows = {
      "windows"
    },
    Linux = {
      "linux"
    },
    Darwin = {
      "macos"
    }
  )
  chromedriver_version <- ee_check_utils$download_chromedriver(
    directory = directory,
    operating_system = os_type,
    version = substr(as.character(GoogleChromeVersion), 1, 2)
  )
  cat(
    "Selenium ChromeDriver v",
    ee_py_to_r(chromedriver_version),
    "saved in",
    directory
  )
  return(invisible(TRUE))
}

#' @rdname ee_install-tools
#' @export
ee_install_rgee_python_packages <- function(method = c(
                                              "auto",
                                              "virtualenv",
                                              "conda"
                                            ),
                                            conda = "auto",
                                            ee_version = "0.1.210",
                                            envname = NULL,
                                            extra_packages = c(
                                              "selenium",
                                              "bs4",
                                              "pysmartDL",
                                              "requests_toolbelt",
                                              "oauth2client"
                                            ),
                                            restart_session = TRUE,
                                            conda_python_version = "3.6",
                                            quiet = FALSE,
                                            ...) {
  # verify 64-bit
  if (.Machine$sizeof.pointer != 8) {
    stop(
      "Unable to install rgee on this platform.",
      "Binary installation is only available for 64-bit platforms."
    )
  }

  # verify Python version
  ee_check_python(quiet = TRUE)

  method <- match.arg(method)
  extra_packages <- unique(extra_packages)
  if (ee_version == "latest") {
    ee_version <- "earthengine-api"
  } else {
    ee_version <- paste0("earthengine-api==", ee_version)
  }

  py_install(
    packages = c(ee_version, extra_packages),
    envname = envname,
    method = method,
    conda = conda,
    python_version = conda_python_version,
    pip = TRUE,
    ...
  )

  cat("\nInstallation complete.\n\n")

  if (restart_session && hasFun("restartSession")) {
    restartSession()
  }

  invisible(NULL)
}
