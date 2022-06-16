## manage dependencies ----

#' reboot this library reloading from dev.
#'
#' @return nothing
#' @export
#'
#' @examples
#' ggrrr::unstable()
unstable = function(pkg = "ggrrr") {
  if ("ggrrr" %in% rownames(installed.packages())) devtools::unload("ggrrr")
  if (file.exists("~/Git/ggrrr")) {
    devtools::document("~/Git/ggrrr",quiet = TRUE)
    devtools::install_local("~/Git/ggrrr", force=TRUE,upgrade = FALSE)
  } else {
    devtools::install_github("terminological/ggrrr",upgrade = FALSE)
  }
  library(ggrrr)
}

#' check if a package is installed
#'
#' @param packageName the name of the package
#'
#' @return boolean value
#' @export
is_installed = function(packageName) {
  if (length(packageName)>1) stop("is_installed() can only check for one package at a time")
  t=requireNamespace(packageName, quietly=TRUE)
  return(t)
  #return(nzchar(find.package(package = packageName)))
}

#' Make sure cran packages are installed
#'
#' @param cran_deps a vector of package names
#'
#' @return nothing
#' @export
#'
#' @examples
#' cran("tidyverse")
cran = function(cran_deps) {
  for (package in cran_deps) {
    if (!is_installed(package)) {
      install.packages(package)
    }
  }
  invisible(NULL)
}

#' Make sure github packages are installed. Use a locally checked out version if available.
#'
#' @param name the name of the package
#' @param github something like "github-repo/project-name"
#' @param force will only update a loaded package if TRUE (defaults to FALSE)
#' @param subdir if the package is in a subdirectory of the github repo
#' @param ... passed to devtools::install_github
#'
#' @return nothing
#' @export
#'
#' @examples
#' non_cran("patchwork", "thomasp85/patchwork")
non_cran = function(name,github,force=FALSE,subdir="",...) {
  if (force | !name %in% c(devtools::dev_packages(),rownames(installed.packages()))) {
    local = paste0("~/Git/",name,"/",subdir)
    if (fs::dir_exists(local)) {
      devtools::load_all(local)
    } else {
      if(subdir != "") {
        devtools::install_github(github,subdir = subdir,...)
      } else {
        devtools::install_github(github,...)
      }
    }
  }
}

# TODO: unload and reload a package including installation using devtools::install_local or devtools::install_github
