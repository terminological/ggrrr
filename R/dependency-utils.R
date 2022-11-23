## manage dependencies ----

#' Get unstable version of package
#'
#' reboot a library reloading from a local development copy if it exists locally alternatively get
#' the most up to date github package.
#'
#' @param pkg the package to load the unstable version of
#' @param org the github organisation
#' @return nothing
#' @export
#'
#' @examples
#' \donttest{
#' library(tidyverse)
#' ggrrr::unstable()
#' }
unstable = function(pkg = "ggrrr", org="terminological") {
  if (pkg %in% rownames(utils::installed.packages())) try({devtools::unload(pkg)},silent = TRUE)
  local = sprintf("~/Git/%s",pkg)
  if (file.exists(local)) {
    devtools::install_deps(local, upgrade = "never")
    devtools::document(pkg = local, quiet = TRUE)
    devtools::install_local(path = local, force=TRUE,upgrade = FALSE)
  } else {
    devtools::install_github(sprintf("%s/%s",org, pkg),upgrade = "never")
  }
}

#' check if a package is installed
#'
#' @param packageName the name of the package
#'
#' @return boolean value
#' @export
is_installed = function(packageName) {
  return(rlang::is_installed(packagename))
  # if (length(packageName)>1) stop("is_installed() can only check for one package at a time")
  # t=requireNamespace(packageName, quietly=TRUE)
  # return(t)
  #return(nzchar(find.package(package = packageName)))
}

#' Make sure packages available on CRAN are installed
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
    if (!rlang::is_installed(package)) {
      utils::install.packages(package)
    }
  }
  invisible(NULL)
}

#' Make sure github packages are installed.
#'
#' Use a locally checked out version if available.
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
  if (force | !name %in% c(devtools::dev_packages(),rownames(utils::installed.packages()))) {
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



#' Get an optional function without triggering a CRAN warning
#'
#' You want to use a function if it is installed but don't want it to
#' be installed as part of your package and you don't want to reference it
#' as part of the Imports or Suggests fields in a package DESCRIPTION.
#'
#' @param pkg the package name
#' @param name the function you wish to use
#' @param alt a function that can be used instead
#'
#' @return the function you want if available or the alternative
#' @export
#'
#' @examples
#' fn = optional_fn("openssl", "md5", digest::digest)
#' as.character(fn(as.raw(c(1,2,3))))
optional_fn = function(pkg, name, alt=function(...) {message("Skipping function call as ",pkg,"::",name," not available")}) {
  if (!rlang::is_installed(pkg)) return(alt)
  return(getFromNamespace(name, pkg))
}

# TODO: unload and reload a package including installation using devtools::install_local or devtools::install_github
