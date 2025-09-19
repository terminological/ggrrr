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
#' # ggrrr::unstable()
unstable = function(pkg = "ggrrr", org = "terminological") {
  lifecycle::deprecate_stop(
    "0.0.0.9024",
    "ggrrr::unstable()",
    "pkgtools::unstable"
  )
  if (pkg %in% rownames(utils::installed.packages())) {
    try(
      {
        devtools::unload(pkg)
      },
      silent = TRUE
    )
  }
  local = sprintf("~/Git/%s", pkg)
  if (file.exists(local)) {
    devtools::install_deps(local, upgrade = "never")
    devtools::document(pkg = local, quiet = TRUE)
    devtools::install_local(path = local, force = TRUE, upgrade = FALSE)
  } else {
    devtools::install_github(sprintf("%s/%s", org, pkg), upgrade = "never")
  }
}

#' Make sure packages available on CRAN are installed
#'
#' @param cran_deps a vector of package names
#'
#' @return nothing
#' @export
#'
#' @examples
#' # cran("tidyverse")
cran = function(cran_deps) {
  lifecycle::deprecate_warn("0.0.0.9024", "ggrrr::cran()")
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
#' # non_cran("patchwork", "thomasp85/patchwork")
non_cran = function(name, github, force = FALSE, subdir = "", ...) {
  lifecycle::deprecate_stop("0.0.0.9024", "ggrrr::non_cran()")
  if (
    force |
      !name %in%
        c(devtools::dev_packages(), rownames(utils::installed.packages()))
  ) {
    local = paste0("~/Git/", name, "/", subdir)
    if (fs::dir_exists(local)) {
      devtools::load_all(local)
    } else {
      if (subdir != "") {
        devtools::install_github(github, subdir = subdir, ...)
      } else {
        devtools::install_github(github, ...)
      }
    }
  }
}

#' @inherit .optional_fn
#' @export
optional_fn = .optional_fn
