% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot-utils.R
\name{.find_chrome}
\alias{.find_chrome}
\title{Find Google Chrome or Chromium in the system}
\usage{
.find_chrome()
}
\value{
A character string.
}
\description{
On Windows, this function tries to find Chrome from the registry. On macOS,
it returns a hard-coded path of Chrome under \file{/Applications}. On Linux,
it searches for \command{chromium-browser} and \command{google-chrome} from
the system's \var{PATH} variable.
}
