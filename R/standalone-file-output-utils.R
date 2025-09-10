# ---
# repo: terminological/ggrrr
# file: standalone-file-output-utils.R
# last-updated: '2024-06-12'
# license: https://unlicense.org
# imports:
# - base64enc
# - flextable
# - fs
# - ggplot2
# - grDevices
# - htmltools
# - huxtable
# - knitr
# - magrittr
# - officer
# - pdftools
# - ragg
# - readr
# - rlang
# - rmarkdown
# - rstudioapi
# - rsvg
# - stringr
# - svglite
# - systemfonts
# - utils
# ---
# This set of functions are implicated in saving files

# Standard sizes ----

#' Standard image and paper sizes
#'
#' The width and height of images to fit scientific publication standards.
#'
#' @docType data
#' @name std_size
#' @format A list with width and height in inches
#' @export
#' @concept output
std_size = list(
  A4 = list(width = 8.25, height = 11.75, rot = 0),
  A5 = list(width = 5 + 7 / 8, height = 8.25, rot = 0),
  full = list(width = 5.9, height = 8, rot = 0),
  landscape = list(width = 9.75, height = 5.9, rot = 0),
  half = list(width = 5.9, height = 4, rot = 0),
  third = list(width = 5.9, height = 3, rot = 0),
  two_third = list(width = 5.9, height = 6, rot = 0),
  quarter = list(width = 5.9, height = 2, rot = 0),
  quarter_portrait = list(width = 3, height = 4, rot = 0),
  sixth = list(width = 3, height = 3, rot = 0),
  slide = list(width = 12, height = 6, rot = 0)
)

# Project output directory tools ----

#' Generate a versioned file name in a subdirectory.
#'
#' This function generates a function that resolves a file path fragment to a
#' specific file location, accounting for a versioning strategy involving
#' the current date. The defaults create a naming strategy that places an
#' file in the "output" sub-directory of the current project with a filename suffix including
#' the date.
#'
#' @param directory the root of the output
#' @param ... not used must be empty
#' @param datedFile do you want the filename to have the date appended?
#' @param datedSubdirectory do you want the files to be placed in a dated subdirectory?
#'
#' @return a function that takes a filename and boolean delete parameter.
#' When called with a filename component this function will return the absolute path of a file which is
#' versioned with date. If the file exists and delete=TRUE it is deleted.
#' (allowing for libraries that refuse to overwrite existing files)
#' @keywords internal
#' @concept output
.outputter = function(
  directory = .here("output"),
  ...,
  datedFile = !datedSubdirectory,
  datedSubdirectory = FALSE
) {
  rlang::check_dots_empty()
  directory = fs::path_norm(directory)
  fs::dir_create(directory)
  message("directing output to: ", directory)
  return(function(filename = "", delete = FALSE) {
    if (datedSubdirectory) {
      directory = fs::path(directory, Sys.Date())
      fs::dir_create(directory)
    }

    if (filename == "**BROWSE**") {
      utils::browseURL(directory)
      return(invisible(NULL))
    }

    ext = fs::path_ext(filename)
    if (datedFile) {
      filename = paste0(fs::path_ext_remove(filename), "-", Sys.Date()) %>%
        fs::path_ext_set(ext)
    }
    path = fs::path(directory, filename)
    if (delete & fs::file_exists(path)) {
      unlink(path)
    }

    return(path)
  })
}

#' Get the file path of the current script
#'
#' Gives you the file input path regardless of whether you are running the
#' script in rstudio, knitr or on the console.
#'
#' @return the file path of the currently executed script or an error if the
#' command is executed outside of a script.
#' @keywords internal
#' @concept output
#' @examples
#' try({
#' .this_script()
#' })
.this_script = function() {
  if (.is_knitting()) {
    return(knitr::current_input(dir = TRUE))
  }
  if (.is_running_in_chunk()) {
    return(fs::path_abs(rstudioapi::getSourceEditorContext()$pat))
  }
  # in a .R script in R studio
  tmp = try(rstudioapi::getActiveDocumentContext()$path)
  if (tmp != "") {
    return(fs::path_abs(tmp))
  }
  # command line with a file parameter
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript
    return(fs::path_abs(sub(needle, "", cmdArgs[match])))
  } else {
    # 'source'd via R console
    tryCatch(
      {
        if (sys.frames()[[1]]$ofile != "") {
          return(fs::path_abs(sys.frames()[[1]]$ofile))
        }
      },
      error = function(e) return(fs::path_abs(getwd()))
    )
  }
  warning("not running in a script.", call. = FALSE)
  return(fs::path_abs(getwd()))
}

#' Find the root of the current project
#'
#' gets a reverse directory listing and then finds the lowest directory
#' that contains an indication of there being a project in the directory
#' this should work where the `here` package does not
#' (particularly when knitting in CRAN checks for example, or running in a
#' non package project)
#'
#' @param inputFile a file to check the project root of.
#'
#' @return the file path of the root of the project
#' @keywords internal
#' @concept output
#'
#' @examples
#' try({
#' .locate_project()
#' })
.locate_project = function(inputFile = .this_script()) {
  . = NULL
  absPath = inputFile %>% fs::path_expand()
  parent = unique(ifelse(fs::is_dir(absPath), absPath, fs::path_dir(absPath)))
  current = parent
  # add parent directories up to /user/home
  repeat {
    grandparent = unique(fs::path_dir(current))
    grandparent = grandparent[!grandparent %in% parent]
    if (length(grandparent) == 0) {
      break
    }
    parent = c(parent, grandparent)
    current = grandparent[grandparent != fs::path_home()]
  }
  root = fs::path_dir(fs::dir_ls(parent, glob = "*.Rproj", type = "file"))
  if (length(root) == 1) {
    return(root)
  }
  root = fs::path_dir(fs::dir_ls(parent, glob = "*/DESCRIPTION", type = "file"))
  if (length(root) == 1) {
    return(root)
  }
  root = fs::path_dir(fs::dir_ls(parent, glob = "*/NAMESPACE", type = "file"))
  if (length(root) == 1) {
    return(root)
  }
  root = fs::path_dir(fs::dir_ls(parent, glob = "*/R", type = "directory"))
  if (length(root) == 1) {
    return(root)
  }
  root = fs::path_dir(fs::dir_ls(
    parent,
    glob = "*/vignettes",
    type = "directory"
  ))
  if (length(root) == 1) {
    return(root)
  }
  root = fs::path_dir(fs::dir_ls(
    parent,
    glob = "*/.git",
    all = TRUE,
    type = "directory"
  ))
  if (length(root) == 1) {
    return(root)
  }
  root = fs::path_dir(fs::dir_ls(
    parent,
    glob = "*/.Rproj.user",
    all = TRUE,
    type = "directory"
  ))
  if (length(root) == 1) {
    return(root)
  }
  # return the longest possible path with a .Rhistory file
  root = fs::path_dir(fs::dir_ls(
    parent,
    glob = "*/.Rhistory$",
    all = TRUE,
    type = "file"
  ))
  return(
    root %>%
      magrittr::extract(stringr::str_length(.) == max(stringr::str_length(.)))
  )
}

#' Drop in replacement for `here` (`here` pkg)
#'
#' @param ... the relative path within the project
#' @param projRoot the project root - defaults to `.locate_project()`
#'
#' @return a path
#' @keywords internal
#' @concept output
#'
#' @examples
#' try(.here("vignettes"))
.here = function(..., projRoot = .locate_project()) {
  return(fs::path(projRoot, ...))
}

# PDF conversion using chrome ----

#' Find Google Chrome or Chromium in the system
#'
#' On Windows, this function tries to find Chrome from the registry. On macOS,
#' it returns a hard-coded path of Chrome under \file{/Applications}. On Linux,
#' it searches for \command{chromium-browser} and \command{google-chrome} from
#' the system's \var{PATH} variable.
#'
#' This function is borrowed from `pagedown` here to avoid the dependency:
#' https://github.com/rstudio/pagedown/blob/main/R/chrome.R which is MIT
#' licensed. We note that there is no check made on MacOS that the path is
#' correct, and since it may be installed elsewhere this is a potential point
#' of failure. Must assume that calls to chrome may fail.
#'
#' @return A character string with the path of chrome or an error.
#' @keywords internal
#' @concept output
#' @examples
#' try(.find_chrome())
#'
.find_chrome = function() {
  switch(
    .Platform$OS.type,
    windows = {
      res = tryCatch(
        {
          # unlist(utils::readRegistry('MSEdgeHTM\shell\open\command', 'HCR'))
          unlist(utils::readRegistry('ChromeHTML\\shell\\open\\command', 'HCR'))
        },
        error = function(e) ''
      )
      res = unlist(strsplit(res, '"'))
      res = utils::head(res[file.exists(res)], 1)
      if (length(res) != 1) {
        stop(
          'Cannot find Google Chrome automatically from the Windows Registry Hive. ',
          "Please pass the full path of chrome.exe to the 'browser' argument ",
          "or to the environment variable 'PAGEDOWN_CHROME'."
        )
      }
      res
    },
    unix = if (unname(Sys.info()["sysname"] == "Darwin")) {
      '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'
      #TODO check this exists
    } else {
      for (i in c(
        'google-chrome',
        'chromium-browser',
        'chromium',
        'google-chrome-stable'
      )) {
        if ((res <- Sys.which(i)) != '') break
      }
      if (res == '') {
        stop('Cannot find Chromium or Google Chrome')
      }
      res
    },
    stop('Your platform is not supported')
  )
}

# tmpbad = tempfile(fileext = ".svg")
# tmpout = tempfile(fileext = ".pdf")
# readr::write_file("sdkljfsdlkfjsdl", tmpbad)
# .print_svg_with_chrome(tmpbad, tmpout)
#' Print an SVG file to pdf using chrome
#'
#' Create a the same size as the SVG file using chrome or chromium
#'
#' @param svgFile the input svg file
#' @param pdfFile the output pdf file
#' @param chromeBinary the location of chrome or chromium binary
#' @param maxWidth the maximum width of the output in inches
#' @param maxHeight the maximum height of the output in inches
#'
#' @return the pdf file path
#' @keywords internal
#' @concept output
#' @examples
#' try({
#'  plot = ggplot2::ggplot(ggplot2::diamonds, ggplot2::aes(x=carat,y=price,color = color))+
#'    ggplot2::geom_point()+
#'    ggplot2::annotate("label",x=2,y=10000,label="Hello \u2014 world", family="Kings")+
#'    ggplot2::labs(tag = "A")+
#'    ggplot2::xlab("Carat\u2082")+
#'    ggplot2::ylab("price\u2265")
#'
#'  res = plot %>% .gg_save_as(filename=tempfile(), formats=c("svg"))
#'  tmp = .print_svg_with_chrome(res$svg)
#'
#'  # browseURL(tmp)
#'  # The resulting pdf has fonts embedded.
#' })
.print_svg_with_chrome = function(
  svgFile,
  pdfFile = tempfile(fileext = ".pdf"),
  chromeBinary = getOption("ggrrr.chrome", default = .find_chrome()),
  maxWidth = NULL,
  maxHeight = NULL
) {
  if (!fs::file_exists(svgFile)) {
    stop("SVG file does not exist: ", svgFile)
  }
  tmp_html = tempfile(fileext = ".html")
  html = sprintf(
    "
<html>
  <head>
    <style>
body {
  margin: 0;
}
    </style>
    <script>
function init() {
  const element = document.getElementById('targetsvg');
  const positionInfo = element.getBoundingClientRect();
  const width = Math.min(positionInfo.width,%d)+1;
  const height = Math.min(positionInfo.height,%d)+1;
  const style = document.createElement('style');
  style.innerHTML = `@page {margin: 0; size: ${width}px ${height}px}`;
  document.head.appendChild(style);
}
window.onload = init;
    </script>
  </head>
  <body>
    <img id='targetsvg' src='%s'>
  </body>
</html>
",
    if (is.null(maxWidth)) 1000000 else maxWidth * 72,
    if (is.null(maxHeight)) 1000000 else maxHeight * 72,
    svgFile
  )
  readr::write_file(html, tmp_html)
  out = system2(
    chromeBinary,
    # args=c("--headless", "--disable-gpu", "--no-pdf-header-footer", sprintf("--print-to-pdf=%s",pdfFile), tmp_html),
    args = c(
      "--headless",
      "--no-pdf-header-footer",
      sprintf("--print-to-pdf=%s", pdfFile),
      tmp_html
    ),
    stdout = NULL,
    stderr = NULL
  )
  if (out == 127) {
    stop("Could not execute chrome headless, using: ", chromeBinary)
  } else if (out != 0) {
    stop("Unable to process SVG file: ", svgFile)
  }
  invisible(pdfFile)
}

#' Print a HTML fragment using chrome
#'
#' @param htmlFragment an HTML fragment
#' @param pdfFile the pdf file
#' @param css a list of valid css style specifications (not urls to stylesheets)
#' @param chromeBinary the chrome browser (or chromium) path
#' @param maxWidth the maximum width of the output in inches
#' @param maxHeight the maximum height of the output in inches
#'
#' @return the pdf path
#' @keywords internal
#' @concept output
#'
#' @examples
#' try({
#'  hux = iris %>% huxtable::as_hux() %>% huxtable::theme_mondrian(font="Roboto")
#'  html = hux %>% huxtable::to_html()
#'  tmp = .print_html_with_chrome(html,maxWidth = 5,maxHeight = std_size$A4$height)
#'
#'  # browseURL(tmp)
#'  # The resulting pdf has fonts embedded & is multipage.
#' })
.print_html_with_chrome = function(
  htmlFragment,
  pdfFile = tempfile(fileext = ".pdf"),
  css = list(),
  chromeBinary = getOption("ggrrr.chrome", default = .find_chrome()),
  maxWidth = NULL,
  maxHeight = NULL
) {
  tmp_html = tempfile(fileext = ".html")
  style_dec = ""
  if (length(css) > 0) {
    style_dec = sprintf("<style>%s</style>", paste0(css, collapse = ""))
  }
  html = sprintf(
    "
<html>
  <head>
    %s
    <style>
body {
  margin: 0;
}
    </style>
    <script>
function init() {
  const element = document.getElementById('body-content');
  const positionInfo = element.getBoundingClientRect();
  const width = Math.min(positionInfo.width,%d)+1;
  const height = Math.min(positionInfo.height,%d)+1;
  const style = document.createElement('style');
  style.innerHTML = `@page {margin: 0; size: ${width}px ${height}px}`;
  element.style.width = `${width}px`;
  document.head.appendChild(style);
}
window.onload = init;
    </script>
  </head>
  <body><div id='body-content'>%s</div></body>
</html>
",
    style_dec,
    as.integer(if (is.null(maxWidth)) 1000000 else maxWidth * 72),
    as.integer(if (is.null(maxHeight)) 1000000 else maxHeight * 72),
    htmlFragment
  )
  readr::write_file(html, tmp_html)
  out = system2(
    chromeBinary,
    # args=c("--headless", "--disable-gpu", "--no-pdf-header-footer", sprintf("--print-to-pdf=%s",pdfFile),tmp_html),
    args = c(
      "--headless",
      "--no-pdf-header-footer",
      sprintf("--print-to-pdf=%s", pdfFile),
      tmp_html
    ),
    stdout = NULL,
    stderr = NULL
  )
  if (out == 127) {
    stop("Could not execute chrome headless, using: ", chromeBinary)
  } else if (out != 0) {
    stop("Unable to process html fragment")
  }
  invisible(pdfFile)
}


#' Convert a single pdf to multiple pngs one per page
#'
#' @param pdfFile an input pdf file
#'
#' @return a list of png files one per page.
#' @keywords internal
#' @concept output
#'
#' @examples
#' try({
#'  hux = iris %>% huxtable::as_hux() %>% huxtable::theme_mondrian(font="Arial")
#'  html = hux %>% huxtable::to_html()
#'  tmp = .print_html_with_chrome(html,maxWidth = std_size$A4$width,maxHeight = std_size$A4$height)
#'  pngs = .convert_pdf_to_pngs(tmp)
#' })
.convert_pdf_to_pngs = function(pdfFile) {
  pages = pdftools::pdf_length(pdfFile)
  out = pdfFile %>%
    fs::path_ext_remove() %>%
    paste0(sprintf("_%03d", 1:pages)) %>%
    fs::path_ext_set("png")
  pdftools::pdf_convert(pdfFile, filenames = out, dpi = 300, verbose = FALSE)
  return(out)
}

# Flags for identifying script execution context ----

#' @inherit knitr::is_html_output
#' @keywords internal
#' @concept output
.is_html_output = knitr::is_html_output

#' @inherit knitr::is_latex_output
#' @keywords internal
#' @concept output
.is_latex_output = knitr::is_latex_output


#' Check knitr output is a word type document
#'
#' @return true if the knitr output target is doc or odt
#' @keywords internal
#' @concept output
.is_document_output = function() {
  !.is_html_output() &
    !.is_latex_output() &
    .is_output(c("odt_document", "word_document"))
}

#' Check knitr output is not a known output forma
#'
#' @return true or false
#' @keywords internal
#' @concept output
.is_unknown_output = function() {
  !.is_html_output() &
    !.is_latex_output() &
    !.is_document_output()
}

#' Check knitr output is of a specified output type
#'
#' @keywords internal
#' @return true or false
#' @param outputFormats the name of a rmarkdown output type. This is the
#' value given in the `output:` part of the yaml metadata.
#' @keywords internal
#' @concept output
.is_output = function(outputFormats) {
  if (is.null(knitr::current_input())) {
    stop("not running in a rmarkdown document")
  }
  fmt = rmarkdown::default_output_format(knitr::current_input())$name
  return(fmt %in% outputFormats)
}

#' Check if the current code is being executed during document knitting
#'
#' @keywords internal
#' @concept output
#' @return
#' TRUE if the whole document is being knitted.
#' FALSE if running in chunk in RStudio, or otherwise outside of a knitr
.is_knitting = function() {
  isTRUE(getOption("knitr.in.progress"))
}

#' Check if the current code is being executed in a document chunk in
#' an RStudio session or when being knitted.
#'
#' @keywords internal
#' @concept output
#' @return
#' TRUE is being knitted OR running in chunk in RStudio
#' FALSE if not interactive or interactive but in console in RStudio
.is_running_in_chunk = function() {
  .is_knitting() |
    isTRUE(try(
      rstudioapi::getActiveDocumentContext()$id != "#console" &
        rstudioapi::getActiveDocumentContext()$path %>% stringr::str_ends("Rmd")
    ))
}

#' Check if the current code is being executed in rstudio console or from
#' a console outside rstudio
#'
#' @keywords internal
#' @concept output
#' @return
#' TRUE is being run in console
#' FALSE if being knitted, or being run
.is_running_in_console = function() {
  isTRUE(try(
    # If this is run from a shell console this will throw an error.
    # and hence the function will return false
    rstudioapi::getActiveDocumentContext()$id == "#console" |
      !rstudioapi::getActiveDocumentContext()$path %>% stringr::str_ends("Rmd")
  ))
}

#' Check if a viewer is available
#'
#' @keywords internal
#' @concept output
#' @return true or false
.is_viewer_available = function() {
  viewer <- getOption("viewer")
  !is.null(viewer)
}


# Ggplot save as ----

#' Save a plot to multiple formats
#'
#' Saves a ggplot object to disk at a set physical size. Allows specific maximum dimensions
#' with an optional target aspect ratio to fit into specific configurations for publication.
#' e.g. a half page plot or a third of a 2 column page. Allows output in pdf for journal
#' publication or png for inclusion in documents, and makes sure that the outputs are
#' near identical.
#'
#' For maximum cross platform reproducibility we are using the combination of
#' `systemfonts` for font management, `svglite` to render the canonical output
#' `rsvg` to convert that to pdf, and `ragg` for bitmap formats.
#' In some situations `rsvg` fails in which case we fall back to rendering in a
#' headless chrome instance. This rather complicated pipeline ensures modern
#' webfont support, and editable SVG or PDF.
#'
#' @param filename base of target filename (excluding extension).
#' @param plot a ggplot
#' @param size a standard size see `std_size`
#' @param maxWidth maximum width in inches
#' @param maxHeight maximum height in inches
#' @param aspectRatio defaults to maxWidth/maxHeight
#' @param formats some of svg, png, pdf, Rdata, eps, ...
#' @param webfontFinder a function that takes a plot and returns a properly
#'   formatted css specification for webfonts in the plot. This is for internal
#'   use and does not need to be changed.
#' @inheritDotParams svglite::svglite -height -width -bg -web_fonts -system_fonts -user_fonts
#'
#' @keywords plot
#' @return the output is an sensible default object that can be displayed given
#'   the context it is called in, for example if knitting an RMarkdown document
#'   a link to the png file for embedding, if latex a link to the pdf file.
#' @keywords internal
#' @concept output
#' @examples
#'
#' try({
#' .gg_pedantic(fontSize = 6)
#' p = ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt, colour=as.factor(cyl))) +
#'   ggplot2::geom_point()
#' # p %>% .gg_save_as(filename="~/tmp/plot_example",maxWidth=4,maxHeight=4)
#' p %>% .gg_save_as(filename=tempfile(),maxWidth=2,maxHeight=1.5)
#'
#' plot = ggplot2::ggplot(ggplot2::diamonds, ggplot2::aes(x=carat,y=price,color = color))+
#'   ggplot2::geom_point()+
#'   ggplot2::annotate("label",x=2,y=10000,label="Hello \u2014 world", family="Kings")+
#'   ggplot2::labs(tag = "A")+
#'   ggplot2::xlab("Carat\u2082")+
#'   ggplot2::ylab("price\u2265")
#'
#' # plot %>% .gg_save_as(filename="~/tmp/plot_example_2")
#' res = plot %>% .gg_save_as(filename=tempfile(), formats=c("png","eps"))
#' as.character(res)
#' res
#' })
.gg_save_as = function(
  plot,
  filename = tempfile(),
  size = std_size$half,
  maxWidth = size$width,
  maxHeight = size$height,
  aspectRatio = maxWidth / maxHeight,
  formats = getOption("ggrrr.formats", default = c("svg", "png", "pdf")),
  webfontFinder = ~ return(list()),
  ...
) {
  webfontFinder = rlang::as_function(webfontFinder)
  web_fonts = webfontFinder(plot)

  if ("formatted.table" %in% class(plot)) {
    # override width specifically for formatted tables
    maxWidth = attr(plot, "target.width")
  }

  # plot comes with an aspect ratio which is expressed as height/width
  # this is generally true if the coords_fixed (?coords_sf) has been used.
  # this works but doesn;t adjust if there is a legend.
  # plotAr = tryCatch({plot$coordinates$ratio}, error = function(e) NULL)
  # if(!is.null(plotAr)) {
  #   aspectRatio = 1/plotAr
  #   if (maxWidth/aspectRatio > maxHeight) maxWidth = maxHeight*aspectRatio
  #   if (maxHeight*aspectRatio > maxWidth) maxHeight = maxWidth/aspectRatio
  # }
  # just better to let ggplot lay it out

  filename = fs::path_expand_r(filename)
  dir = fs::path_dir(filename)
  if (!fs::is_absolute_path(dir)) {
    dir = fs::path_abs(dir, getwd())
    filename = fs::path_abs(filename, getwd())
  }
  if (
    fs::path_ext(filename) %in% c("svg", "png", "pdf", "jpg", "tiff", "Rdata")
  ) {
    formats = fs::path_ext(filename)
  }
  fs::dir_create(dir)
  filename = fs::path_ext_remove(filename)
  withExt = function(extn) {
    fs::path_ext_set(filename, extn)
  }

  if ("Rdata" %in% formats) {
    saveRDS(plot, withExt("Rdata"))
    out$rds = withExt("Rdata")
  }

  out = list()
  out$plot = plot
  out$width = min(maxWidth, maxHeight * aspectRatio)
  out$height = min(maxHeight, maxWidth / aspectRatio)

  if ("svg" %in% formats) {
    svg_loc = withExt("svg")
    out$svg = withExt("svg")
  } else {
    svg_loc = tempfile(fileext = ".svg")
  }

  ggplot2::ggsave(
    svg_loc,
    plot,
    width = out$width,
    height = out$height,
    bg = "transparent",
    device = svglite::svglite,
    web_fonts = web_fonts,
    ...
  )

  if ("pdf" %in% formats) {
    pdf_loc = withExt("pdf")
    out$pdf = withExt("pdf")

    tryCatch(
      {
        rsvg::rsvg_pdf(svg_loc, pdf_loc)
      },
      error = function(e) {
        .print_svg_with_chrome(svg_loc, pdf_loc)
      }
    )
  }

  if ("eps" %in% formats) {
    rsvg::rsvg_eps(svg_loc, withExt("eps")) #, css = web_fonts)

    #   cairops = optional_fn("Cairo :: CairoPS",alt = stop("Please install package `Cairo` for ps output"))
    #   ggplot2::ggsave(
    #     withExt("ps"),
    #     plot,
    #     width = out$width,
    #     height = out$height,
    #     bg = "transparent", device = cairops, dpi = 300, ...);

    if (fs::file_exists(withExt("eps"))) out$ps = withExt("eps")
  }

  if ("png" %in% formats) {
    ggplot2::ggsave(
      withExt("png"),
      plot,
      width = out$width,
      height = out$height,
      bg = "transparent",
      device = ragg::agg_png,
      dpi = 300,
      ...
    )
    # suppressWarnings(pdftools::pdf_convert(pdf_loc, dpi = 300,filenames = withExt("png"), format="png", page=1, verbose = FALSE))
    out$png = withExt("png")
  }

  if ("jpg" %in% formats) {
    ggplot2::ggsave(
      withExt("jpg"),
      plot,
      width = out$width,
      height = out$height,
      bg = "transparent",
      device = ragg::agg_jpeg,
      dpi = 300,
      ...
    )
    # suppressWarnings(pdftools::pdf_convert(pdf_loc, dpi = 300,filenames = withExt("jpg"), format="jpg", page=1, verbose = FALSE))
    out$jpg = withExt("jpg")
  }

  if ("tiff" %in% formats) {
    ggplot2::ggsave(
      withExt("tiff"),
      plot,
      width = out$width,
      height = out$height,
      bg = "transparent",
      device = ragg::agg_tiff,
      dpi = 300,
      ...
    )
    # suppressWarnings(pdftools::pdf_convert(pdf_loc, dpi = 300,filenames = withExt("tiff"), format="tiff", page=1, verbose = FALSE))
    out$tiff = withExt("tiff")
  }

  return(structure(out, class = "rendered_plot"))
}


#' Print a rendered_plot object
#'
#' @param x the rendered_plot
#' @param ... not used
#'
#' @return nothing - used for side effects
#' @export
#' @concept output
print.rendered_plot = function(x, ...) {
  if (interactive()) {
    mktmp = function(file) {
      tmp = tempfile()
      fs::file_copy(file, tmp)
      return(tmp)
    }

    # open the pdf or png in a viewer to check dimensions
    v = getOption("viewer", utils::browseURL)
    if (!is.null(x$png)) {
      v(mktmp(x$png))
    } else if (!is.null(x$svg)) {
      v(mktmp(x$svg))
    } else if (!is.null(x$pdf)) {
      utils::browseURL(x$pdf)
    } else {
      # this will generally open in a viewer and will output pdf by default
      # grDevices::dev.new(width=x$width,height=x$height,unit="in",noRStudioGD = TRUE)
      print(x$plot)
    }

    if (.is_running_in_chunk()) {
      # this is running in a chunk in RStudio.
      # If this was not the case it would be rendered by knitr::knit_print
      # trick here is to render something inline in markdown document.
      grDevices::dev.new(width = x$width, height = x$height, unit = "in")
      print(x$plot)
    }
  } else {
    # file paths are more useful when not interactive.
    print(as.character(x))
  }
}

#' Convert a rendered_plot object to a character
#'
#' @param x the rendered_plot
#' @param ... not used
#'
#' @return a named vector
#' @export
#' @concept output
as.character.rendered_plot = function(x, ...) {
  tmp = x[!names(x) %in% c("plot", "width", "height")]
  out = sprintf("a ggplot with %d outputs:", length(tmp))
  if (length(tmp) > 0) {
    class(tmp) = "list"
    out = c(
      out,
      sprintf("%s: %s", names(tmp), sapply(tmp, paste0, collapse = ", "))
    )
  }
  return(out)
}

#' Knit a rendered_plot object
#'
#' @param x the rendered_plot
#' @param options the chunk options
#' @param ... not used
#'
#' @importFrom knitr knit_print
#'
#' @return nothing - used for side effects
#' @export
#' @concept output
knit_print.rendered_plot = function(x, options, ...) {
  # return(knitr::asis_output(sprintf("<img src='%s'></img>", base64enc::dataURI(file = x$png, mime = "image/png"))))
  # overwrite current settings for defaults

  options$fig.width = x$width
  options$fig.height = x$height

  if (.is_html_output()) {
    if (!is.null(x$png)) {
      return(knitr::asis_output(sprintf(
        "<img src='%s'></img>",
        base64enc::dataURI(file = x$png, mime = "image/png")
      )))
      # return(knitr::include_graphics(path = x$png, dpi=300))
    } else if (!is.null(x$svg)) {
      return(knitr::asis_output(sprintf(
        "<img src='%s'></img>",
        base64enc::dataURI(file = x$svg, mime = "image/svg+xml")
      )))
      # N.B. don't technically need to encode: https://codepen.io/tigt/post/optimizing-svgs-in-data-uris
      # return(knitr::include_graphics(path = x$svg))
    } else {
      return(knitr::knit_print(x$plot, options, ...))
    }
    # return(knitr::asis_output(sprintf("<img src='%s'></img>", base64enc::dataURI(file = x$png, mime = "image/png"))))
  } else if (.is_latex_output()) {
    if (!is.null(x$pdf)) {
      return(knitr::include_graphics(path = x$pdf))
    } else if (!is.null(x$png)) {
      return(knitr::include_graphics(path = x$png, auto_pdf = TRUE, dpi = 300))
    } else {
      # else if (!is.null(x$svg)) return(knitr::include_graphics(path = x$svg))
      return(knitr::knit_print(x$plot, options, ...))
    }
  } else {
    return(knitr::knit_print(x$plot, options, ...))
  }
}

# TODO: https://yihui.org/knitr/options/#chunk-options
# dev: ('pdf' for LaTeX output and 'png' for HTML/Markdown; character) The
# graphical device to generate plot files. All graphics devices in base R and
# those in Cairo, svglite, ragg, and tikzDevice are supported, e.g., pdf, png,
# svg, jpeg, tiff, cairo_pdf, CairoJPEG, CairoPNG, svglite, gridSVG, ragg_png,
# tikz, and so on. See names(knitr:::auto_exts) for the full list. Besides these
# devices, you can also provide a character string that is the name of a
# function of the form function(filename, width, height). The units for the
# image size are always inches (even for bitmap devices, in which DPI is used to
# convert between pixels and inches).

# Huxtable save as ----

#' Save a table to a variety of formats
#'
#' depending on the context return the correct format for a document.
#' The basic output here is to use HTML as an output if possible and convert it to an image or a PDF that can then
#' be included into a latex document for example.
#'
#' @param hux the huxtable to save
#' @param filename the filename, which may omit the extension
#' @param size a `std_size` list entry
#' @param maxWidth or the maximum width in inches
#' @param maxHeight and either the maximum height in inches
#' @param aspectRatio or the minimum allowed aspect ratio
#' @param formats if the extension is omitted, all the formats described here will be saved. Currently supported outputs are "html","png","pdf","docx","xlsx"
#' @param defaultFontSize the default font size
#' @param sheetname if saving as an xlsx file.
#' @param pdfConverter a function that takes an HTML fragment and returns a pdf file.
#' @param webfontFinder a function that takes a set of font families and returns a css webfonts directive
#'
#' @return the output depends on if the function is called in a knitr session. It maybe the HTML or a link to the pdf output for example.
#' @keywords internal
#' @concept output
#' @examples
#' try({
#'  hux = iris %>% huxtable::as_hux() %>%
#'    huxtable::theme_mondrian(font="Roboto")
#'  out = .hux_save_as(hux, tempfile())
#'  # browseURL(out$html)
#'
#'  out2 = .hux_save_as(hux, tempfile(), formats=c("pdf","png"))
#'  as.character(out2)
#'
#'  # The resulting pdf has fonts embedded & is multipage.
#' })
#'
.hux_save_as = function(
  hux,
  filename,
  size = std_size$full,
  maxWidth = size$width,
  maxHeight = size$height,
  aspectRatio = maxWidth / maxHeight,
  formats = c("html", "docx"),
  defaultFontSize = 8,
  sheetname = fs::path_ext_remove(fs::path_file(filename)),
  pdfConverter = .print_html_with_chrome,
  webfontFinder = ~ return(list())
) {
  html2 = NULL # remove global binding note
  pdfConverter = rlang::as_function(pdfConverter)
  webfontFinder = rlang::as_function(webfontFinder)

  if (!huxtable::is_hux(hux)) {
    stop("input must be a huxtable")
  }
  if (.is_knitting() & .is_latex_output()) {
    formats = unique(c(formats, "pdf"))
  }

  supported = c("html", "png", "pdf", "docx", "xlsx")
  formats = formats[formats %in% supported]

  dir = fs::path_dir(filename)
  if (dir == ".") {
    stop(
      "directory not given. filename must be a full path (use `.here` function)."
    )
  }
  if (fs::path_ext(filename) %in% supported) {
    formats = fs::path_ext(filename)
  }
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  filename = fs::path_ext_remove(filename)
  withExt = function(extn) {
    fs::path_ext_set(filename, extn)
  }

  matchedFiles = function(extns) {
    extns = paste0(extns, collapse = "|")
    fs::dir_ls(
      fs::path_dir(filename),
      regexp = paste0(.escape_regex(filename), "(_[0-9]+)?\\.(", extns, ")")
    )
  }

  # clean up any possible outputs from previous run
  lapply(matchedFiles(supported), function(x) try(unlink(x)))

  fonts_used = hux %>% huxtable::font() %>% as.vector() %>% unique()
  non_local_fonts = fonts_used[
    !fonts_used %in% systemfonts::system_fonts()$family
  ]

  out = list()
  out$hux = hux
  out$width = maxWidth
  out$height = maxHeight

  if ("docx" %in% formats) {
    doc = officer::read_docx()
    # browser()
    pageSize =
      if (maxWidth >= 21 * 2 / 2.54) {
        #A3 landscape
        officer::page_size(
          width = 29.7 / 2.54,
          height = 21 * 2 / 2.54,
          orient = "landscape"
        )
      } else if (maxWidth >= 29.7 / 2.54) {
        #A3 portrait
        officer::page_size(
          width = 29.7 / 2.54,
          height = 21 * 2 / 2.54,
          orient = "portrait"
        )
      } else if (maxWidth >= 21 / 2.54) {
        #A4 landscape
        officer::page_size(
          width = 21 / 2.54,
          height = 29.7 / 2.54,
          orient = "landscape"
        )
      } else {
        officer::page_size(
          width = 21 / 2.54,
          height = 29.7 / 2.54,
          orient = "portrait"
        )
      } #A4 portrait

    marginSize = (pageSize$width - maxWidth) / 2

    doc = doc %>%
      officer::body_set_default_section(
        officer::prop_section(
          page_size = pageSize,
          page_margins = officer::page_mar(
            marginSize,
            marginSize,
            marginSize,
            marginSize,
            0.1,
            0.1,
            0.1
          ),
          type = "continuous"
        )
      )
    ft = hux %>% huxtable::as_flextable() %>% flextable::autofit(add_h = 0)
    for (i in 1:ncol(hux)) {
      ft = ft %>%
        flextable::width(
          j = i,
          width = huxtable::col_width(hux)[[i]] * maxWidth
        )
    }
    # ft = ft %>% #flextable::autofit(part = "body") %>%
    # flextable::fit_to_width(maxWidth)
    if (!is.null(ft$caption$value)) {
      if (utils::packageVersion("flextable") >= "0.5.5") {
        doc <- officer::body_add_par(
          doc,
          ft$caption$value,
          style = "table title"
        )
      }
    }
    doc <- flextable::body_add_flextable(doc, ft)
    doc <- officer::body_add_par(doc, " ")
    print(doc, target = withExt("docx"))
    out$docx = withExt("docx")
  }

  if ("xlsx" %in% formats) {
    hux %>% huxtable::quick_xlsx(file = withExt("xlsx"), open = FALSE)
    out$xlsx = withExt("xlsx")
  }

  if (any(c("pdf", "png", "html") %in% formats)) {
    html = stringr::str_remove(
      hux %>% huxtable::to_html(),
      stringr::fixed("margin-bottom: 2em; margin-top: 2em;")
    )

    if ("html" %in% formats) {
      style_dec = ""
      fonts = .hux_used_fonts(hux)
      webfonts = webfontFinder(fonts)
      if (length(webfonts) > 0) {
        style_dec = sprintf(
          "<style>%s</style>",
          paste0(webfonts, collapse = "")
        )
      }
      write(
        sprintf(
          "<html><head><meta charset='UTF-8'>%s</head><body>%s</body></html>",
          style_dec,
          html
        ),
        withExt("html")
      )
      out$html = withExt("html")
    }

    if (any(c("pdf", "png") %in% formats)) {
      tmp = c("pdf", "png")[c("pdf", "png") %in% formats]
      unlink(withExt("pdf"))
      pdfConverter(
        html,
        withExt("pdf"),
        maxWidth = maxWidth,
        maxHeight = maxHeight
      )
      if (fs::file_exists(withExt("pdf"))) {
        if ("pdf" %in% tmp) {
          out$pdf = withExt("pdf")
        }
        if ("png" %in% formats) {
          pngs = .convert_pdf_to_pngs(withExt("pdf"))
          if ("png" %in% tmp) out$png = unname(matchedFiles("png"))
        }
      }
    }
  }

  return(structure(out, class = "rendered_table"))
}

#' Knit a rendered_table object
#'
#' @param x the rendered_table
#' @param ... not used
#'
#' @return nothing - used for side effects
#' @export
#' @concept output
knit_print.rendered_table = function(x, ...) {
  hidetables = getOption("hide.tables", FALSE)
  if (hidetables) {
    # e.g. knitting to a word document
    return(knitr::asis_output(paste0(
      "INSERT TABLE HERE: ",
      as.character(x),
      "\n\n"
    )))
  } else {
    pngs = x$png
    if (.is_html_output()) {
      # knitting a html document
      return(knitr::knit_print(x$hux %>% huxtable::set_width("auto")))
    } else if (.is_document_output()) {
      # knitting a docx document
      # try and embed in the document but width must be specified with no
      # sensible default and paging is determined by word.
      return(knitr::knit_print(x$hux %>% huxtable::set_width(1)))
    } else {
      # most likely latex
      if (.is_latex_output() & !is.null(x$pdf)) {
        return(knitr::include_graphics(path = x$pdf))
      } else if (length(pngs) > 0) {
        # this will be default output in many situations - a png with fallback to pdf if available.
        return(knitr::include_graphics(path = pngs, auto_pdf = TRUE, dpi = 300))
      } else {
        # some non standard output.
        # just let huxtable decide what the best thing to do is.
        return(knitr::knit_print(x$hux))
      }
    }
  }
}

#' Convert a rendered_table object to a character
#'
#' @param x the rendered_table
#' @param ... not used
#'
#' @return a named vector
#' @export
#' @concept output
#' @examples
#' hux = iris %>% hux_default_layout()
#' tmp = hux %>% hux_save_as(tempfile())
#' as.character(tmp)
as.character.rendered_table = function(x, ...) {
  tmp = x[!names(x) %in% c("hux", "width", "height")]
  out = sprintf("a huxtable with %d outputs:", length(tmp))
  if (length(tmp) > 0) {
    class(tmp) = "list"
    out = c(
      out,
      sprintf("%s: %s", names(tmp), sapply(tmp, paste0, collapse = ", "))
    )
  }
  return(out)
}

#' Print a rendered_table object
#'
#' @param x the rendered_table
#' @param ... not used
#'
#' @return nothing - used for side effects
#' @export
#' @concept output
print.rendered_table = function(x, ...) {
  if (interactive()) {
    v = getOption("viewer", utils::browseURL)
    if (!is.null(x$pdf)) {
      v(x$pdf)
    } else if (!is.null(x$png)) {
      v(x$png)
    } else if (!is.null(x$html)) {
      v(x$html)
    } else {
      htmltools::html_print(htmltools::HTML(x$hux %>% huxtable::to_html()))
    }
  }

  print(x$hux)
}


# private utilities ----

# from rex:::escape.character
.escape_regex = function(x) {
  chars <- c(
    "*",
    ".",
    "?",
    "^",
    "+",
    "$",
    "|",
    "(",
    ")",
    "[",
    "]",
    "{",
    "}",
    "\\"
  )
  gsub(
    paste0("([\\", paste0(collapse = "\\", chars), "])"),
    "\\\\\\1",
    x,
    perl = TRUE
  )
}
