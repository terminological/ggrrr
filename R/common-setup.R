# here::i_am("common-setup.R")

## manage dependencies ----

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
    if (!package %in% rownames(installed.packages())) {
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

#' Create a function to generate versioned file name in a subdirectory.
#'
#' @param directory the root of the output
#' @param datedFile do you want the filename to have the date appended
#' @param datedSubdirectory do you want the files to be placed in a dated subdirectory
#'
#' @return a function that takes a filename and boolean delete parameter. The filename will be the relative path within the directory.
#' @export
#'
#' @examples
#' out = outputter("~/output",datedSubdirectory=TRUE)
#' out("file.png")
outputter = function(directory = here::here("output"), datedFile=!datedSubdirectory, datedSubdirectory=FALSE) {
  directory = fs::path_norm(directory)
  fs::dir_create(directory)
  message("directing output to: ",directory)
  return(function(filename, delete=FALSE) {
    if(datedSubdirectory) {
      directory = fs::path(directory,Sys.Date())
    }
    ext = fs::path_ext(filename)
    if(datedFile) filename = paste0(fs::path_ext_remove(filename),"-",Sys.Date()) %>% fs::path_ext_set(ext)
    path = fs::path(directory,filename)
    if (delete & fs::file_exists(path)) unlink(path)
    return(path)
  })
}


#' Create a function list that allows for supplementary tables (as huxtables) to be added to a XLSX output file.
#'
#' getOption("hide.supplementary.tables",default=TRUE)
#'
#' @param ... output location options will be passed to outputter(...) to define the location of the file
#' @param filename the xlsx filename
#' @param out an outputter (defaults to outputter)
#' @param nameGlue What will the tables be named
#'
#' @return a list of 2 functions.
#' $add_table(hux, caption, footnote, index), which takes a huxtable, caption, and index a writes the huxtable into a supplementary.
#' $write() which writes the collection of tables to the excel file.
#' @export
#'
#' @examples
data_supplement = function(..., filename="supplementary-material.xlsx", out = outputter(...), nameGlue="Supplementary Table {index}") {
  e1 = new.env(parent = environment())
  with(e1, {

    .out = out
    .wb = openxlsx::createWorkbook()
    .indexes = integer()
    .default_filename = filename
    .catalog = tibble(table=character(),description=character())

    add_table = function(hux, caption, footnote = NULL, index=NULL, hide=getOption("hide.supplementary.tables",default=TRUE)) {
      wb = .wb
      indexes = .indexes
      if(is.null(index)) index = max(c(indexes,0))+1
      sheetname = glue::glue(nameGlue)
      if(sheetname %in% names(wb)) openxlsx::removeWorksheet(wb,sheet = sheetname)

      hux2 = hux %>% hux_auto_widths("xlsx") %>%
        huxtable::set_caption(paste0(sheetname," - ",caption)) %>%
        huxtable::set_caption_pos("topleft")

      if (!is.null(footnote)) {
        hux2 = hux2 %>% huxtable::insert_row(footnote, after=nrow(hux2), colspan = ncol(hux2), fill="",copy_cell_props = FALSE)
      }

      wb = hux2 %>% huxtable::as_Workbook(Workbook = wb, sheet = sheetname)
      .catalog <<- .catalog %>% filter(table != sheetname) %>% bind_rows(tibble(table=sheetname,description=caption))
      .wb <<- wb
      # indexes keeps track of order in which sheets added and overwritten
      .indexes <<- unique(c(indexes,index),fromLast = TRUE)
      if(!hide) return(hux2 %>% huxtable::set_width("auto"))
      invisible(sheetname)
    }

    write = function(filename = .out(.default_filename)) {

      cat = .catalog %>%
        mutate(.sort = dense_rank(.indexes)) %>%
        arrange(.sort) %>%
        select(-.sort) %>%
        huxtable::as_hux(add_colnames=FALSE) %>%
        huxtable::set_caption("Supplementary materials - table of contents") %>%
        huxtable::set_caption_pos("topleft") %>%
        huxtable::set_col_width(c(0.2,0.8)) %>%
        # 25 here is the desired with of the table of contents
        huxtable::set_width((25 / 0.206) / (20*ncol(.catalog)))
      # %>%
      # huxtable::set_height(nrow(.catalog)*20) %>%
      # huxtable::set_row_height(rep(1/(nrow(.catalog)+1),(nrow(.catalog)+1)))

      wb = .wb
      # add in new contents page
      if("Contents" %in% names(wb)) openxlsx::removeWorksheet(wb,"Contents")
      wb = cat %>% huxtable::as_Workbook(Workbook = wb, sheet = "Contents")

      # bring contents page to front. Order others by index.
      openxlsx::worksheetOrder(wb) = order(c(.indexes,0))
      openxlsx::saveWorkbook(wb,filename,overwrite = TRUE)
      invisible(filename)
    }
  })
  return(e1)
}

## Fonts ----

#' Version of loadfonts from extra fonts that uses first font file it finds rather than failing if mulitple found
#'
#' @param device The output device. Can be "pdf" (the default), "postscript", or "win".
#' @param quiet If FALSE, print a status message as each font is registered. If TRUE, don't print.
#'
#' @return
#' @export
loadfonts = function (device = "pdf", quiet = FALSE)
{
  fontdata <- extrafont::fonttable()
  if (device == "pdf") {
    cfonts <- names(grDevices::pdfFonts())
    ffname <- "pdfFonts"
  }
  else if (device == "postscript") {
    cfonts <- names(grDevices::postscriptFonts())
    ffname <- "postscriptFonts"
  }
  else if (device == "win") {
    cfonts <- names(grDevices::windowsFonts())
    ffname <- "windowsFonts"
  }
  else {
    stop("Unknown device: ", device)
  }
  fontfunc <- match.fun(ffname)
  if (device %in% c("pdf", "postscript")) {
    for (family in unique(fontdata$FamilyName)) {
      if (family %in% cfonts) {
        if (!quiet)
          message(family, " already registered with ",
                  ffname, "().")
        (next)()
      }
      fd <- fontdata[fontdata$FamilyName == family, ]
      regular <- head(fd$afmfile[!fd$Bold & !fd$Italic],1)
      bold <- head(fd$afmfile[fd$Bold & !fd$Italic],1)
      italic <- head(fd$afmfile[!fd$Bold & fd$Italic],1)
      bolditalic <- head(fd$afmfile[fd$Bold & fd$Italic],1)
      if (length(regular) == 0) {
        if (!quiet) {
          message("No regular (non-bold, non-italic) version of ",
                  family, ". Skipping setup for this font.")
        }
        (next)()
      }
      if (length(bold) == 0)
        bold <- regular
      if (length(italic) == 0)
        italic <- regular
      if (length(bolditalic) == 0)
        bolditalic <- bold
      if (!is.na(fd$afmsymfile[1]) && fd$afmsymfile[1] !=
          "" && all(fd$afmsymfile[1] == fd$afmsymfile)) {
        symbol <- fd$afmsymfile[1]
      }
      else {
        symbol <- NULL
      }
      if (!quiet)
        message("Registering font with R using ", ffname,
                "(): ", family)
      args <- list()
      args[[family]] <- grDevices::Type1Font(family, metrics = file.path(extrafont:::metrics_path(),
                                                              c(regular, bold, italic, bolditalic, symbol)))
      do.call(fontfunc, args)
    }
  }
  else if (device == "win") {
    for (family in unique(fontdata$FamilyName)) {
      if (family %in% cfonts) {
        if (!quiet)
          message(family, " already registered with ",
                  ffname, "().")
        (next)()
      }
      fd <- fontdata[fontdata$FamilyName == family, ]
      if (!quiet)
        message("Registering font with R using ", ffname,
                "(): ", family)
      args <- list()
      args[[family]] <- grDevices::windowsFont(family)
      do.call(fontfunc, args)
    }
  }
}
