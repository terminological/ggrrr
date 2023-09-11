## Fonts ----

#' Which fonts are available on this system.
#'
#' @param family a font family name or names
#'
#' @return the font family name if it can be located or an empty list otherwise
#' @export
#' @examples
#' fonts_available(c("Arial","sdfdsfsd"))
fonts_available = function(family) {
  families = c(systemfonts::registry_fonts()$family, systemfonts::system_fonts()$family)

  if (rlang::is_missing(family)) {
    return(unique(families))
  } else {
    return(family[family %in% families])
  }
}

#' Ensures a font is available.
#'
#' This checks to see if a font exists. If missing it will try and install from google fonts.
#' If this is not possible it will throw an error.
#'
#' @param family a font family name or names
#'
#' @return the font family name if it can be located or an error otherwise
#' @export
#'
#' @examples
#' check_font(c("Roboto","Arial","Kings","EB Garamond"))
check_font = function(family) {

  path = NULL

  .find_and_download_web_fonts(family)
  .register_web_fonts_with_systemfont()
  .import_systemfont_to_extrafont(family)

  sub = .substitute_fonts(family)

  switched = dplyr::setdiff(sub,c(family,"sans","serif","monotype"))
  if (length(switched) > 0) {
    .find_and_download_web_fonts(switched)
    .import_systemfont_to_extrafont(sub)
  }

  return(sub)

}




#' Reset any custom fonts
#'
#' @return nothing
#' @export
reset_fonts = function() {
  options("ggrrr.webfonts" = list())
  systemfonts::clear_registry()
  .reset_webfont_catalogue()
}

.cache_loc = function(filename) {
  fs::dir_create(fs::path(fs::path_expand_r(rappdirs::user_cache_dir("ggrrr")),"fonts"))
  fs::path(fs::path_expand_r(rappdirs::user_cache_dir("ggrrr")),"fonts",fs::path_sanitize(filename))
}

.font_loc = function(filename) {
  if (.Platform$OS.type == "unix") {
    if(grepl("^darwin", R.version$os)) {
      # mac
      dir = fs::path_home_r("Library/Fonts")
    } else {
      # linux
      dir = fs::path_home_r(".fonts")
    }
  } else {
    # windows
    dir = fs::path(Sys.getenv("LOCALAPPDATA"),"Microsoft/Windows/Fonts")
  }
  fs::dir_create(dir)
  fs::path(dir,fs::path_sanitize(filename))
}

## webfont catalogue ----

.webfont_catalogue = new.env()


#' @noRd
#' @examples
#' .search_webfont_services(c("Roboto","EB Garamond"))
.search_webfont_services = function(fonts, services = names(webfont_provider), ...) {

  font_family = NULL

  services = match.arg(services, several.ok = TRUE)
  deferred = fonts[!.check_webfont_catalogue(fonts)]

  out = .empty_webfont_catalogue()

  for (service in services) {
    request_url = webfont_provider[[service]](deferred)
    response_df = .parse_css(request_url)
    out = dplyr::bind_rows(out,response_df)
    deferred = deferred[!deferred %in% response_df$font_family]
    if (length(deferred)==0) break;
  }

  .add_webfont_catalogue(out)
  out = .get_webfont_catalogue() %>% dplyr::filter(font_family %in% fonts)

  return(out)
}

#' @noRd
#' @examples
#' .get_webfont_catalogue()
#' .webfont_catalogue
.get_webfont_catalogue = function() {
  if (!exists("x",envir=.webfont_catalogue)) {
    if (fs::file_exists(.cache_loc("webfonts.csv"))) {
      tmp =  readr::read_csv(.cache_loc("webfonts.csv"),col_types = readr::cols(.default = readr::col_character()))
      assign("x", tmp, envir=.webfont_catalogue)
    } else {
      assign("x", .empty_webfont_catalogue(), envir=.webfont_catalogue)
    }
  }
  return(get("x", envir = .webfont_catalogue))
}

.empty_webfont_catalogue = function() {
  tibble::tibble(
    font_family = character(),
    font_style = character(),
    font_weight = character(),
    url = character(),
    local = character(),
    format = character(),
    unicode_range = character()
  )
}

.add_webfont_catalogue = function(css_df) {
  tmp = dplyr::distinct(dplyr::bind_rows(
    .get_webfont_catalogue(),
    css_df
  ))
  assign("x", tmp, envir=.webfont_catalogue)
  readr::write_csv(
    tmp,
    file = .cache_loc("webfonts.csv"))
}

.reset_webfont_catalogue = function() {
  fs::file_delete(.cache_loc("webfonts.csv"))
  assign("x", .empty_webfont_catalogue(), envir=.webfont_catalogue)
}

## check catalogues ----

#' @noRd
#' @examples
#' .check_webfont_catalogue(c("Roboto","Times New Roman"))
.check_webfont_catalogue = function(family) {
  families = .get_webfont_catalogue()$font_family
  tmp = family %in% families
  names(tmp) = family
  return(tmp)
}

#' @noRd
#' @examples
#' .check_systemfonts(c("Roboto","Times New Roman"))
.check_systemfonts = function(family, valid_ps = FALSE) {
  families = .all_system_fonts(valid_ps = valid_ps)$family
  tmp = family %in% families
  names(tmp) = family
  return(tmp)
}

#' @noRd
#' @examples
#' .check_postscript_font(c("Roboto","Times New Roman"))
.check_postscript_font = function(family) {
  tmp = family %in% names(grDevices::postscriptFonts())
  names(tmp) = family
  return(tmp)
}

#' @noRd
#' @examples
#' .check_pdf_font(c("Roboto","Times New Roman"))
.check_pdf_font = function(family) {
  tmp = family %in% names(grDevices::pdfFonts())
  names(tmp) = family
  return(tmp)
}

#' @noRd
#' @examples
#' .check_extrafonts(c("Roboto","Arial","Kings","EB Garamond"))
.check_extrafonts = function(family) {
  tmp = family %in% extrafont::fonts()
  names(tmp) = family
  return(tmp)
}

## format conversion utilities ---

.esc = function(names) {
  stringr::str_replace_all(names,"[[:space:]]", "+")
}

# .unquote(c("ok","'ok'","\"ok\""))
.unquote = function(s) {
  stringr::str_extract(s, "^('|\")?([^'\"]*)('|\")?$", group = 2)
}

.style_to_css_weight = function(style) {
  dplyr::case_when(
    stringr::str_detect(tolower(style), stringr::fixed("bold")) ~ "700",
    stringr::str_detect(tolower(style), stringr::fixed("12")) ~ "700",
    stringr::str_detect(tolower(style), stringr::fixed("regular")) ~ "400",
    stringr::str_detect(tolower(style), stringr::fixed("normal")) ~ "400",
    stringr::str_detect(tolower(style), stringr::fixed("08")) ~ "400",
    stringr::str_detect(tolower(style), stringr::fixed("thin")) ~ "100",
    stringr::str_detect(tolower(style), stringr::fixed("extra light")) ~ "200",
    stringr::str_detect(tolower(style), stringr::fixed("light")) ~ "300",
    stringr::str_detect(tolower(style), stringr::fixed("book")) ~ "350",
    stringr::str_detect(tolower(style), stringr::fixed("medium")) ~ "500",
    stringr::str_detect(tolower(style), stringr::fixed("semi bold")) ~ "600",
    stringr::str_detect(tolower(style), stringr::fixed("extra bold")) ~ "800",
    stringr::str_detect(tolower(style), stringr::fixed("black")) ~ "900",
    tolower(style) == "italic" ~ "400",
    tolower(style) == "oblique" ~ "400",
    TRUE ~ NA_character_)
}

.style_to_css_style = function(style) {
  dplyr::case_when(
    stringr::str_detect(tolower(style), stringr::fixed("italic")) ~ "italic",
    stringr::str_detect(tolower(style), stringr::fixed("oblique")) ~ "italic",
    TRUE ~ "normal")
}

.style_to_ps_face = function(style = NULL, css_weight=.style_to_css_weight(style), css_style=.style_to_css_style(style)) {
  dplyr::case_when(
    css_weight == "400" & css_style == "normal" ~ "plain",
    css_weight == "700" & css_style == "normal" ~ "bold",
    css_weight == "400" & css_style == "italic" ~ "italic",
    css_weight == "700" & css_style == "italic" ~ "bolditalic",
    TRUE ~ NA_character_
  ) %>% factor(levels=c("plain", "bold", "italic", "bolditalic"))
}

.css_format_to_extn = function(format) {
  dplyr::case_when(
    format == "truetype" ~ "ttf",
    format == "opentype" ~ "otf",
    format == "embedded-opentype" ~ "eof",
    TRUE ~ format
  )
}

# Webfonts ----
## adapted from showtextdb ----

webfont_provider = list(
  # google = function(fonts) {
  #   sprintf(
  #     "https://fonts.googleapis.com/css?family=%s",
  #     paste0(sprintf(
  #       "%s:r,b,i,bi", .esc(fonts)
  #     ),collapse="|")
  #   )
  # },
  google = function(fonts) {
    sprintf(
      "https://fonts.googleapis.com/css2?%s",
      paste0(sprintf(
        "family=%s:ital,wght@0,400;0,700;1,400;1,700", .esc(fonts)
      ),collapse="&")
    )
  },
  brick = function(fonts) {
    sprintf(
      "https://brick.freetls.fastly.net/%s",
      paste0(sprintf(
        "%s:400,400i,700,700i", .esc(fonts)
      ),collapse="/")
    )
  }
)

#' @noRd
#' @examples
#' url = webfont_provider$google("Kings")
#' .parse_css(url)
.parse_css = function(request_url) {

  res = tryCatch({
    ret = suppressMessages(suppressWarnings(ggrrr::cache_download(request_url, .stale = 7, quiet=TRUE, .extn="css")))
    res = readr::read_file(ret)
  }, error=function(e) "" )

  info = stringr::str_match_all(res, "@font-face\\s?\\{([^\\}]+)\\}")[[1]][,2]
  info = info[info != ""]

  tmp = tibble::tibble(
    font_family = stringr::str_extract(info, "font-family:[[:space:]]*([^:;]+);",group=1) %>% .unquote(),
    font_style = stringr::str_extract(info, "font-style:[[:space:]]*([^:;]+);", group=1),
    font_weight = stringr::str_extract(info, "font-weight:[[:space:]]*([[:digit:]]+);", group=1),
    url = stringr::str_extract(info, "url\\(([^\\)]+)\\)", group=1) %>% stringr::str_replace("^//", "https://"),
    format = stringr::str_extract(info, "format\\(([^\\)]+)\\)", group=1) %>% .unquote(),
    local = stringr::str_extract(info, "local\\(([^\\)]+)\\).*", group=1) %>% .unquote(),
    unicode_range = stringr::str_extract(info, "unicode-range:[[:space:]]*([^:;]+);", group=1)
  )

  return(tmp)
}



#' @noRd
#' @examples
#' tmp = .search_webfont_services(c("Roboto","EB Garamond")) %>% dplyr::mutate(
#'    face = .style_to_ps_face(css_weight = font_weight,css_style = font_style),
#'    extn = .css_format_to_extn(format),
#'    locn = .cache_loc(sprintf("%s-%s.%s",font_family, face, extn))
#' )
#' tmp %>% dplyr::mutate(ttf = purrr::map2_chr(url, locn, ~ .get_web_ttf(.x,.y), .progress="Downloading fonts"))
.get_web_ttf = function(url, ttf) {
  if (fs::file_exists(ttf)) return(ttf)
  #TODO: consider purrr:safely
  try({download.file(url,destfile = ttf,quiet = TRUE)},silent = TRUE)
  if (fs::file_exists(ttf)) return(ttf)
  return(NA_character_)
}

#' @noRd
#' @examples
#' .get_font_face(c("Roboto", "EB Garamond", "Kings"))
#'
.get_font_face = function(fonts) {

  font_family = font_weight = font_style = family = ps_face = url_type = . = NULL

  tmp = .get_webfont_catalogue() %>% dplyr::filter(font_family %in% fonts) %>% dplyr::mutate(ps_face = .style_to_ps_face(css_weight = font_weight, css_style = font_style))
  tmp2 = .all_system_fonts() %>% dplyr::filter(family %in% fonts & !is.na(ps_face)) %>% dplyr::select(family, ps_face, source) %>% dplyr::distinct()
  tmp3 = tmp %>% dplyr::left_join(tmp2, by=c("font_family" = "family","ps_face"))
  tmp4 = tmp3 %>% dplyr::mutate(
      local = dplyr::if_else(is.na(source), local,font_family),
      url_type = .css_format_to_extn(format)
    ) %>%
    dplyr::select(family = font_family, style = font_style, weight = font_weight, local, url_type, url) %>%
    # dplyr::group_by(family, style, weight, local, url_type) %>%
    # dplyr::arrange(dplyr::desc(source)) %>%
    # dplyr::filter(dplyr::row_number() == 1) %>%
    # dplyr::ungroup() %>%
    # dplyr::select(-source) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = url_type, values_from = url, values_fn = dplyr::first)
  tmp5 = tmp4 %>% dplyr::filter(!is.na(local)) %>% dplyr::mutate(font_face = purrr::pmap(., svglite::font_face))
  tmp6 = tmp4 %>% dplyr::filter(is.na(local)) %>% dplyr::select(-local) %>% dplyr::mutate(font_face = purrr::pmap(., svglite::font_face))
  return(c(tmp5$font_face,tmp6$font_face))
}

# Worfkflow ----

#' @noRd
#' @examples
#' tmp = .find_and_download_web_fonts("Kings")
#' tmp
#'
.find_and_download_web_fonts = function(fonts, services = names(webfont_provider), ...) {

  name = path = .  = font_family = font_weight = font_style = face = extn = locn = NULL

  services = match.arg(services, several.ok = TRUE)

  webfonts = .search_webfont_services(fonts, services)
  missing_webfonts = webfonts %>% filter(!.check_systemfonts(font_family))

  # download any new fonts.
  tmp = missing_webfonts %>% dplyr::mutate(
          face = .style_to_ps_face(css_weight = font_weight,css_style = font_style),
          extn = .css_format_to_extn(format),
          locn = .font_loc(sprintf("%s-%s.%s",font_family, face, extn))
        ) %>% dplyr::mutate(
          ttf = purrr::map2_chr(url, locn, ~ .get_web_ttf(.x,.y), .progress="Downloading fonts")
        )

  #TODO: what woudl happen if we rebuild systemfonts cache at this point?
  # e.g. systemfonts::reset_font_cache()
  # systemfonts::system_fonts()
  # it might prevent the webfonts and need for chrome and maybe rsvg can work
  # if the font is actually installed. Maybe.

  return(tmp)
}

#' @noRd
#' @examples
#' tmp = .find_and_download_web_fonts("Kings")
#' .register_web_fonts_with_systemfont(tmp)
.register_web_fonts_with_systemfont = function(webfonts = .get_webfont_catalogue()) {

  font_weight = font_style = font_family = face = extn = ttf = font_family = TRUE

  unregistered_with_systemfonts = webfonts %>% dplyr::anti_join(.all_system_fonts(valid_ps=FALSE), by=c("font_family"="family"))

  # register any newly downloaded fonts with systemfonts
  tmp2 = unregistered_with_systemfonts  %>% dplyr::mutate(
    face = .style_to_ps_face(css_weight = font_weight,css_style = font_style),
    extn = .css_format_to_extn(format),
    ttf = .font_loc(sprintf("%s-%s.%s",font_family, face, extn))
  ) %>% dplyr::select(
      name = font_family, face, ttf
    ) %>% tidyr::pivot_wider(
      names_from = face, values_from = ttf
    )

  outcome = tmp2 %>% purrr::pwalk( purrr::safely(systemfonts::register_font), .progress = "Registering fonts with systemfonts" )

  matched = webfonts %>%
    dplyr::left_join(.all_system_fonts(valid_ps=FALSE), by=c("font_family"="family")) %>%
    dplyr::mutate(family = font_family)

  #TODO: there can be a mismatch here due to the fact that systemfonts will
  # reuse plain face for bold and bolditalic if not given.

  return(matched)

}


#' @noRd
#' @examples
#' .import_systemfont_to_extrafont("Courier New")
#'
.import_systemfont_to_extrafont = function(family = .all_system_fonts(ttf_only = TRUE)$family) {

  path = name = ttf_files = NULL

  localfonts = unique(.all_system_fonts(ttf_only = TRUE)$family)
  # check what is locally available in systemfonts but not registered with grDevices
  toadd = .extrafont_missing(dplyr::intersect(family,localfonts))
  tmp = toadd %>%
    dplyr::select(ttf_files = path, family = name) %>%
    dplyr::distinct() %>%
    dplyr::group_by(family) %>%
    dplyr::summarise(ttf_files = list(ttf_files))
  tmp %>%
    purrr::pwalk(purrr::safely(.extrafont_ttf_import))
  suppressMessages(extrafont::loadfonts())
}

.import_systemfont_to_grdevices = function(family = .all_system_fonts(valid_ps = TRUE)$family) {

  localfonts = unique(.all_system_fonts(valid_ps = TRUE)$family)
  # check what is locally available in systemfonts but not registered with grDevices
  toadd = .grdevices_missing(dplyr::intersect(family,localfonts))

  tmp3 = .register_ttf(toadd$name, toadd$plain, toadd$bold, toadd$italic, toadd$bolditalic)

}


# systemfonts fonts ----

#' @noRd
#' @examples
#' .all_system_fonts() %>% dplyr::pull(family) %>% unique()
.all_system_fonts = function(valid_ps = FALSE, ttf_only = valid_ps) {

  weight = style = css_weight = css_style = path = ps_face = type = family =
    monospace = name = NULL

  tmp = dplyr::bind_rows(
    systemfonts::registry_fonts() %>% dplyr::mutate(weight = as.character(weight), source = "registry"),
    systemfonts::system_fonts() %>% dplyr::mutate(weight = as.character(weight), source = "system")
  ) %>% dplyr::mutate(
    source = factor(source, levels=c("registry","system")),
    css_weight = .style_to_css_weight(style),
    css_style = .style_to_css_style(style),
    ps_face = .style_to_ps_face(style, css_weight, css_style),
    type = fs::path_ext(path)
  )
  if (ttf_only) tmp = tmp %>% dplyr::filter(!is.na(ps_face) & type=="ttf")
  if (valid_ps) {
    tmp = tmp %>% dplyr::group_by(family, monospace, ps_face) %>%
      dplyr::arrange(monospace, name) %>%
      dplyr::filter(dplyr::row_number()==1) %>%
      dplyr::group_by(family) %>%
      dplyr::filter(dplyr::n() == 4) %>%
      dplyr::ungroup()
  }
  return(tmp)
}

#' @noRd
#' @examples
#' .substitute_fonts(c("Roboto","Arial","Kings"))
.substitute_fonts = function(family) {

  path = NULL

  tmp = tibble::tibble(
    family = family,
    path = sapply(family, function(x) systemfonts::match_font(x)$path)
  ) %>% dplyr::inner_join(
    .all_system_fonts(valid_ps = FALSE) %>% dplyr::select(sub=family,path), by="path"
  ) %>% dplyr::select(family, sub) %>%
    dplyr::distinct()
  names(tmp$sub) = tmp$family
  return(tmp$sub)
}

# extrafont fonts ----


#' @noRd
#' @examples
#' .extrafont_missing("EB Garamond")
.extrafont_missing = function(family) {

  path = ps_face = name = NULL

  tmp = family

  installed = extrafont::fonttable()$FamilyName
  to_install = family[!family %in% installed]

  installable = .all_system_fonts(ttf_only=TRUE) %>%
    dplyr::filter(family %in% to_install) %>%
    dplyr::filter(fs::path_ext(path) == "ttf") %>%
    dplyr::select(name=family,path,ps_face) %>%
    dplyr::group_by(name,ps_face) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup()


  # uninstallable = dplyr::setdiff(to_install,installable$family)

  return(installable)

}


.extrafont_ttf_import = function(ttf_files, family) {

  weight = fontname = familyname = fullname = italicangle = NULL

  if (length(ttf_files) == 0) return(NULL)
  # TODO: write afm gzipped here:
  # extrafont:::metrics_path()
  metrics_path = optional_fn("extrafont","metrics_path")
  ttf_extract = optional_fn("extrafont","ttf_extract")
  fonttable_add = optional_fn("extrafont","fonttable_add")

  fontmap <- suppressMessages(ttf_extract(ttf_files))
  afmfiles = fs::path(metrics_path(), fs::path_file(fontmap$fontfile) %>% fs::path_ext_set("afm.gz"))
  lapply(afmfiles, purrr::safely(.set_afm_meta), familyname=family)
  fontdata = .get_afm_meta(afmfiles)
  fontdata = fontdata %>% mutate(
    Bold = stringr::str_detect(weight, "Bold") | stringr::str_detect(fontname, "Bold|12"),
    Italic = stringr::str_detect(weight, "Italic|Oblique") | stringr::str_detect(fontname, "Italic|Oblique"),
    Symbol = stringr::str_detect(familyname, "Symbol"),
    afmfile = fs::path_file(afmfiles),
    fontfile = ttf_files,
    afmsymfile = NA
  ) %>% dplyr::rename(
    FamilyName = familyname,
    FontName = fontname,
    FullName = fullname
  ) %>% select(c(-weight,-italicangle))

  # then continue as before
  # fontmap <- fontmap[!is.na(fontmap$FontName), ]
  # # message("Found FontName for ", nrow(fontmap), " fonts.")
  # afmdata <- extrafont:::afm_scan_files()
  #
  # fontdata <- afmdata %>% dplyr::filter(afmfile %in% afmfiles)

  if (nrow(fontdata) > 0) {
    fontdata$package <- NA
    suppressMessages(fonttable_add(fontdata))
  }
  return(NULL)
}

# grDevices fonts ----

#' @noRd
#' @examples
#' .grdevices_missing("Courier New")
#'
.grdevices_missing = function(family) {

  path = ps_face = NULL

  tmp = family

  both_present = family[.check_postscript_font(family) & .check_pdf_font(family)]

  to_install = dplyr::setdiff(tmp, both_present)

  installable = .all_system_fonts(valid_ps = TRUE) %>%
      dplyr::filter(family %in% to_install) %>%
      dplyr::select(name=family,path,ps_face) %>%
      tidyr::pivot_wider(names_from=ps_face, values_from=path) %>%
      dplyr::ungroup()

  if (nrow(installable) == 0) {
    installable = installable %>% dplyr::mutate(
      plain = character(),
      italic = character(),
      bold = character(),
      bolditalic = character()
    )
  }

  # uninstallable = dplyr::setdiff(to_install,installable$family)

  return(installable)

}

#' @noRd
#' @examples
#' library(magrittr)
#' toadd = .grdevices_missing(.all_system_fonts()$family)
#' # toadd = .grdevices_missing(sample(.all_system_fonts()$family,5))
#' # toadd = .grdevices_missing("Courier New")
#' tmp = .register_ttf(toadd$name, toadd$plain, toadd$bold, toadd$italic, toadd$bolditalic)
#' tmp$status
.register_ttf = function(name, plain, bold = plain, italic = plain, bolditalic = plain, ...) {
  # https://github.com/sjewo/extrafont/blob/master/R/truetype.r

  face = registered_pdf = registered_ps = ttf = loc = afm = files = missing_afm = type1 = NULL

  tmp = tibble::tibble(
    name = name,
    plain = plain,
    bold = bold,
    italic = italic,
    bolditalic = bolditalic
  ) %>% dplyr::mutate(
    registered_pdf  = .check_pdf_font(name),
    registered_ps  = .check_postscript_font(name)
  ) %>% tidyr::pivot_longer(
    cols = c(plain, bold, italic, bolditalic),
    names_to = "face", values_to = "ttf"
  ) %>% dplyr::mutate(
    loc = .cache_loc(sprintf("%s-%s.afm",name,face)),
    afm = dplyr::if_else(
      !registered_pdf | !registered_ps,
      purrr::map2_chr(ttf, loc, ~ .exec_ttf2afm(.x, .y), .progress = "Converting TTF to AFM"),
      NA_character_
    )
  ) %>% dplyr::select(
    -loc
  )

  tmp = tmp %>% tidyr::nest(
    files = c(face,ttf,afm)
  ) %>% dplyr::mutate(
    missing_afm = purrr::map_lgl(files, ~ any(is.na(.x$afm)))
  ) %>% dplyr::mutate(
    type1 = dplyr::if_else(
      !missing_afm,
      purrr::map2(name, files, ~Type1Font(.x, .y$afm)),
      list(NULL)
    )
  )



  tmp2 = tmp %>% dplyr::filter(!registered_ps & !missing_afm) %>%
    dplyr::select(name,type1)
  arg2 = tmp2$type1
  names(arg2) = tmp2$name
  message(tmp2$name)
  browser()

  try({
    suppressMessages(do.call(grDevices::postscriptFonts, args = arg2))
  })

  tmp3 = tmp %>% dplyr::filter(!registered_pdf & !missing_afm) %>%
    dplyr::select(name,type1)
  arg3 = tmp3$type1
  names(arg3) = tmp3$name
  message(tmp3$name)
  browser()

  try({
    suppressMessages(do.call(grDevices::pdfFonts, args = arg3))
  })

  tmp = tmp %>% dplyr::mutate(
    new_registered_pdf  = .check_pdf_font(name),
    new_registered_ps  = .check_postscript_font(name)
  )

  tmp = tmp %>% dplyr::mutate(
    status = dplyr::case_when(
      is.null(.ttf2afm_binary) ~ "SKIP: no binary for ttf2afm",
      registered_pdf & registered_ps ~ sprintf("SKIP: %s already registered",name),
      missing_afm ~ sprintf("SKIP: conversion failed for the font %s",name),
      new_registered_pdf & new_registered_ps ~ sprintf("OK: registered %s for ps and pdf",name),
      new_registered_ps ~ sprintf("OK: registered %s for ps",name),
      new_registered_pdf ~ sprintf("OK: registered %s for pdf",name),
      TRUE ~ sprintf("FAIL: unable to register %s",name)
    )
  )

  return(tmp)

}

#' @noRd
#' @examples
#' .ttf2afm_binary()
.ttf2afm_binary = function() {
  tmp = getOption("ggrrr.ttf2afm", default = Sys.which("ttf2afm"))
  if (tmp == "") return(NULL)
  return(enc2native(tmp))
}

#  ttf2afm /Library/Fonts/Impact.ttf /out/path/Impact

#' @noRd
#' @examples
#' ttf = systemfonts::system_fonts() %>% dplyr::filter(family == "Roboto") %>% dplyr::pull(path) %>% utils::head(1)
#' afm = fs::path(tempdir(),fs::path_file(ttf)) %>% fs::path_ext_set("afm")
#' converted_afm = .exec_ttf2afm(ttf, afm, overwrite=TRUE)
#' tmp = readr::read_lines(converted_afm)
.exec_ttf2afm = function(ttf, afm =fs::path_ext_set(ttf,"afm"), binary = .ttf2afm_binary(), overwrite=FALSE, family=NULL) {

  #TODO: fall back to Rttf2pt1 when no binary
  if (fs::file_exists(afm) && !overwrite) return(afm)
  fs::dir_create(fs::path_dir(afm))
  if (is.null(binary)) return(NA_character_)
  tmp = suppressWarnings(system2(
    binary,
    c(shQuote(ttf), "-o", shQuote(afm)),
    stdout = NULL, stderr = NULL,timeout = 1))
  if (tmp != 0) fs::file_delete(afm)
  if (!fs::file_exists(afm)) return(NA_character_)

  if (is.null(family)) {
    tmp = .get_afm_meta(afm)
    family = tmp$familyname
  }

  .set_afm_meta(afm, familyname = family)

  return(afm)
}

#' @noRd
#' @examples
#' ttf = systemfonts::system_fonts() %>% dplyr::filter(family == "Carlito") %>% dplyr::pull(path) %>% utils::head(1)
#' afm = fs::path(tempdir(),fs::path_file(ttf)) %>% fs::path_ext_set("afm")
#' converted_afm = .exec_ttf2afm(ttf, afm, overwrite=TRUE)
#' .get_afm_meta(converted_afm)
#' .get_afm_meta(fs::dir_ls(extrafont:::metrics_path(),glob = "*.afm.gz"))
.get_afm_meta = function(afm) {
  tmp = readr::read_lines(afm, progress = FALSE)
  tibble::tibble(
    fontname = stringr::str_remove(tmp[stringr::str_starts(tmp,"FontName ")],"FontName "),
    fullname = stringr::str_remove(tmp[stringr::str_starts(tmp,"FullName ")],"FullName "),
    familyname = stringr::str_remove(tmp[stringr::str_starts(tmp,"FamilyName ")],"FamilyName "),
    weight = stringr::str_remove(tmp[stringr::str_starts(tmp,"Weight ")],"Weight "),
    italicangle = stringr::str_remove(tmp[stringr::str_starts(tmp,"ItalicAngle ")],"ItalicAngle ")
  )
}

#' @noRd
#' @examples
#' .set_afm_meta(converted_afm, familyname="NEW")
#' .get_afm_meta(converted_afm)
.set_afm_meta = function(afm, fontname = NULL, fullname = NULL, familyname = NULL, weight = NULL, italicangle = NULL) {
  if (!fs::file_exists(afm)) return(NA_character_)
  tmp = readr::read_lines(afm)
  # strip brackets
  # tmp = stringr::str_replace_all(tmp, "\\(|\\)", "")
  # tmp = stringr::str_replace_all(tmp, "^(Version [^;]*);.*$", "\\1")
  tmp = tmp[!stringr::str_starts(tmp,"Notice ")]
  tmp = tmp[!stringr::str_starts(tmp,"Version ")]

  if (!is.null(fontname)) tmp[stringr::str_starts(tmp,"FontName ")] = sprintf("FontName %s",fontname)
  if (!is.null(fullname)) tmp[stringr::str_starts(tmp,"FullName ")] = sprintf("FullName %s",fullname)
  if (!is.null(familyname)) tmp[stringr::str_starts(tmp,"FamilyName ")] = sprintf("FamilyName %s",familyname)
  if (!is.null(weight)) tmp[stringr::str_starts(tmp,"Weight ")] = sprintf("Weight %s",weight)
  if (!is.null(italicangle)) tmp[stringr::str_starts(tmp,"ItalicAngle ")] = sprintf("ItalicAngle %s",italicangle)
  readr::write_lines(tmp,file = afm,append = FALSE)
  return(afm)
}

