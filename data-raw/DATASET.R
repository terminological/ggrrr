## code to prepare `DATASET` dataset goes here

ftmp = tempfile()
download.file(
  "https://raw.githubusercontent.com/adobe-type-tools/agl-aglfn/refs/heads/master/glyphlist.txt",
  ftmp
)
lines = readr::read_lines(ftmp)
lines = lines[!grepl("^#", lines)]
tmp = strsplit(lines, split = ";")
glyphmap = dplyr::tibble(
  agl = sapply(tmp, `[`, 1),
  unicode = sapply(tmp, `[`, 2),
)

usethis::use_data(glyphmap, overwrite = TRUE)
