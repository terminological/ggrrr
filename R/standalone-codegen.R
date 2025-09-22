# ---
# repo: terminological/ggrrr
# file: standalone-codegen.R
# last-updated: '2025-09-22'
# license: https://unlicense.org
# imports:
# - dplyr
# - glue
# - rlang
# - stringr
# - testthat
# - utils
# - whisker
# ---

#' Create a skeleton based on a template
#'
#' @inheritParams .update_fenced_block
#'
#' @returns A character vector of the file skeleton. does not write the file
#' @keywords internal
.create_skeleton = function(
  .template = NULL,
  ...,
  .insert_before = "# insert before ----"
) {
  if (is.null(.template)) {
    return(.insert_before)
  } else {
    tmpl_list = rlang::list2(...)
    tmpl_list = tmpl_list[names(tmpl_list) != ""]
    template_content = paste0(.template, collapse = "\n")
    new_content = whisker::whisker.render(template_content, tmpl_list)
    new_content = unlist(stringr::str_split(new_content, "\\n"))

    if (
      !any(stringr::str_detect(new_content, stringr::fixed(.insert_before)))
    ) {
      new_content = c(new_content, .insert_before)
    }
    return(new_content)
  }
}


#' Update a fenced block within a file with new content based on a template
#'
#' This function looks for a fenced section (typically of code) with markers defined by
#' `.start_glue` and `.end_glue` and replaces them with the evaluated template
#' content. If no matching sections are found the new code is inserted before the
#' matched `.insert_before` marker (or at the end if that is not found)
#'
#' @param .input the input as a character vector
#' @param .template the whisker template as a character vector
#' @param name a name for the block
#' @param ... additional named parameters supplied to the whisker template
#' @param .start_glue a glue specification for the marker of the start of the
#'   block to be replaced. May use any of the whisker variables e.g. `"# {name}
#'   start ----"`. This can also be given as a numeric index, were particularly
#'   0 is the beginning of the input and -1 is the end. If numeric it is start
#'   inclusive.
#' @param .end_glue a glue specification for the marker of the end of the
#'   block to be replaced. May use any of the whisker variables e.g.
#'   `"# {name} end ----"` or numeric as above. If numeric it is end inclusive.
#' @param .insert_before a marker for where in the file to insert a block if no
#'   matching start and end markers are found.
#' @param .parse_fn a function that takes the matching block in the source and
#'   extracts a named list of data from it. The default returns an empty list.
#'
#' @returns A character vector of the updated content of the file. Does not
#'   write the file
#' @keywords internal
#'
#' @unit
#' # double braces in mustache files, single braces in glue specs.
#' template = "# {{name}} {{status}}"
#'
#' input = c(
#'   "before",
#'   "#TEST test start ----",
#'   "# test original",
#'   "#TEST test end ----",
#'   "#TEST insert before ----",
#'   "end"
#' )
#'
#' input = .update_fenced_block(
#'   input, template, name = "test", status = "update",
#'   .start_glue = "#TEST {name} start ----",
#'   .end_glue = "#TEST {name} end ----",
#'   .insert_before = "#TEST insert before ----",
#' )
#'
#' input = .update_fenced_block(
#'   input, template, name = "test2", status = "update",
#'   .start_glue = "#TEST {name} start ----",
#'   .end_glue = "#TEST {name} end ----",
#'   .insert_before = "#TEST insert before ----",
#' )
#'
#' .update_fenced_block(
#'   input, template, name = "test", status = "update",
#'   .start_glue = 2,
#'   .end_glue = "#TEST {name} end ----",
#' )
#'
#'
#'
.update_fenced_block = function(
  .input,
  .template,
  name,
  ...,
  .start_glue = "# {name} start ----",
  .end_glue = "# {name} end ----",
  .insert_before = "# insert before ----",
  .parse_fn = ~ list()
) {
  template_content = paste0(.template, collapse = "\n")

  .parse_fn = rlang::as_function(.parse_fn)

  tmpl_list = rlang::list2(...)
  tmpl_list$name = name
  tmpl_list = tmpl_list[names(tmpl_list) != ""]

  input_lines = unlist(stringr::str_split(.input, "\\n"))

  if (is.numeric(.start_glue)) {
    # Allow start and end to be given as indices.
    # The start is zero, not one.
    start_loc = .start_glue
    start = input_lines[start_loc]
    if (all(start_loc < 0)) start_loc = start_loc + length(.input)
  } else {
    start = glue::glue_data(tmpl_list, .start_glue)
    start_loc = which(stringr::str_detect(input_lines, start))
  }

  if (.start_glue == .end_glue) {
    # Deal with identical start and end fences.
    start_loc = start_loc[seq_along(start_loc) %% 2 == 1]
    end_loc = start_loc[seq_along(start_loc) %% 2 == 0]
  } else {
    if (is.numeric(.end_glue)) {
      # end is given as a numeric.
      # the last line is -1, it is end inclusive.
      end_loc = .end_glue
      end = input_lines[end_loc]
      if (all(end_loc < 0)) end_loc = end_loc + length(.input) + 1
    } else {
      end = glue::glue_data(tmpl_list, .end_glue)
      end_loc = which(stringr::str_detect(input_lines, end))
    }
  }

  if (length(start_loc) == 0 || length(end_loc) == 0) {
    # markers not detected. file maybe empty or markers not present
    insert_loc = which(stringr::str_detect(
      input_lines,
      stringr::fixed(.insert_before)
    ))
    # insert_loc maybe a zero length vector
    start_loc = min(c(length(input_lines), insert_loc)) # insert before (or at end if not found)
    end_loc = min(c(length(input_lines), insert_loc - 1))
  } else if (
    length(start_loc) != 1 || length(end_loc) != 1 || end_loc <= start_loc
  ) {
    stop(
      "Mismatched markers were found between:\n'",
      start,
      "' and \n'",
      end,
      "' in file:\n",
      .input,
      "\nIt is in an inconsistent state and will need manual editing."
    )
  }

  if (start_loc > 0) {
    input_leading = input_lines[seq_len(start_loc - 1)]
    indent = unlist(stringr::str_extract(input_lines[start_loc], "^\\s*"))
    input_matched = input_lines[start_loc:end_loc]
  } else {
    # empty file
    input_leading = character()
    indent = ""
    input_matched = input_lines[seq_len(end_loc)]
  }
  # end_loc maybe zero if at start of file.
  input_trailing = input_lines[-1 * seq_len(end_loc)]

  # Parse existing content and add to template input:
  parsed = .parse_fn(trimws(input_matched))
  tmpl_list = modifyList(parsed, tmpl_list)

  new_block = whisker::whisker.render(template_content, tmpl_list)
  new_block_lines = unlist(stringr::str_split(new_block, "\\n"))

  # top and tail new content and indent
  new_block_lines = paste0(indent, c(start, new_block_lines, end))

  return(c(input_leading, new_block_lines, input_trailing))
}


#' Deletes content between markers in an input
#'
#' This function deletes anything between matching markers in an input vector
#' including the markers themselves. This will match multiple markers if present
#' and paired
#'
#' @inheritParams .update_fenced_block
#'
#' @returns a character vector with the deleted blocks
#' @keywords internal
#'
#' @unit
#'
#' tmp = .delete_fenced_block(letters,.start_glue = letters[4], .end_glue = letters[10])
#' testthat::expect_true(!any(letters[4:10] %in% tmp))
#'
#' # multiple copies of fenced block are removed
#' # fenced block to end of file is removed
#' testthat::expect_equal(
#'   .delete_fenced_block(rep(letters,3),.start_glue = "b", .end_glue = "z"),
#'   rep("a",3)
#' )
#'
#' # fenced block at beginning is removed:
#' testthat::expect_equal(
#'   .delete_fenced_block(letters,.start_glue = "a", .end_glue = "y"),
#'   "z"
#' )
#'
#' # fenced block
#' testthat::expect_equal(
#'   .delete_fenced_block(letters,.start_glue = "a", .end_glue = "z"),
#'   character()
#' )
#'
#' testthat::expect_equal(
#'   .delete_fenced_block(letters,.start_glue = 2, .end_glue = -2),
#'   c("a","z")
#' )
.delete_fenced_block = function(
  .input,
  name,
  ...,
  .start_glue = "# {name} start ----",
  .end_glue = "# {name} end ----"
) {
  tmpl_list = rlang::list2(...)
  if (!missing(name)) {
    tmpl_list$name = name
  }

  input_lines = unlist(stringr::str_split(.input, "\\n"))

  if (is.numeric(.start_glue)) {
    # Allow start and end to be given as indices.
    # The start is zero, not one.
    start_loc = .start_glue
    if (all(start_loc < 0)) start_loc = start_loc + length(.input)
  } else {
    start = glue::glue_data(tmpl_list, .start_glue)
    start_loc = which(stringr::str_detect(input_lines, start))
  }

  if (.start_glue == .end_glue) {
    # Deal with identical start and end fences.
    start_loc = start_loc[seq_along(start_loc) %% 2 == 1]
    end_loc = start_loc[seq_along(start_loc) %% 2 == 0]
  } else {
    if (is.numeric(.end_glue)) {
      # end is given as a numeric.
      # the last line is -1, it is end inclusive.
      end_loc = .end_glue
      if (all(end_loc < 0)) end_loc = end_loc + length(.input) + 1
    } else {
      end = glue::glue_data(tmpl_list, .end_glue)
      end_loc = which(stringr::str_detect(input_lines, end))
    }
  }

  if (length(start_loc) == 0 || length(end_loc) == 0) {
    return()
  }

  if (
    length(start_loc) != length(end_loc) ||
      any(end_loc <= start_loc) ||
      any(end_loc >= c(start_loc[-1], Inf))
  ) {
    stop(
      "Mismatched markers were found between:\n'",
      start,
      "' and \n'",
      end,
      "' in file:\n",
      .input,
      "\nIt is in an inconsistent state and will need manual editing."
    )
  }

  select = unlist(lapply(seq_along(start_loc), function(i) {
    -1 * start_loc[i]:end_loc[i]
  }))
  return(input_lines[select])
}

#' Extract data from the output of a glue call
#'
#' This function allow for the reverse of the `glue::glue_data()` operation
#' and regenerates the data as a dataframe. It won't work for very complex glue
#' specifications and will only return strings.
#'
#' @param glue_spec a glue spec with braces determining named substitutes
#' @param output the glue output
#'
#' @returns a dataframe with a column per piece which could have been used in
#'   `glue::glue_data(.x,glue_spec)` apart from leading white space.
#' @keywords internal
#'
#' @unit
#' glue_spec = "# {name}, {value1} and {value2} ----"
#' output = rep("  # one, two and three ----",5)
#' tmp = .recover_pieces(glue_spec,output)
#'
#' testthat::expect_equal(nrow(tmp), 5)
#' testthat::expect_equal(trimws(output), glue::glue_data(tmp,glue_spec))
#'
.glue_recover_pieces = function(glue_spec, output) {
  names = unlist(stringr::str_extract_all(glue_spec, "(?<=\\{)[^\\}]*(?=\\})"))
  matcher = stringr::str_replace_all(glue_spec, "\\{[^\\}]*\\}", "(.*?)")
  matcher = paste0(c("^", matcher, "$"), collapse = "")
  values = stringr::str_match_all(trimws(output), matcher)
  out = dplyr::tibble(.rows = length(output))
  for (i in seq_along(names)) {
    out[[names[i]]] = sapply(values, function(value) value[1, i + 1])
  }
  return(out)
}


#' Detect glue outputs
#'
#' This allows you to filter for lines that match those that could have been
#' generated with a given glue spec
#'
#' @param glue_spec a glue spec with braces determining named substitutes
#' @param content a character vector
#'
#' @returns a logical vector with true if the character could have been created
#'   by the glue spec.
#' @keywords internal
#'
#' @unit
#' content = c("sdlfksl","  # X, Y and Z ----","sdfskjfsdlk asdlkfjsdlkj")
#' testthat::expect_equal(
#'   .glue_detect_output("# {name}, {value1} and {value2} ----", content),
#'   c(FALSE,TRUE,FALSE)
#' )
.glue_detect_output = function(glue_spec, content) {
  matcher = stringr::str_replace_all(glue_spec, "\\{[^\\}]*\\}", "(.*?)")
  matcher = paste0(c("^", matcher, "$"), collapse = "")
  stringr::str_detect(trimws(content), matcher)
}
