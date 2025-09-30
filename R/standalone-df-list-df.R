# ---
# repo: terminological/ggrrr
# file: standalone-df-list-df.R
# last-updated: 2025-09-30
# license: https://unlicense.org
# imports:
# - dplyr
# - purrr
# - testthat
# - tidyr
# ---

#' Convert a nested dataframe to a multilevel list
#'
#' @param df a nested dataframe
#' @inheritDotParams .transpose
#'
#' @returns a list of lists
#' @keywords internal
#' @concept transpose
#'
#' @unit
#'
#' iris_list = .df_to_list_of_lists(datasets::iris)
#' # TODO: iris_list has lost Petal.Length as it is interpreting Petal.Width as
#' # nested item and it overwrites Petal.Length rather than merging with it.
#'
#' testthat::expect_equal(
#'   iris_list[[1]]$Species,
#'   iris$Species[[1]]
#' )
#'
#' mtcars_nest = datasets::mtcars %>%
#'   dplyr::mutate(name = rownames(.)) %>%
#'   tidyr::nest(details = -c(cyl,gear))
#'
#' mtcars_list = mtcars_nest %>% .df_to_list_of_lists()
#'
#' mtcars_unnest = mtcars_list %>% .list_of_lists_to_df()
#'
#' testthat::expect_equal(
#'   mtcars_list[[1]]$details[[1]]$name,
#'   mtcars_nest$details[[1]]$name[[1]]
#' )
#'
.df_to_list_of_lists = function(df, ...) {
  .transpose.data.frame(df, ...)
}

#' Convert a multilevel list to a nested dataframe
#'
#' @param list a multilevel list
#' @inheritDotParams .transpose
#'
#' @returns a dataframe with each sublist nested as a dataframe
#' @keywords internal
#' @concept transpose
#'
#' @unit
#' iris_list = .df_to_list_of_lists(iris, .fix=FALSE)
#' iris2 = .list_of_lists_to_df(iris_list, .fix=FALSE)
#'
#' testthat::expect_equal(datasets::iris, as.data.frame(iris2))
#'
#' mtcars_nest = datasets::mtcars %>%
#'   dplyr::mutate(name = rownames(.)) %>%
#'   tidyr::nest(details = -c(cyl,gear))
#'
#' mtcars_list = mtcars_nest %>% .df_to_list_of_lists()
#'
#' mtcars_nest2 = mtcars_list %>% .list_of_lists_to_df()
#'
#' testthat::expect_equal(
#'   mtcars_nest2$details[[2]],
#'   mtcars_nest$details[[2]]
#' )
#'
#' # test unequal length vector column is mapped to list of vectors
#' # and multiply named nests are treated as rows
#' testlist = list(
#'    row = list(a=1:5, b="x"),
#'    row = list(a=2:4, b="y"),
#'    row = list(a=3, b="z")
#' )
#' testdf = testlist %>% .list_of_lists_to_df()
#' testthat::expect_equal(testdf$b, c("x", "y", "z"))
#' testthat::expect_equal(testdf$a[[2]], 2:4)
#'
.list_of_lists_to_df = function(lst, ...) {
  .transpose.list(lst, ...)
}

#' Transform a nested dataframe to / from a row by row list
#'
#' Data frames are column lists, which may have nested dataframes. This function
#' transforms a data frame to row based list with named sub lists with one entry
#' per dataframe column (a `row_list`). It alternative converts a `row_list` back
#' to a nested data frame
#'
#' @param x a `data.frame` or `row_list`
#' @param .fix collapse or expand names in redundant multi-level `row_list`s.
#'   Either `FALSE` or a string to join or split the names of the multi-level
#'   list by
#' @param ... not used
#' @keywords internal
#' @concept transpose
#'
#' @unit
#'
#' # create a test nested data frame:
#'
#' mtcars_nest = datasets::mtcars %>%
#'   dplyr::mutate(name = rownames(.)) %>%
#'   tidyr::nest(by_carb = -c(cyl,gear,carb)) %>%
#'   tidyr::nest(by_cyl_and_gear = -c(cyl,gear))
#'
#' mtcars_list = mtcars_nest %>% .transpose()
#'
#' mtcars_nest2 = mtcars_list %>% .transpose()
#'
#' testthat::expect_equal(mtcars_nest, mtcars_nest2)
.transpose = function(x, ..., .fix = ".") {
  UseMethod(".transpose", x)
}

#' @unit
#' tmp = tibble::tibble(a=1:3, b=letters[1:3])
#' tmp2 = .transpose.data.frame(tmp)
#' wrapped = lapply(1:5, \(i) list(a=list(b=list(c=i)), z = letters[i]))
#' df= .transpose.list(wrapped)
#' unwrap = .transpose(df)
#' @export
.transpose.list = function(x, ..., .fix = ".") {
  if (!..df2_is_transposable(x, "df")) {
    return(x)
  }
  # a list.named_list which we need to convert to a named_list.list
  new_names = unique(unlist(lapply(x, names)))
  out = list()
  for (name in new_names) {
    # extract the name value from x_i,name only to tmp_i
    tmp = lapply(x, `[[`, name) # will create nulls
    if (!isFALSE(.fix)) {
      tmp2 = ..df2_pull_up(tmp, name, sep = .fix)
      name = tmp2$new_name
      tmp = tmp2$data
    }

    # tmp will be list of atomics or non length 1 lists or data frames
    # if everything is a atomic or NULL or length zero. Fix NULLs and length zero to be NA
    # and unlist:
    if (..df2_is_simplifiable(tmp)) {
      tmp = ..df2_simplify(tmp)
    }
    # browser()
    # recurse
    if (
      is.list(tmp) && all(sapply(tmp, ..df2_is_transposable, target = "df"))
    ) {
      # do the recursion
      tmp = lapply(tmp, .transpose.list)
    }

    out[[name]] = unname(tmp)
  }
  return(structure(
    out,
    row.names = if (length(out) == 0) integer() else seq_along(out[[1]]),
    class = c("tbl_df", "tbl", "data.frame")
  ))
}

#' @unit
#' tmp = tibble::tibble(a=1:3, b=letters[1:3])
#' tmp2 = .transpose.data.frame(tmp)
#' tmp3 = tibble::tibble(x.y.z = 1:3, b=letters[1:3])
#' tmp4 = .transpose.data.frame(tmp)
#' @export
.transpose.data.frame = function(x, ..., .fix = ".") {
  # target is a list.named_list from a named_list.list
  if (!..df2_is_transposable(x, "list")) {
    return(x)
  }
  new_names = names(x)
  # use the first column as template TODO: empty data frames?
  # N.B. creates a list of empty lists
  out = lapply(seq_along(x[[1]]), function(i) list())

  for (name in new_names) {
    # cycle through the named_list.list by column name
    # stick with the data frame list column format at the moment
    # each name here is a list which needs to be mapped to an
    # entry

    tmp = x[[name]]

    if (
      is.list(tmp) && all(sapply(tmp, ..df2_is_transposable, target = "list"))
    ) {
      # do the recursion
      tmp = lapply(tmp, .transpose.data.frame)
    }

    if (!isFALSE(.fix)) {
      tmp2 = ..df2_push_down(x = tmp, name = name, sep = .fix)
      tmp = tmp2$data
      # get the name that has resulted
      # pieces = unlist(strsplit(name, .fix, fixed = TRUE))
      name = tmp2$new_name
    }

    # set out_i...name to tmp_i
    for (i in seq_along(out)) {
      tmp2 = out[[i]]
      # skip NAs: null values => missing in list of lists. NAs not needed.
      if (!(is.atomic(tmp) && is.na(tmp[[i]]))) {
        if (is.list(tmp2[[name]])) {
          tmp2[[name]] = utils::modifyList(tmp2[[name]], tmp[[i]])
        } else {
          tmp2[[name]] = tmp[[i]]
        }
      }
      out[[i]] = tmp2
    }
  }
  return(structure(out, class = c("row_list", class(out))))
}

.transpose.generic = function(x, ...) {
  stop("Method applies only to data frames or list of lists")
}

..df2_is_simplifiable = function(x) {
  all(sapply(x, function(l) {
    is.null(l) || length(l) == 0 || (is.atomic(l) && length(l) == 1)
  }))
}

..df2_simplify = function(x) {
  unlist(lapply(x, function(l) {
    return(if (is.null(l) || length(l) == 0) NA else l)
  }))
}

# ..df2_is_namedlist(list())
..df2_is_namedlist = function(x) {
  # a named list without duplicated names
  # will match dataframes
  is.list(x) && !is.null(names(x)) && !anyDuplicated(names(x))
}

..df2_is_unique = function(x) {
  return(length(unique(x)) == 1)
}

# ..df2_is_unnamedlist(list())
..df2_is_unnamedlist = function(x) {
  # a list with either no names or all the same name
  is.list(x) && (is.null(names(x)) || ..df2_is_unique(names(x)))
}

..df2_is_transposable = function(x, target = c("df", "list")) {
  target = match.arg(target)
  if (target == "df") {
    return(
      # x is an unnamed or uniquely named list
      ..df2_is_unnamedlist(x) &&
        # and all elements of x are named lists
        all(sapply(x, ..df2_is_namedlist))
    )
  } else {
    return(
      # any named list with equal lengths can be converted to a list of named lists.
      # length zero named lists - i.e. dataframe with no columns?
      ..df2_is_namedlist(x) && ..df2_is_unique(sapply(x, length))
    )
  }
}

..df2_is_wrapped = function(x) {
  return(..df2_is_namedlist(x) && !is.data.frame(x) && length(x) == 1)
}

# x= list(sub1 = 1,sub2 = 2,sub3 = 3)
# names(x) = "test"

# collapses nested named lists to a single level combining names. Does not
# collapse unnamed lists
# tmp = ..df2_push_down(list(1,2,3), "a.b.c", ".") # should be list
# ..df2_pull_up(tmp$data, "a")
# wrapped = lapply(1:5, \(i) list(a=list(b=list(c=i))))
# ..df2_pull_up(wrapped)
..df2_pull_up = function(x, name = NULL, sep = ".") {
  if (!is.list(x) || is.data.frame(x)) {
    return(x)
  }
  while (
    all(sapply(x, ..df2_is_wrapped)) && length(unique(sapply(x, names))) == 1
  ) {
    name = paste0(c(name, unique(sapply(x, names))), collapse = sep)
    x = lapply(x, function(sub) sub[[1]])
  }
  return(list(data = x, new_name = name))
}


# ..df2_push_down(c(1,2,3), "a", ".") # should be unchanged
# ..df2_push_down(c(1,2,3), "a.b", ".") # should be list
# ..df2_push_down(list(1,2,3), "a.b.c", ".") # should be list
..df2_push_down = function(x, name, sep = ".") {
  pieces = unlist(strsplit(name, sep, fixed = TRUE))
  new_name = pieces[1]
  pieces = rev(pieces[-1])
  for (piece in pieces) {
    x = lapply(x, function(sub) {
      tmp = list()
      tmp[[piece]] = sub
      tmp
    })
  }
  return(list(data = x, new_name = new_name))
}


#' Print method for `row_list` objects
#'
#' @inheritParams format.row_list
#' @inheritDotParams format.row_list -x
#' @keywords internal
#' @concept transpose
print.row_list <- function(
  x,
  ...
) {
  cat(format.row_list(x, ...))
  invisible(x)
}


#' Format method for `row_list` objects
#'
#' @param x A `row_list` object
#' @param max_levels The maximum number of levels to show
#' @param ... Additional arguments:
#' - `max_length` the number of items in a vector to show before truncating.
#' - others are passed to `format(...)`
#' @keywords internal
#' @concept transpose
#'
format.row_list <- function(
  x,
  max_levels = 6,
  ...
) {
  tmp = ..df2_format_tree(x, level = max_levels, ...)
  tmp = c(
    sprintf("<row_list> (%d items)", length(x)),
    unlist(tmp)
  )
  tmp = paste0(tmp, collapse = "\n")
  return(tmp)
}

# values = list("1","2",list("3.1","3.2"),"4")
# values = 1
# ..df2_format_tree(values)
..df2_format_tree = function(
  values,
  level = 6,
  direction = "up",
  ...
) {
  values = purrr::map2(
    values,
    if (is.null(names(values))) rep(NA, length(values)) else names(values),
    ..df2_format_leaf,
    level = level,
    ...
  )

  if (length(values) == 0) {
    values = list("<empty>")
  }
  for (i in seq_along(values)) {
    if (length(values[[i]]) == 0) values[[i]] = "<empty>"
  }

  connectors = list(
    first = if (direction == "up") c("├─", "│ ") else c("┬─", "│ "),
    mid = c("├─", "│ "),
    end = if (direction == "left" && length(values) < 2) {
      c("──", "  ")
    } else {
      c("└─", "  ")
    }
  )

  lengths = sapply(values, length)

  first_prefixes = character()
  mid_prefixes = character()
  end_prefixes = character()

  end_len = lengths[[length(values)]]
  end_prefixes = c(connectors$end[1], rep(connectors$end[2], end_len - 1))

  if (length(values) > 1) {
    first_len = lengths[[1]]
    first_prefixes = c(
      connectors$first[1],
      rep(connectors$first[2], first_len - 1)
    )
  }

  if (length(values) > 2) {
    mid_lens = lengths[2:(length(values) - 1)]
    mid_prefixes =
      unlist(
        sapply(
          mid_lens,
          function(l) {
            c(connectors$mid[1], rep(connectors$mid[2], l - 1))
          },
          USE.NAMES = FALSE
        )
      )
  }

  prefixes = c(first_prefixes, mid_prefixes, end_prefixes)

  # return value of this function should be a character vector (not a list)
  return(paste0(prefixes, unlist(values)))
}

# prefix all the values of a list preserving list structure
# prefixes apply to head of the top level only, beyond top level
# or values after the first one are determined by cont or if it is the last item
# in the top level
# ..df2_prefix_nested = function(values, prefixes, cont = NULL) {
#   return(lapply(seq_along(values), function(i) {
#     v = values[[i]]
#     p = prefixes[[i]]
#     if (is.null(cont)) {
#       if (i == length(prefixes)) {
#         cont = "  "
#       } else {
#         cont = "│ "
#       }
#     }
#     if (is.list(v)) {
#       v2 = ..df2_prefix_nested(v, rep(cont, length(v)), cont)
#     } else {
#       v2 = sprintf("%s%s", cont, v)
#     }
#     if (is.list(v[[1]])) {
#       v2[[1]] = ..df2_prefix_nested(
#         v[[1]],
#         c(p, rep(cont, length(v[[1]]) - 1)),
#         cont
#       )
#     } else {
#       v2[[1]] = sprintf("%s%s", p, v[[1]])
#     }
#     v2
#   }))
# }

..df2_format_leaf = function(
  value,
  name,
  level,
  ...,
  max_length = 4
) {
  if (max_length < 2) {
    max_length = 2
  }

  is_named = !is.na(name)
  nm_prefix = if (is_named) sprintf("$%s:", name) else ""

  vec_shorten = function(x) {
    if (length(x) > max_length) {
      x = c(x[1:(max_length - 1)], "...", x[length(x)])
    }
    paste0(x, collapse = ",")
  }

  nm_fn = function(x) sprintf("%s %s", nm_prefix, vec_shorten(x))

  # nulls
  if (is.null(value)) {
    return(nm_fn("<null>"))
  }

  # nested dfs
  if (inherits(value, "data.frame")) {
    out = sprintf(
      "<dataframe> (%d rows: %s)",
      nrow(value),
      vec_shorten(colnames(value))
    )
  }

  # nested dfs
  if (inherits(value, "matrix")) {
    out = sprintf(
      "<dataframe> (%d rows: %d cols)",
      nrow(value),
      ncol(value)
    )
  }

  # sublists
  if (is.list(value)) {
    if (length(value) == 0) {
      return(nm_fn("<empty list>"))
    }

    if (level <= 0) {
      if (..df2_is_unnamedlist(value)) {
        return(nm_fn(sprintf("<list> (%d items)", length(value))))
      } else {
        return(nm_fn(sprintf("<row> (%s)", vec_shorten(names(value)))))
      }
    }

    tmp = ..df2_format_tree(
      value,
      level = level - 1,
      direction = if (is_named) "up" else "left",
      ...
    )

    if (is_named) {
      return(c(nm_prefix, tmp))
    } else {
      return(tmp)
    }
  }

  # regular values
  if (is.character(value)) {
    value = paste0('"', value, '"')
  }
  value = format(value, ...)
  if (length(value) > 1) {
    value = sprintf("[%s]", vec_shorten(value))
  }
  return(nm_fn(value))
}
