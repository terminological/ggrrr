# ---
# repo: terminological/ggrrr
# file: standalone-tidy-utils.R
# last-updated: '2024-06-12'
# license: https://unlicense.org
# imports:
# - dplyr
# - ggplot2
# - glue
# - purrr
# - rlang
# - stats
# - tibble
# - tidyr
# - tidyselect
# - utils
# ---

## Summarisation helpers ----

#' Bind rows for colums with factors
#'
#' Bind_rows works until there are factors with a set of different levels then it throws a
#' wobbly. This handles that particular situation by combining factor levels.
#'
#' @param ... a list of dataframes
#'
#' @return the union of those dataframes. Factor levels are combined with a superset of all levels
#' @export
#'
#' @examples
#' library(tidyverse)
#' bind_rows_with_factors(iris,
#'  ggplot2::diamonds %>% dplyr::rename(Species = cut)) %>%
#'  dplyr::pull(Species) %>%
#'  levels()
bind_rows_with_factors <- function(...) {
  name = value = NULL # remove global binding note

  # Identify all factors, and all their levels and
  dots = rlang::list2(...)
  factors = unique(unlist(purrr::map(
    dots,
    ~ .x %>%
      dplyr::ungroup() %>%
      dplyr::select(tidyselect::where(is.factor)) %>%
      colnames()
  )))
  factorLevels = dplyr::bind_rows(purrr::map(
    dots,
    ~ purrr::map(
      .x %>% dplyr::ungroup() %>% dplyr::select(tidyselect::where(is.factor)),
      ~ .x %>% levels()
    ) %>%
      tibble::enframe() %>%
      tidyr::unnest(c(value))
  ))
  # convert factors to character, bind dataframes, convert characters back to factors
  dots2 = purrr::map(
    dots,
    ~ .x %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.factor), as.character))
  )
  out = dplyr::bind_rows(dots2)
  for (col in factors) {
    # combine the factors. This will tend to keep the order of the levels the first time a factor is encountered
    l = factorLevels %>%
      dplyr::filter(name == col) %>%
      dplyr::pull(value) %>%
      unique()
    out[[col]] = factor(out[[col]], levels = l)
  }
  return(out)
}


#' Summarise a subgroup and create a summary row
#'
#' Summarise and include a total row, or a row including the summary for the whole group, into a factor list.
#' This looks and feels like a natural summarisation step, but applies the summarisation both to the
#' subgroups and to the data ungrouped by one level. The additional group result is included as a new row.
#' allows for a natural grouped and ungrouped summarisation
#'
#' @param .data a dataframe
#' @param ... the summarisation specification
#' @param .groups what to do with the grouping after summarisation (same as dplyr::summarise)
#' @param .total name of the total row which will be added into a factor list.
#' @param .total_first should the total be before or after the groups
#'
#' @return a summarised dataframe with the additional totals or group row
#' @export
#'
#' @examples
#' library(tidyverse)
#' diamonds %>%
#'  dplyr::group_by(color,cut) %>%
#'  summarise_with_totals(
#'     mpg = sprintf("%1.1f \u00B1 %1.1f", mean(price), stats::sd(price)),
#'     .total = "Overall"
#'  )
summarise_with_totals = function(
  .data,
  ...,
  .groups = NULL,
  .total = "Total",
  .total_first = FALSE
) {
  grps = .data %>% dplyr::groups()
  last_col = (utils::tail(grps, 1)[[1]])
  first_level = .data %>% dplyr::summarise(..., .groups = .groups)
  grps_out = first_level %>% dplyr::groups()
  totals = .data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!!(grps %>% utils::head(-1))) %>%
    dplyr::summarise(..., .groups = .groups)

  if (.data %>% dplyr::pull(!!last_col) %>% is.factor()) {
    if (.data %>% dplyr::pull(!!last_col) %>% is.ordered()) {
      totals = totals %>% dplyr::mutate(!!last_col := .total %>% ordered())
    } else {
      totals = totals %>% dplyr::mutate(!!last_col := .total %>% factor())
    }
    if (.total_first) {
      out = bind_rows_with_factors(totals, first_level)
    } else {
      out = bind_rows_with_factors(first_level, totals)
    }
  } else {
    totals = totals %>% dplyr::mutate(!!last_col := .total %>% factor())
    first_level = first_level %>%
      dplyr::mutate(!!last_col := as.character(!!last_col))
    if (.total_first) {
      out = dplyr::bind_rows(totals, first_level)
    } else {
      out = dplyr::bind_rows(first_level, totals)
    }
  }
  return(out %>% dplyr::group_by(!!!grps_out))
}

#' Create a dataframe with groups mathing a range of predicates
#'
#' Create a new data frame including duplicate rows where the rows fulfil a potentially overlapping set of conditions
#' specified as named predicates (as formulae)
#'
#' @param .data a data frame
#' @param ... a set of predicates specified like case_whens syntax, such as mpg < 5 ~ "gas guzzlers"
#' @param .colname the name of the new group
#'
#' @return a new dataframe containing the overlapping groups which may create duplicates of individual rows.
#' @export
#'
#' @examples
#' library(tidyverse)
#' iris %>% dplyr::group_by(Species) %>% intersecting_group_by(
#'   Sepal.Length > mean(Sepal.Length) ~ "Long",
#'   Sepal.Width > mean(Sepal.Width) ~ "Wide"
#' )
intersecting_group_by = function(.data, ..., .colname) {
  UseMethod("intersecting_group_by", .data)
}

#' @export
intersecting_group_by.default = function(.data, ..., .colname) {
  stop(
    "Only dataframes and tracked dataframes are supported by intersecting_group_by"
  )
}

#' @export
intersecting_group_by.data.frame = function(.data, ..., .colname = "group") {
  .colname = rlang::ensym(.colname)
  grps = .data %>% dplyr::groups()
  dots = rlang::list2(...)
  dplyr::bind_rows(lapply(dots, function(form) {
    value = rlang::f_rhs(form)
    predicate = rlang::f_lhs(form)
    .data %>% dplyr::filter(!!predicate) %>% dplyr::mutate(!!.colname := value)
  })) %>%
    dplyr::group_by(!!!grps, !!.colname)
}

#' @export
intersecting_group_by.trackr_df = function(
  .data,
  ...,
  .colname = "group",
  .headline = "Create overlapping subsets",
  .messages = "{.count.out} items"
) {
  .colname = rlang::ensym(.colname)
  grps = .data %>% dplyr::groups()

  # wrap this is a group modify for tracked dataframes.
  .data %>%
    dplyr::ungroup(.messages = "") %>%
    dplyr::group_modify(
      function(d, g, ...) {
        dots = rlang::list2(...)
        dplyr::bind_rows(lapply(dots, function(form) {
          value = rlang::f_rhs(form)
          predicate = rlang::f_lhs(form)
          d %>%
            dplyr::filter(!!predicate) %>%
            dplyr::mutate(!!.colname := value)
        }))
      },
      ...,
      .headline = .headline,
      .messages = .messages
    ) %>%
    dplyr::group_by(!!!grps, !!.colname, .messages = "")
}

# .maybe = function(expr) {
#   expr = rlang::enexpr(expr)
#   browser()
#   #data = .search_call_stack()
#   tryCatch(rlang::eval_tidy(expr,tidyselect::peek_data()), error = function(e) {
#     warning(e$message)
#     NA}
#   )
# }

# TODO:
# fct_set_na = function(x, predicateExpr) {
#   predicateExpr = rlang::enexpr(predicateExpr)
#   tmp = ifelse(!!predicateExpr, NA, as.character(x))
#   factor(tmp, levels=levels(x), ordered = is.ordered(x))
# }

## Rowwise mutate

#' Create new data in a strictly row-wise fashion without vectorisation
#'
#' Applies an expression to each row and assignes it to a new column.
#' Per-row failures are handled with default values (NAs) or can be intercepted
#' by the user with a tryCatch(...) expression. There are many other ways to
#' do a similar thing in `dplyr` and `purrr` but they are all more complicated than
#' I expect them to be.
#'
#' @param .data a dataframe. grouping is ingnored
#' @param ... a named list of expressions similar to mutate but where the expressions
#'   to be evaluated are evaluated in only in the context of the current row - and
#'   are not vectorised. This does not support [dplyr::accross] syntax.
#' @param .onerror a function that is called for
#'
#' @return a dataframe the same length as input with additional or altered columns
#' @export
#'
#' @examples
#' # calculations are scoped only to current row. Hence max(x) == x always:
#' iris %>% rowwise_mutate(
#'   widths = Sepal.Width+max(Petal.Width),
#'   lengths = Sepal.Length+max(Petal.Length),
#'   tmp = tibble::tibble(a=1, b=2)) %>%
#' dplyr::glimpse()
#'
#' # This is different to standard dplyr behaviour when the additional tibble
#' # column is considered. standard dplyr rowwise does something unexpected:
#' iris %>% dplyr::rowwise() %>% dplyr::mutate(
#'   widths = Sepal.Width+max(Petal.Width),
#'   lengths = Sepal.Length+max(Petal.Length),
#'   tmp = tibble::tibble(a=1, b=2)) %>%
#' dplyr::glimpse()
#'
#' # As expressions are not vectorised we can use normal if ... else ... statements
#' # and errors can be handled and default values provided.
#' suppressWarnings(
#' iris %>% rowwise_mutate(
#'   tmp = if (Petal.Width > 2.0) stop("error message: ",Petal.Width) else Petal.Width,
#'   .onerror = function(e) -Petal.Width
#' ) %>%
#' dplyr::glimpse()
#' )
#'
#' # The default values
#' # are evaluated in the same context as the original expression, but only are
#' # defaults for all the columns so makes most sense when a default value is given
#'
#' suppressWarnings(
#' iris %>% rowwise_mutate(
#'   tmp = if (Petal.Width > 2.0) stop("too wide petals: ",Petal.Width) else Petal.Width,
#'   tmp2 = if (Sepal.Width > 4) stop("too wide sepals: ",Sepal.Width) else Sepal.Width,
#'   .onerror = function(e) Inf
#' ) %>%
#' dplyr::glimpse()
#' )
rowwise_mutate = function(.data, ..., .onerror = function(e, ...) NA) {
  out = .data
  list = rlang::enexprs(...)
  env = rlang::caller_env()
  tmp = lapply(names(list), function(x) vector("list", nrow(out)))
  # var = names(list)[1]
  errors = c()

  for (i in 1:nrow(out)) {
    input = lapply(.data, `[[`, i)
    env = list2env(input, envir = env)

    for (var in names(list)) {
      expr = list[[var]]
      result = tryCatch(
        rlang::eval_bare(expr, env = env),
        error = function(e, ...) {
          errors <<- unique(c(
            errors,
            sprintf("`%s`: evaluating %s", e$message, var)
          ))
          .onerror(e, ...)
        }
      )
      tmp[[var]][[i]] = result
    }
  }

  for (var in names(list)) {
    tmp2 = unlist(tmp[[var]])
    if (
      # unlisting worked
      is.null(names(tmp2)) && length(tmp2) == nrow(out)
    ) {
      out[[var]] = tmp2
    } else {
      # stick with a list column
      out[[var]] = tmp[[var]]
    }
  }

  if (length(errors) > 0) {
    warning(
      "errors occurred (max 10 shown):\n",
      paste0(utils::head(errors, 10), collapse = "\n")
    )
  }

  return(out)
}


# wrap functions that chuck an error
.opt = function(expr) {
  tryCatch(expr, error = function(e) NA_real_)
}


#' Cut and label an integer valued quantity
#'
#' Deals with some annoying issues classifying integer data sets, such as ages, into groups. where you want to
#' specify just the change over points as integers and clearly label the resulting ordered factor.
#'
#' @param x a vector of integer valued numbers, e.g. ages, counts
#' @param cut_points a vector of integer valued cut points which define the lower boundaries of conditions
#' @param glue a glue spec that may be used to generate a label. It can use \{low\}, \{high\}, \{next_low\}, or \{label\} as values.
#' @param lower_limit the minimum value we should include (this is inclusive for the bottom category) (default -Inf)
#' @param upper_limit the maximum value we should include (this is also inclusive for the top category) (default Inf)
#' @param ... not used
#'
#' @return an ordered factor of the integer
#' @export
#'
#' @examples
#' cut_integer(stats::rbinom(20,20,0.5), c(5,10,15))
#' cut_integer(floor(stats::runif(100,-10,10)), cut_points = c(2,3,4,6), lower_limit=0, upper_limit=10)
cut_integer = function(
  x,
  cut_points,
  glue = "{label}",
  lower_limit = -Inf,
  upper_limit = Inf,
  ...
) {
  next_low = NULL # remove global binding note

  if (!all(as.integer(x) == x, na.rm = TRUE)) {
    warning("input to cut_integer(...) has been coerced to integer values")
  }
  x = floor(x)
  if (!all(as.integer(cut_points) == cut_points)) {
    stop(
      "cut_points must be integer valued, and define the lower end of each category."
    )
  }
  if (any(cut_points <= lower_limit | cut_points >= upper_limit)) {
    warning(
      "cut_point values should be between lower_limit (",
      lower_limit,
      ") and upper_limit (",
      upper_limit,
      ")."
    )
  }
  # make sure the limits are not included.
  cut_points = cut_points[cut_points > lower_limit & cut_points < upper_limit]
  # sort and uniquify
  #
  breaks = unique(sort(c(lower_limit, cut_points, upper_limit + 1)))
  labels = tibble::tibble(
    low = utils::head(breaks, -1),
    next_low = utils::head(dplyr::lead(breaks, n = 1), -1),
    high = ifelse(next_low != upper_limit, next_low - 1, upper_limit)
  ) %>%
    dplyr::mutate(
      label = dplyr::case_when(
        low == high ~ sprintf("%1.0f", low),
        low == -Inf ~ sprintf("<%1.0f", next_low),
        high == Inf ~ sprintf("\u2265%1.0f", low),
        TRUE ~ sprintf("%1.0f\u2012%1.0f", low, high)
      )
    ) %>%
    dplyr::mutate(
      label2 = glue::glue(glue)
    )

  return(
    cut(
      x,
      include.lowest = TRUE,
      breaks = breaks,
      labels = labels$label2,
      ordered_result = TRUE,
      right = FALSE
    )
  )
}

# TODO: ----

#' @noRd
#' @examples
#' # example code
#' f_m <- function(x) {message("this is a message"); utils::str(x)}
#' f_w <- function(x) {warning("this is a warning"); utils::str(x)}
#' f_e <- function() {stop("This is an error")}
#'
#' pure_fm <- purely(f_m)
#' pure_fw <- purely(f_w)
#' pure_fe <- purely(f_e)
purely <- function(.f) {
  function(..., .log = "Log start...") {
    res <- rlang::try_fetch(
      rlang::eval_tidy(.f(...)),
      error = function(err) err,
      warning = function(warn) warn,
      message = function(message) message,
    )

    final_result <- list(
      result = NULL,
      log = NULL
    )

    final_result$result <- if (
      any(c("error", "warning", "message") %in% class(res))
    ) {
      NA
    } else {
      res
    }

    final_result$log <- if (
      any(c("error", "warning", "message") %in% class(res))
    ) {
      res$message
    } else {
      NA
    }
    final_result
  }
}


## Dataset description helpers ----

#' Get a value set list of a dataframe
#'
#' This function examines a dataframe and returns a list of the columns with sub-lists as all the options for factors.
#' This provides programmatic access (and autocomplete) to the values available in a dataframe, and throws and early
#' error if we try and access data by a variable that does not exist.
#'
#' @param df a dataframe to examine
#'
#' @return a list of lists with the column name and the factor levels as list, as a `checked list`.
#' @export
get_value_sets = function(df) {
  v = lapply(colnames(df), function(x) {
    if (all(is.na(df[[x]]))) {
      node = list(empty = TRUE)
    } else if (is.factor(df[[x]])) {
      node = as.list(levels(df[[x]]))
      names(node) = node
    } else if (is.character(df[[x]])) {
      tmp = unique(df[[x]])
      # TODO: sort by frequency
      node = as.list(tmp)
      names(node) = node
    } else if (is.numeric(df[[x]])) {
      node = list(
        min = min(df[[x]], na.rm = TRUE),
        max = max(df[[x]], na.rm = TRUE),
        mean = mean(df[[x]], na.rm = TRUE)
      )
    } else {
      node = list()
    }
    class(node) = c("checked_list", class(node))
    return(node)
  })
  names(v) = colnames(df)
  class(v) = c("checked_list", class(v))
  return(v)
}

#' Checked list accessor
#'
#' A checked list is a sub class of list access operator that throws an error if
#' you attempt to access a value that does not exist (rather than returning
#' NULL) the point of this is to throw errors early if the data changes.
#'
#' @param x the list
#' @param y the item
#'
#' @return the value of the list item or an error if it does not exist
#' @export
`$.checked_list` <- function(x, y) {
  if (is.character(y)) {
    ylab = y
  } else {
    ylab <- deparse(substitute(y))
  }
  if (!ylab %in% names(x)) {
    stop("The value `", ylab, "` is not a valid entry") # for ",xlab)
  }
  NextMethod()
}
