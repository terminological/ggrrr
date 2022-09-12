# look for a dataframe as the first argument in the call stack
.search_call_stack = function(nframe = sys.nframe()-1) {
  frame = sys.frame(nframe)
  first_arg_name = ls(frame)[1]
  try({
    data = suppressWarnings(first_arg_name %>% get(envir=frame))
    if(is.data.frame(data)) return(data)
  })
  nframe = nframe-1
  if (nframe < 1) stop("no data frame found")
  .search_call_stack(nframe)
}


#' Reuse tidy-select syntax outside of a tidy-select function
#'
#' @param tidyselect a tidyselect syntax which will be evaluated in context by looking for a call in the call stack that includes a dataframe as the first argument
#' @param data (optional) a specific dataframe with which to evaluate the tidyselect
#'
#' @return a list of symbols resulting from the evaluation of the tidyselect in the context of the current call stack (or a provided data frame)
#' @export
as_vars = function(tidyselect, data=NULL) {
  expr = rlang::enquo(tidyselect)
  if(is.null(data)) data = .search_call_stack()
  res = tidyselect::eval_select(expr,data)
  lapply(names(res), as.symbol)
}

## Dataset description helpers ----


#' Get a value set list of a dataframe
#'
#' This function examines a dataframe and returns a list of the columns with sub-lists as all the options for factors.
#' This provides programmatic access (and automcomplete) to the values available in a dataframe, and throws and early
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
        min = min(df[[x]],na.rm = TRUE),
        max = max(df[[x]],na.rm = TRUE),
        mean = mean(df[[x]],na.rm = TRUE)
      )
    } else {
      node = list()
    }
    class(node) = c("checked_list",class(node))
    return(node)
  })
  names(v) = colnames(df)
  class(v) = c("checked_list",class(v))
  return(v)
}

# This is a sub class of list access operator that throws an error if you attempt to access a value that does not exist (rather than returning NULL)
# the point of this is to throw errors early if the data changes.
`$.checked_list` <- function(x, y) {
  if (is.character(y)) {
    ylab = y
  } else {
    ylab <- deparse(substitute(y))
  }
  if(!ylab %in% names(x)) {
    stop("The value `",ylab,"` is not a valid entry") # for ",xlab)
  }
  NextMethod()
}


## Summarisation helpers ----

#' Bind rows for colums with factors
#'
#' Bind_rows works until there are factors with a set of different levels then it throws a
#' wobbly. This handles that paricular situation by combining factor levels.
#'
#' @param ... a list of dataframes
#'
#' @return the union of those dataframes. Factor levels are combined with a superset of all levels
#' @export
#'
#' @examples
#' library(tidyverse)
#' bind_rows_with_factors(iris,
#'  ggplot2::diamonds %>% rename(Species = cut)) %>%
#'  pull(Species) %>%
#'  levels()
bind_rows_with_factors <- function(...) {

  name = value = NULL # remove global binding note

  # Identify all factors, and all their levels and
  dots = rlang::list2(...)
  factors = unique(unlist(purrr::map(dots, ~ .x %>% dplyr::ungroup() %>% dplyr::select(where(is.factor)) %>% colnames())))
  factorLevels = dplyr::bind_rows(purrr::map(dots, ~ purrr::map(.x %>% dplyr::ungroup() %>% dplyr::select(where(is.factor)), ~ .x %>% levels()) %>% tibble::enframe() %>% tidyr::unnest(c(value))))
  # convert factors to character, bind dataframes, convert characters back to factors
  dots2 = purrr::map(dots, ~ .x %>% dplyr::mutate(dplyr::across(where(is.factor),as.character)))
  out = dplyr::bind_rows(dots2)
  for (col in factors) {
    # combine the factors. This will tend to keep the order of the levels the first time a factor is encountered
    l = factorLevels %>% dplyr::filter(name==col) %>% dplyr::pull(value) %>% unique()
    out[[col]] = factor(out[[col]],levels = l)
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
#'     mpg = sprintf("%1.1f \u00B1 %1.1f", mean(price), sd(price)),
#'     .total = "Overall"
#'  )
summarise_with_totals = function(.data, ..., .groups = NULL, .total="Total", .total_first = FALSE) {
  grps = .data %>% dplyr::groups()
  last_col = (utils::tail(grps,1)[[1]])
  first_level = .data %>% dplyr::summarise(...,.groups=.groups)
  grps_out = first_level %>% dplyr::groups()
  totals = .data %>% dplyr::ungroup() %>% dplyr::group_by(!!!(grps %>% utils::head(-1))) %>% dplyr::summarise(...,.groups=.groups)

  if (.data %>% dplyr::pull(!!last_col) %>% is.factor()) {
    if (.data %>% dplyr::pull(!!last_col) %>% is.ordered()) {
      totals = totals %>% dplyr::mutate(!!last_col := .total %>% ordered())
    } else {
      totals = totals %>% dplyr::mutate(!!last_col := .total %>% factor())
    }
    if (.total_first) {
      out = ggrrr::bind_rows_with_factors(totals, first_level)
    } else {
      out = ggrrr::bind_rows_with_factors(first_level, totals)
    }
  } else {
    totals = totals %>% dplyr::mutate(!!last_col := .total %>% factor())
    first_level = first_level %>% dplyr::mutate(!!last_col := as.character(!!last_col))
    if (.total_first) {
      out = dplyr::bind_rows(totals,first_level)
    } else {
      out = dplyr::bind_rows(first_level,totals)
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
intersecting_group_by = function(.data, ..., .colname = "group") {
  .colname = rlang::ensym(.colname)
  grps = .data %>% dplyr::groups()
  dots = rlang::list2(...)
  dplyr::bind_rows(lapply(dots, function(form) {
    value = rlang::f_rhs(form)
    predicate = rlang::f_lhs(form)
    .data %>% dplyr::filter(!!predicate) %>% dplyr::mutate(!!.colname := value)
  })) %>% dplyr::group_by(!!!grps, !!.colname)
}

# .maybe = function(expr) {
#   expr = enexpr(expr)
#   browser()
#   #data = .search_call_stack()
#   tryCatch(rlang::eval_tidy(expr,tidyselect::peek_data()), error = function(e) {
#     warning(e$message)
#     NA}
#   )
# }

# TODO:
# fct_set_na = function(x, predicateExpr) {
#   predicateExpr = enexpr(predicateExpr)
#   tmp = ifelse(!!predicateExpr, NA, as.character(x))
#   factor(tmp, levels=levels(x), ordered = is.ordered(x))
# }

## Data manipulation helpers ----

#' Expand a data vector to the full range
#'
#' Convert a vector of observation dates to a ordered sequence of every day in the time series
#'
#' @param dates a vector of dates, possibly including NA values
#'
#' @return a vector of dates for every day between the minimum and maximum of dates
#' @export
#'
#' @examples
#' full_seq_dates(c("2020-01-01","2020-02-01","2020-01-15","2020-02-01",NA))
full_seq_dates = function(dates) {
  dates = as.Date(dates)
  as.Date(seq(min(dates,na.rm=TRUE),max(dates,na.rm=TRUE),1),"1970-01-01")
}

# wrap functions that chuck an error
.opt = function(expr) tryCatch(expr,error=function(e) NA_real_)

#' Cut and label an integer valued quantity
#'
#' Deals with some annoying issues classifying integer data sets, such as ages, into groups. where you want to
#' specify just the change over points as integers and clearly label the resulting ordered factor.
#'
#' @param x a vector of integer valued numbers, e.g. ages, counts
#' @param cut_points a vector of integer valued cut points which define the lower boundaries of conditions
#' @param glue a glue spec that may be used to generate a label. It can use {low}, {high}, {next_low}, or {label} as values.
#' @param lower_limit the minimum value we should include (this is inclusive for the bottom category) (default -Inf)
#' @param upper_limit the maximum value we should include (this is also inclusive for the top category) (default Inf)
#' @param ... not used
#'
#' @return an ordered factor of the integer
#' @export
#'
#' @examples
#' cut_integer(rbinom(20,20,0.5), c(5,10,15))
#' cut_integer(floor(runif(100,-10,10)), cut_points = c(2,3,4,6), lower_limit=0, upper_limit=10)
cut_integer = function(x, cut_points, glue = "{label}", lower_limit = -Inf, upper_limit = Inf, ...) {

  next_low = NULL # remove global binding note

  if (!all(as.integer(x)==x)) warning("input to cut_integer(...) has been coerced to integer values")
  x = floor(x)
  if (!all(as.integer(cut_points)==cut_points)) stop("cut_points must be integer valued, and define the lower end of each category.")
  if (any(cut_points <= lower_limit | cut_points >= upper_limit)) warning("cut_point values should be between lower_limit (",lower_limit,") and upper_limit (",upper_limit,").")
  # make sure the limits are not included.
  cut_points = cut_points[cut_points > lower_limit & cut_points < upper_limit]
  # sort and uniquify
  #
  breaks = unique(sort(c(lower_limit,cut_points,upper_limit+1)))
  labels = tibble::tibble(
    low = utils::head(breaks,-1),
    next_low = utils::head(dplyr::lead(breaks,n = 1),-1),
    high = ifelse(next_low != upper_limit, next_low-1, upper_limit)
  ) %>% dplyr::mutate(
    label = dplyr::case_when(
      low == high ~ sprintf("%1.0f",low),
      low == -Inf ~ sprintf("<%1.0f",next_low),
      high == Inf ~ sprintf("\u2265%1.0f",low),
      TRUE ~ sprintf("%1.0f\u2012%1.0f",low, high)
    )
  ) %>% dplyr::mutate(
    label2 = glue::glue(glue)
  )

  return(
    cut(x,include.lowest = TRUE,breaks = breaks, labels=labels$label2, ordered_result = TRUE)
  )
}


## Formula helpers ----

.vars_from_rhs = function(formula) {
  # formula = age+gender+region ~ date + count(n) + type(cls)
  if (is.null(formula)) return(NULL)
  vars = all.vars(rlang::f_rhs(formula))
  expressions = attr(terms(as.formula(paste0("~ ",as_label(rlang::f_rhs(formula))))),"variables")
  expressionStrings = lapply(expressions, as_label)[-1]
  names = stringr::str_extract(expressionStrings,"(.*)\\(.*\\)") %>% stringr::str_remove("\\(.*\\)")
  names[is.na(names)] = "date"
  if (any(duplicated(names))) stop("the right had side has duplicates in it. Did you foget to name all the terms?")
  tmp = lapply(vars,as.symbol)
  names(tmp) = names
  return(tmp)
}

.var_from_rhs = function(formula, match="date") {
  if (is.null(formula)) return(NA)
  v = .vars_from_rhs(formula)
  if (!any(names(v)==match)) return(NA)
  return(v[[match]])
}

.vars_from_lhs = function(formula) {
  if (is.null(formula)) return(NULL)
  vars = all.vars(rlang::f_lhs(formula))
  if (length(vars)==0) return(vars())
  lapply(vars, as.symbol)
}

.formula_from_vars = function(dateVar, grps, ...) {
  lhs = paste0(grps, collapse = "+")
  rhs = c(date=dateVar, rlang::list2(...))
  rhs = rhs[!is.na(rhs)]
  rhs = paste0(names(rhs),"(",rhs,")",collapse = "+")
  return(as.formula(paste0(lhs," ~ ",rhs)))
}
