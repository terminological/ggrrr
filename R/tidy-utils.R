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
#' @param period the gap between observations as a number or, a negative number means the resulting sequence defines a end of time periods, a positive defines the beginning. may be an integer number of days, or a text string like '2 weeks', '-1 month', etc.
#' @param anchor defines the day of week the periods start or end. either "start", "end", a day of the week, or a date
#' @param fmt a strptime formatting string for date range labels.
#'
#' @return a vector of dates for complete between the minimum and maximum of dates, on the day of week of the anchoring date
#' @export
#'
#' @examples
#' full_seq_dates(c("2020-01-01","2020-02-01","2020-01-15","2020-02-01",NA), "2 days")
full_seq_dates = function(dates, period="1 day", anchor="start", fmt = "%d %b") {
  by = .period_to_string(period)
  dates = trunc(as.Date(dates))
  start = as.Date(min(dates,na.rm=TRUE),"1970-01-01")
  end = as.Date(max(dates,na.rm=TRUE),"1970-01-01")

  # figure out and standardise period:
  if (is.numeric(by)) by = paste0(floor(by)," day")
  dreg = "(-?)([0-9]*)\\s?(day|week|month|quarter|year)s?"
  if (length(by) != 1) stop("period in by parameter must be of length one")
  match = stringr::str_match_all(by,dreg)
  neg = match[[1]][1,2]
  quantity = if (match[[1]][1,3] == "") 1 else as.numeric(match[[1]][1,3])
  unit = match[[1]][1,4]
  if (anchor == "end") neg = "-"
  by = sprintf("%s%1.0f %s", neg, quantity, unit)


  anchor_date = NA
  if (anchor == "start") anchor_date=start
  if (anchor == "end") anchor_date=end+1
  if (is.na(anchor_date)) anchor_date = tryCatch(as.Date(anchor), error=function(e) NA)
  if (is.na(anchor_date)) anchor_date = (as.Date("1970-01-01")+0:6)[stringr::str_starts(tolower(weekdays(as.Date("1970-01-01")+0:6)),tolower(anchor))]
  if (length(anchor_date)!=1) stop("Did not understand anchor value. It should be one of 'start', 'end', or a (possibly abbreviated) day of week (e.g. sun)")

  # last anchor period before the start of the dates
  start2 = start-as.numeric(start - anchor_date)%%7
  # anchor period after the end of the dates
  end2 = end+7-as.numeric(end - anchor_date)%%7

  if (quantity == 1 & unit == "day") {
    out = seq(start,end,"1 day")
    out_label = as.character(out,fmt)
    out2 = structure(
      out,
      names = out_label,
      assign = function(date) {return(as.Date(ifelse(trunc(date) <= end & trunc(date) >=start, trunc(date), NA), "1970-01-01"))},
      period = "1 day"
    )
  } else if (neg == "-") {
    # This is backwards, therefore time periods are ending with the date
    out = seq(end2,start2,by)
    out = seq(from = end2,by = by,length.out = length(out)+1)
    # this will be in reverse order at this point
    out_label = sprintf("%s \u2014 %s", lead(as.character(out+1,fmt),default="before") , as.character(out,fmt))
    # next value (backwards) is not before start, and current value is not after end
    if (!all(dates %in% out)) {
      sel = !is.na(lead(out)) & lead(out) >= start &  out <= end
    } else {
      # the input was already periodic in some sense and doesn;t need to fit into complete bins
      sel = out >= start &  out <= end
    }
    out2 = structure(
      rev(out[sel]),
      names = rev(out_label[sel]),
      start.inclusive = rev(lead(out)[sel])+1,
      end.inclusive = rev(out[sel]),
      assign = function(date) {
        start.inclusive = rev(lead(out)[sel])+1
        end.inclusive = rev(out[sel])
        return(as.Date(sapply(trunc(date), function(d) rev(out[sel])[start.inclusive <= d & end.inclusive >= d][1]),"1970-01-01"))
      },
      period = by
    )

  } else {
    # This is forwards, therefore time periods are starting with the date
    out = seq(start2,end2,by)
    out = seq(start2,by = by,length.out = length(out)+1)
    out_label = sprintf("%s \u2014 %s", as.character(out,fmt) ,lead(as.character(out-1,fmt),default="onwards"))

    # next value (backwards) is not before start, and current value is not after end
    if (!all(dates %in% out)) {
      sel = !is.na(lead(out)) & out >= start & lead(out) <= end
    } else {
      # the imput was already periodic in some sense
      sel = out >= start & out <= end
    }
    out2 = structure(
      out[sel],
      names = out_label[sel],
      start.inclusive = out[sel],
      end.inclusive = lead(out)[sel]-1,
      assign = function(date) {
        start.inclusive = out[sel]
        end.inclusive = lead(out)[sel]-1
        return(as.Date(sapply(trunc(date), function(d) out[sel][start.inclusive <= d & end.inclusive >= d][1]),"1970-01-01"))
      },
      period = by
    )

  }

  return(out2)

}



# guess the intervals between dates
.day_interval = function(dates) {
  dates = sort(unique(dates))
  if (length(dates) < 4) return(1)
  interval = .gcd(stats::na.omit(as.numeric(dates-lag(dates))))
  return(interval)
}

# greatest common denominator
.gcd2 = function(a, b) {
  if (b == 0) a else Recall(b, a %% b)
}

.gcd <- function(...) {
  Reduce(.gcd2, c(...))
}

# lubridate period round trip to string
.period_to_string = function(p, backwards = FALSE) {
  sign = if(backwards) "-" else ""
  if (!lubridate::is.period(p)) {
    if (is.numeric(p)) return(sprintf("%s%1.0f day",sign,p))
    return(as.character(p))
  }
  pat = attributes(p)
  out = c()

  if (pat$year != 0) out = c(out,sprintf("%s%1.0f year",sign,pat$year))
  if (pat$month != 0) out = c(out,sprintf("%s%1.0f month",sign,pat$month))
  if (pat$day != 0) {
    if (pat$day%%7 == 0) out = c(out,sprintf("%s%1.0f week",sign,pat$day %/% 7))
    else out = c(out,sprintf("%s%1.0f day",sign,pat$day))
  }
  paste0(out, collapse = ", ")
}

#' Places a set of dates withing a regular time series
#'
#' where the periodicity of the time series is expressed as numbers of days, weeks, months quarters, or years.
#'
#' @param dates a set of dates
#' @param full_seq a full sequence of allowable dates as created by 'full_seq_dates()'.
#' Alternatively a vector of dates will some regular periodicity,
#' that will be used as an input for 'full_seq_dates()',
#' if missing this will be derived from the data itself.
#' @param factor return the result as an ordered factor with the date ranges as a label. if false this returns a date vector where the date is
#' @param ... if full_seq is not give, or a plain vector of dates, other options for 'full_seq_dates()' can be set here. E.g. ('fmt="\%d/\%m/\%Y", period="1 week")
#'
#' @return a set of dates, representing the start (or end) of the period the date falls into, where the period is defined by 'full_seq' - which is usually defined by 'full_seq_dates()'
#' @export
#'
#' @examples
#' dates = as.Date(c("2020-01-01","2020-02-01","2020-01-15","2020-02-03",NA))
#' fs = full_seq_dates(dates, "2 days")
#' dates - cut_date(dates, fs)
#' cut_date(dates,fs,TRUE)
#'
#' # A weekly set of dates:
#' dates2 = Sys.Date() + floor(runif(50,max=10))*7
#'
#' # in this specific situation the final date is not truncated because the
#' # input data is seen as an exact match for the whole output period.
#' cut_date(dates2, fmt = "%d/%b", factor = TRUE)
#'
#' # if the input dates don't line up with the output dates
#' # there may be incomplete coverage of the first and last category.
#' # where the cutting results in short periods. In this
#' # instance the first and last periods are truncated to prevent them
#' # being counted as complete when they are in fact potentially missing a few days worth of data:
#' cut_date(dates2, fmt = "%d/%b", factor = TRUE, period = "-2 weeks", anchor="sun")
#'
cut_date = function(dates, full_seq = dates, factor = FALSE, ...) {
  f = attr(full_seq,"assign")
  if (is.null(f)) {
    dots = rlang::list2(...)
    if (is.null(dots[["period"]])) dots[["period"]] = .day_interval(full_seq)
    full_seq = rlang::exec(full_seq_dates, full_seq, !!!dots)
    f = attr(full_seq,"assign")
  }
  out = f(dates)
  if (factor) out = factor(as.character(out), levels = as.character(full_seq), labels = names(full_seq), ordered = TRUE)
  return(out)
}

#' Convert a set of dates to numeric timepoints
#'
#' Using a day_zero and a unit specification or a full sequence of dates (see 'full_seq_dates()')
#'
#' @param dates a vector of dates to convert
#' @param unit a specification of the unit of the resulting time series. Will be determined from periodicity of dates if not specified
#' @param day_zero the origin of the conversion. Defaults to the beginning of the COVID pandemic
#'
#' @return a sequence of numeric time points as the number of periods since day zero
#' @export
#'
#' @examples
#' times = date_to_time(as.Date("2019-12-29")+0:100, "1 week")
#' dates = time_to_date(times)
date_to_time = function(dates, unit = .day_interval(dates), day_zero = getOption("day_zero","2019-12-29")) {

  if (!lubridate::is.period(unit)) {
    if (is.numeric(unit)) unit = lubridate::period(sprintf("%1.0f days", unit))
    else unit = lubridate::period(unit)
  }

  day_zero = as.Date(day_zero)

  return(structure(
    lubridate::interval(day_zero, dates) / unit,
    day_zero = day_zero,
    unit = .period_to_string(unit)
  ))
}

#' Convert a set of timepoints to dates
#'
#' @param timepoints a set of numeric time points
#' @param unit the period / unit of the time points, which will be extracted from timepoints if possible
#' @param day_zero the zero day of the time series, will be extracted from timepoints if possible
#'
#' @return a vector of dates
#' @export
#'
#' @examples
#' times = date_to_time(as.Date("2019-12-29")+0:100, "1 week")
#' dates = time_to_date(times)
time_to_date = function(timepoints, unit = attr(timepoints,"unit"), day_zero = getOption("day_zero","2019-12-29")) {

  if (is.null(unit)) stop("Cannot determine unit from timepoints input. You must specify unit.")
  if (!lubridate::is.period(unit)) {
    if (is.numeric(unit)) unit = lubridate::period(sprintf("%1.0f days", unit))
    else unit = lubridate::period(unit)
  }

  day_zero = as.Date(day_zero)

  x = sort(unique(c(floor(timepoints),ceiling(timepoints))))
  y = day_zero+unit*x
  as.Date(stats::approx(x,y,xout=timepoints)$y,"1970-01-01")
}


#' Generate a full regular timepoint sequence
#'
#' @param timepoints a set of timepoints relating to data
#' @param period the desired interval between time points, e.g. "1 day". negative periods define the intervals as closed on the left
#' @param unit the unit of the timepoints in terms of "1 week"
#' @param day_zero the origin of the timepoints
#' @param ... passed to 'full_seq_dates()', paricularly anchor, and fmt, to define the day of week of the new sequence and the format of the labels.
#'
#' @return a complete set of timepoints on the same scale as the original but with potentially different frequency. This will probably involve non integer times
#' @export
#'
#' @examples
#' times = date_to_time(as.Date("2019-12-29")+0:100, "1 week")
#' tmp = full_seq_times(times)
full_seq_times = function(timepoints, period = unit, unit = attr(timepoints,"unit"), day_zero = attr(timepoints,"day_zero"), ...) {
  if (is.null(unit)) stop("Cannot determine unit from timepoints input. You must specify unit")
  if (is.null(day_zero)) {
    warning("Cannot determine day_zero from timepoints input. Using defaults from getOption('day_zero')")
    day_zero = getOption("day_zero","2019-12-29")
  }

  if (!lubridate::is.period(unit)) {
    if (is.numeric(unit)) unit = lubridate::period(sprintf("%1.0f days", unit))
    else unit = lubridate::period(unit)
  }

  dates.old = time_to_date(timepoints, unit, day_zero = day_zero)
  dates.new = full_seq_dates(dates.old, period, ...)
  atts = attributes(dates.new)
  cuts = c(atts$start.inclusive,last(atts$end.inclusive)+1)
  out = date_to_time(dates.new, unit, day_zero = day_zero)
  breaks = date_to_time(cuts, unit, day_zero = day_zero)
  attributes(breaks)=NULL
  right = stringr::str_starts(period,"-")
  if (right) {
    structure(
      out,
      names = atts$names,
      period = period,
      breaks = breaks,
      assign = function(timepoint) {
        return(sapply(timepoint, function(d) {
          sel = (lag(breaks) < d & d <= breaks)[-1]
          out[sel][1]
        }))
      }
    )
  } else {
    structure(
      out,
      names = atts$names,
      period = period,
      breaks = breaks,
      assign = function(timepoint) {
        return(sapply(timepoint, function(d) {
          sel = (lag(breaks) <= d & d < breaks)[-1]
          out[sel][1]
        }))
      }
    )
  }
}

#' Places a set of dates withing a regular time series
#'
#' where the periodicity of the time series is expressed as numbers of days, weeks, months quarters, or years.
#'
#' @param timepoints a set of times (defined by count of periods from a zero day - see 'date_to_time()')
#' @param full_seq a full sequence of allowable dates as created by 'full_seq_dates()'.
#' Alternatively a vector of dates will some regular periodicity,
#' that will be used as an input for 'full_seq_dates()',
#' if missing this will be derived from the data itself.
#' @param unit the unit of the timepoints in terms of "1 week"
#' @param day_zero the origin of the timepoints
#' @param factor return the result as an ordered factor with the date ranges as a label. if false this returns a date vector where the date is
#' @param ... if full_seq is not give, or a plain vector of dates, other options for 'full_seq_dates()' can be set here. E.g. ('fmt="\%d/\%m/\%Y", period="1 week")
#'
#' @return a set of dates, representing the start (or end) of the period the date falls into, where the period is defined by 'full_seq' - which is usually defined by 'full_seq_dates()'
#' @export
#'
#' @examples
#' dates = as.Date(c("2020-01-01","2020-02-01","2020-01-15","2020-02-03",NA))
#' fs = full_seq_dates(dates, "2 days")
#' dates - cut_date(dates, fs)
#' cut_date(dates,fs,TRUE)
#'
#' # A weekly set of dates:
#' dates2 = Sys.Date() + floor(runif(50,max=10))*7
#' times2 = date_to_time(dates2)
#'
#' # in this specific situation the final date is not truncated because the
#' # input data is seen as an exact match for the whole output period.
#' cut_time(times2, fmt = "%d/%b", factor = TRUE)
#'
#' # if the input dates don't line up with the output dates
#' # there may be incomplete coverage of the first and last category.
#' # where the cutting results in short periods. In this instance
#' # the first and last periods are truncated to prevent them
#' # being counted as complete when they are in fact potentially missing a few days worth of data:
#' cut_time(times2, fmt = "%d/%b", factor = TRUE, period = "-2 weeks", anchor="sun")
#' times2 - cut_time(times2, fmt = "%d/%b", factor = FALSE, period = "-2 weeks", anchor="sun")
#'
#'
cut_time = function(timepoints, full_seq = timepoints, unit = attr(timepoints,"unit"), day_zero = attr(timepoints,"day_zero"), factor = FALSE, ...) {

  if (is.null(unit)) stop("Cannot determine unit from timepoints input. You must specify unit.")
  if (is.null(day_zero)) {
    warning("Cannot determine day_zero from timepoints input. Using defaults from getOption('day_zero')")
    day_zero = getOption("day_zero","2019-12-29")
  }

  f = attr(full_seq,"assign")
  if (is.null(f)) {
    dots = rlang::list2(...)
    dots$unit = unit
    dots$day_zero = day_zero
    full_seq = rlang::exec(full_seq_times, full_seq, !!!dots)
    f = attr(full_seq,"assign")
  }
  out = f(timepoints)
  if (factor) out = factor(as.character(out), levels = as.character(full_seq), labels = names(full_seq), ordered = TRUE)
  return(out)
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
