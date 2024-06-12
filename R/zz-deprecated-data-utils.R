

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
#' # full_seq_dates(c("2020-01-01","2020-02-01","2020-01-15","2020-02-01",NA), "2 days")
full_seq_dates = function(dates, period="1 day", anchor="start", fmt = "%d %b") {
  stop("deprecated: please switch to growthrates::full_seq()")
  # by = .period_to_string(period)
  # dates = trunc(as.Date(dates))
  # start = as.Date(min(dates,na.rm=TRUE),"1970-01-01")
  # end = as.Date(max(dates,na.rm=TRUE),"1970-01-01")
  #
  # # figure out and standardise period:
  # if (is.numeric(by)) by = paste0(floor(by)," day")
  # dreg = "(-?)([0-9]*)\\s?(day|week|month|quarter|year)s?"
  # if (length(by) != 1) stop("period in by parameter must be of length one")
  # match = stringr::str_match_all(by,dreg)
  # neg = match[[1]][1,2]
  # quantity = if (match[[1]][1,3] == "") 1 else as.numeric(match[[1]][1,3])
  # unit = match[[1]][1,4]
  # if (is.character(anchor) && anchor == "end") neg = "-"
  # by = sprintf("%s%1.0f %s", neg, quantity, unit)
  #
  #
  # anchor_date = NA
  # if (is.character(anchor) && anchor == "start") anchor_date=start
  # if (is.character(anchor) && anchor == "end") anchor_date=end+1
  # if (is.na(anchor_date)) anchor_date = tryCatch(as.Date(anchor), error=function(e) NA)
  # if (is.na(anchor_date)) anchor_date = (as.Date("1970-01-01")+0:6)[stringr::str_starts(tolower(weekdays(as.Date("1970-01-01")+0:6)),tolower(anchor))]
  # if (length(anchor_date)!=1) stop("Did not understand anchor value. It should be one of 'start', 'end', or a (possibly abbreviated) day of week (e.g. sun)")
  #
  # # last anchor period before the start of the dates
  # start2 = start-as.numeric(start - anchor_date)%%7
  # # anchor period after the end of the dates
  # end2 = end+7-as.numeric(end - anchor_date)%%7
  #
  # if (quantity == 1 & unit == "day") {
  #   out = seq(start,end,"1 day")
  #   out_label = as.character(out,fmt)
  #   out2 = structure(
  #     out,
  #     names = out_label,
  #     assign = function(date) {return(as.Date(ifelse(trunc(date) <= end & trunc(date) >=start, trunc(date), NA), "1970-01-01"))},
  #     period = "1 day"
  #   )
  # } else if (neg == "-") {
  #   # This is backwards, therefore time periods are ending with the date
  #   out = seq(end2,start2,by)
  #   out = seq(from = end2,by = by,length.out = length(out)+1)
  #   # this will be in reverse order at this point
  #   out_label = sprintf("%s \u2014 %s", dplyr::lead(as.character(out+1,fmt),default="before") , as.character(out,fmt))
  #   # next value (backwards) is not before start, and current value is not after end
  #   if (!all(dates %in% out)) {
  #     sel = !is.na(dplyr::lead(out)) & dplyr::lead(out) >= start &  out <= end
  #   } else {
  #     # the input was already periodic in some sense and doesn;t need to fit into complete bins
  #     sel = out >= start &  out <= end
  #   }
  #   out2 = structure(
  #     rev(out[sel]),
  #     names = rev(out_label[sel]),
  #     start.inclusive = rev(dplyr::lead(out)[sel])+1,
  #     end.inclusive = rev(out[sel]),
  #     assign = function(date) {
  #       start.inclusive = rev(dplyr::lead(out)[sel])+1
  #       end.inclusive = rev(out[sel])
  #       return(as.Date(sapply(trunc(date), function(d) rev(out[sel])[start.inclusive <= d & end.inclusive >= d][1]),"1970-01-01"))
  #     },
  #     period = by
  #   )
  #
  # } else {
  #   # This is forwards, therefore time periods are starting with the date
  #   out = seq(start2,end2,by)
  #   out = seq(start2,by = by,length.out = length(out)+1)
  #   out_label = sprintf("%s \u2014 %s", as.character(out,fmt) ,dplyr::lead(as.character(out-1,fmt),default="onwards"))
  #
  #   # next value (backwards) is not before start, and current value is not after end
  #   if (!all(dates %in% out)) {
  #     sel = !is.na(dplyr::lead(out)) & out >= start & dplyr::lead(out) <= end
  #   } else {
  #     # the imput was already periodic in some sense
  #     sel = out >= start & out <= end
  #   }
  #   out2 = structure(
  #     out[sel],
  #     names = out_label[sel],
  #     start.inclusive = out[sel],
  #     end.inclusive = dplyr::lead(out)[sel]-1,
  #     assign = function(date) {
  #       start.inclusive = out[sel]
  #       end.inclusive = dplyr::lead(out)[sel]-1
  #       return(as.Date(sapply(trunc(date), function(d) out[sel][start.inclusive <= d & end.inclusive >= d][1]),"1970-01-01"))
  #     },
  #     period = by
  #   )
  #
  # }
  #
  # return(out2)

}



# # guess the intervals between dates
.day_interval = function(dates) {
  dates = sort(unique(dates))
  if (length(dates) < 4) return(1)
  interval = .gcd(stats::na.omit(as.numeric(dates-stats::lag(dates))))
  return(interval)
}

# greatest common denominator
.gcd2 = function(a, b) {
  if (b == 0) a else Recall(b, a %% b)
}
#
.gcd <- function(...) {
  Reduce(.gcd2, c(...))
}

# lubridate period round trip to string
# .period_to_string = function(p, backwards = FALSE) {
#   sign = if(backwards) "-" else ""
#   if (!lubridate::is.period(p)) {
#     if (is.numeric(p)) return(sprintf("%s%1.0f day",sign,p))
#     return(as.character(p))
#   }
#   pat = attributes(p)
#   out = c()
#
#   if (pat$year != 0) out = c(out,sprintf("%s%1.0f year",sign,pat$year))
#   if (pat$month != 0) out = c(out,sprintf("%s%1.0f month",sign,pat$month))
#   if (pat$day != 0) {
#     if (pat$day%%7 == 0) out = c(out,sprintf("%s%1.0f week",sign,pat$day %/% 7))
#     else out = c(out,sprintf("%s%1.0f day",sign,pat$day))
#   }
#   paste0(out, collapse = ", ")
# }

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
#' # dates = as.Date(c("2020-01-01","2020-02-01","2020-01-15","2020-02-03",NA))
#' # fs = full_seq_dates(dates, "2 days")
#' # dates - cut_date(dates, fs)
#' # cut_date(dates,fs,TRUE)
#'
#' # A weekly set of dates:
#' # dates2 = Sys.Date() + floor(stats::runif(50,max=10))*7
#'
#' # in this specific situation the final date is not truncated because the
#' # input data is seen as an exact match for the whole output period.
#' # cut_date(dates2, fmt = "%d/%b", factor = TRUE)
#'
#' # if the input dates don't line up with the output dates
#' # there may be incomplete coverage of the first and last category.
#' # where the cutting results in short periods. In this
#' # instance the first and last periods are truncated to prevent them
#' # being counted as complete when they are in fact potentially missing a few days worth of data:
#' # cut_date(dates2, fmt = "%d/%b", factor = TRUE, period = "-2 weeks", anchor="sun")
#'
cut_date = function(dates, full_seq = dates, factor = FALSE, ...) {
  stop("deprecated: please switch to growthrates::cut_date()")
  # f = attr(full_seq,"assign")
  # if (is.null(f)) {
  #   dots = rlang::list2(...)
  #   if (is.null(dots[["period"]])) dots[["period"]] = .day_interval(full_seq)
  #   full_seq = rlang::exec(full_seq_dates, full_seq, !!!dots)
  #   f = attr(full_seq,"assign")
  # }
  # out = f(dates)
  # if (factor) out = factor(as.character(out), levels = as.character(full_seq), labels = names(full_seq), ordered = TRUE)
  # return(out)
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
#' # DEPRECATED
#' # times = date_to_time(as.Date("2019-12-29")+0:100, "1 week")
#' # dates = time_to_date(times)
date_to_time = function(dates, unit = .day_interval(dates), day_zero = getOption("day_zero","2019-12-29")) {
  stop("deprecated: please switch to growthrates::time_period")
  # if (!lubridate::is.period(unit)) {
  #   if (is.numeric(unit)) unit = lubridate::period(sprintf("%1.0f days", unit))
  #   else unit = lubridate::period(unit)
  # }
  #
  # day_zero = as.Date(day_zero)
  #
  # return(structure(
  #   lubridate::interval(day_zero, dates) / unit,
  #   day_zero = day_zero,
  #   unit = .period_to_string(unit)
  # ))
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
#' # DEPRECATED
#' # times = date_to_time(as.Date("2019-12-29")+0:100, "1 week")
#' # dates = time_to_date(times)
time_to_date = function(timepoints, unit = attr(timepoints,"unit"), day_zero = getOption("day_zero","2019-12-29")) {
  stop("deprecated: please switch to growthrates::full_seq()")
  # if (is.null(unit)) stop("Cannot determine unit from timepoints input. You must specify unit.")
  # if (!lubridate::is.period(unit)) {
  #   if (is.numeric(unit)) unit = lubridate::period(sprintf("%1.0f days", unit))
  #   else unit = lubridate::period(unit)
  # }
  #
  # day_zero = as.Date(day_zero)
  #
  # x = sort(unique(c(floor(timepoints),ceiling(timepoints))))
  # y = day_zero+unit*x
  # as.Date(floor(stats::approx(x,y,xout=timepoints)$y),"1970-01-01")
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
#' # DEPRECATED
#' # times = date_to_time(as.Date("2019-12-29")+0:100, "1 week")
#' # tmp = full_seq_times(times)
full_seq_times = function(timepoints, period = unit, unit = attr(timepoints,"unit"), day_zero = attr(timepoints,"day_zero"), ...) {
  stop("deprecated: please switch to growthrates::full_seq()")
  # if (is.null(unit)) stop("Cannot determine unit from timepoints input. You must specify unit")
  # if (is.null(day_zero)) {
  #   warning("Cannot determine day_zero from timepoints input. Using defaults from getOption('day_zero')")
  #   day_zero = getOption("day_zero","2019-12-29")
  # }
  #
  # if (!lubridate::is.period(unit)) {
  #   if (is.numeric(unit)) unit = lubridate::period(sprintf("%1.0f days", unit))
  #   else unit = lubridate::period(unit)
  # }
  #
  # dates.old = time_to_date(timepoints, unit, day_zero = day_zero)
  # dates.new = full_seq_dates(dates.old, period, ...)
  # atts = attributes(dates.new)
  # cuts = c(atts$start.inclusive,dplyr::last(atts$end.inclusive)+1)
  # out = date_to_time(dates.new, unit, day_zero = day_zero)
  # breaks = date_to_time(cuts, unit, day_zero = day_zero)
  # attributes(breaks)=NULL
  # right = stringr::str_starts(period,"-")
  # if (right) {
  #   structure(
  #     out,
  #     names = atts$names,
  #     period = period,
  #     breaks = breaks,
  #     assign = function(timepoint) {
  #       return(sapply(timepoint, function(d) {
  #         sel = (stats::lag(breaks) < d & d <= breaks)[-1]
  #         out[sel][1]
  #       }))
  #     }
  #   )
  # } else {
  #   structure(
  #     out,
  #     names = atts$names,
  #     period = period,
  #     breaks = breaks,
  #     assign = function(timepoint) {
  #       return(sapply(timepoint, function(d) {
  #         sel = (stats::lag(breaks) <= d & d < breaks)[-1]
  #         out[sel][1]
  #       }))
  #     }
  #   )
  # }
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
#' #dates = as.Date(c("2020-01-01","2020-02-01","2020-01-15","2020-02-03",NA))
#' #fs = full_seq_dates(dates, "2 days")
#' #dates - cut_date(dates, fs)
#' #cut_date(dates,fs,TRUE)
#'
#' # A weekly set of dates:
#'# dates2 = Sys.Date() + floor(stats::runif(50,max=10))*7
#' #times2 = date_to_time(dates2)
#'
#' # in this specific situation the final date is not truncated because the
#' # input data is seen as an exact match for the whole output period.
#' #cut_time(times2, fmt = "%d/%b", factor = TRUE)
#'
#' # if the input dates don't line up with the output dates
#' # there may be incomplete coverage of the first and last category.
#' # where the cutting results in short periods. In this instance
#' # the first and last periods are truncated to prevent them
#' # being counted as complete when they are in fact potentially missing a few days worth of data:
#' #cut_time(times2, fmt = "%d/%b", factor = TRUE, period = "-2 weeks", anchor="sun")
#' #times2 - cut_time(times2, fmt = "%d/%b", factor = FALSE, period = "-2 weeks", anchor="sun")
#'
#'
cut_time = function(timepoints, full_seq = timepoints, unit = attr(timepoints,"unit"), day_zero = attr(timepoints,"day_zero"), factor = FALSE, ...) {
  stop("deprecated: please switch to growthrates::cut()")
  # if (is.null(unit)) stop("Cannot determine unit from timepoints input. You must specify unit.")
  # if (is.null(day_zero)) {
  #   warning("Cannot determine day_zero from timepoints input. Using defaults from getOption('day_zero')")
  #   day_zero = getOption("day_zero","2019-12-29")
  # }
  #
  # f = attr(full_seq,"assign")
  # if (is.null(f)) {
  #   dots = rlang::list2(...)
  #   dots$unit = unit
  #   dots$day_zero = day_zero
  #   full_seq = rlang::exec(full_seq_times, full_seq, !!!dots)
  #   f = attr(full_seq,"assign")
  # }
  # out = f(timepoints)
  # if (factor) out = factor(as.character(out), levels = as.character(full_seq), labels = names(full_seq), ordered = TRUE)
  # return(out)
}
