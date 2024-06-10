## Ported from
## https://github.com/tidyverse/readr/blob/main/R/collectors.R
## https://github.com/tidyverse/readr/blob/main/R/locale.R

## MIT See copyright file: https://github.com/tidyverse/readr/blob/main/LICENSE

#' @useDynLib minty, .registration = TRUE
NULL

collector <- function(type, ...) {
    structure(list(...), class = c(paste0("collector_", type), "collector"))
}

is.collector <- function(x) inherits(x, "collector")

collector_find <- function(name) {
    if (is.na(name)) {
        return(col_character())
    }
    get(paste0("col_", name), envir = asNamespace("minty"))()
}

#' Parse a character vector.
#'
#' @family parsers
#' @param x Character vector of elements to parse.
#' @param collector Column specification.
#' @param .return_problems Whether to hide the `problems` tibble from the output
#' @keywords internal
#' @export
#' @return a parsed vector
#' @examples
#' x <- c("1", "2", "3", "NA")
#' parse_vector(x, col_integer())
#' parse_vector(x, col_double())
parse_vector <- function(x, collector, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE, .return_problems = FALSE) {
    stopifnot(is.character(x))
    if (is.character(collector)) {
        collector <- collector_find(collector)
    }

    ##  warn_problems(parse_vector_(x, collector, na = na, locale_ = locale, trim_ws = trim_ws))
    res <- parse_vector_(x, collector, na = na, locale_ = locale, trim_ws = trim_ws)
    if (.return_problems || is.null(attr(res, "problems"))) {
        return(res)
    }
    attr(res, "problems") <- NULL
    return(res)
}

#' Parse logicals, integers, and reals
#'
#' Use `parse_*()` if you have a character vector you want to parse.
#'
#' @name parse_atomic
#' @aliases NULL
#' @param x Character vector of values to parse.
#' @param na Character vector of strings to interpret as missing values. Set this
#'   option to `character()` to indicate no missing values.
#' @param locale The locale controls defaults that vary from place to place.
#'   The default locale is US-centric (like R), but you can use
#'   [locale()] to create your own locale that controls things like
#'   the default time zone, encoding, decimal mark, big mark, and day/month
#'   names.
#' @param trim_ws Should leading and trailing whitespace (ASCII spaces and tabs) be trimmed from
#'     each field before parsing it?
#' @inheritParams parse_vector
#' @family parsers
#' @return a parsed vector
#' @examples
#' parse_integer(c("1", "2", "3"))
#' parse_double(c("1", "2", "3.123"))
#' parse_number("$1,123,456.00")
#'
#' # Use locale to override default decimal and grouping marks
#' es_MX <- locale("es", decimal_mark = ",")
#' parse_number("$1.123.456,00", locale = es_MX)
#'
#' # Invalid values are replaced with missing values with a warning.
#' x <- c("1", "2", "3", "-")
#' parse_double(x)
#' # Or flag values as missing
#' parse_double(x, na = "-")
NULL

#' @rdname parse_atomic
#' @export
parse_logical <- function(x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE, .return_problems = FALSE) {
    parse_vector(x, col_logical(), na = na, locale = locale, trim_ws = trim_ws, .return_problems = .return_problems)
}

#' @rdname parse_atomic
#' @export
parse_integer <- function(x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE, .return_problems = FALSE) {
    parse_vector(x, col_integer(), na = na, locale = locale, trim_ws = trim_ws, .return_problems = .return_problems)
}

#' @rdname parse_atomic
#' @export
parse_double <- function(x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE, .return_problems = FALSE) {
    parse_vector(x, col_double(), na = na, locale = locale, trim_ws = trim_ws, .return_problems = .return_problems)
}

#' @rdname parse_atomic
#' @export
parse_character <- function(x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE, .return_problems = FALSE) {
    parse_vector(x, col_character(), na = na, locale = locale, trim_ws = trim_ws, .return_problems = .return_problems)
}

#' @rdname parse_atomic
#' @export
col_logical <- function() {
    collector("logical")
}

#' @rdname parse_atomic
#' @export
col_integer <- function() {
    collector("integer")
}

#' @rdname parse_atomic
#' @export
col_double <- function() {
    collector("double")
}

#' @rdname parse_atomic
#' @export
col_character <- function() {
    collector("character")
}

#' @rdname cols
#' @family parsers
#' @export
col_skip <- function() {
    collector("skip")
}

#' Parse numbers, flexibly
#'
#' This parses the first number it finds, dropping any non-numeric characters
#' before the first number and all characters after the first number. The
#' grouping mark specified by the locale is ignored inside the number.
#'
#' @inheritParams parse_atomic
#' @return A numeric vector (double) of parsed numbers.
#' @family parsers
#' @export
#' @return a parsed vector
#' @examples
#' ## These all return 1000
#' parse_number("$1,000") ## leading `$` and grouping character `,` ignored
#' parse_number("euro1,000") ## leading non-numeric euro ignored
#' parse_number("t1000t1000") ## only parses first number found
#'
#' parse_number("1,234.56")
#' ## explicit locale specifying European grouping and decimal marks
#' parse_number("1.234,56", locale = locale(decimal_mark = ",", grouping_mark = "."))
#' ## SI/ISO 31-0 standard spaces for number grouping
#' parse_number("1 234.56", locale = locale(decimal_mark = ".", grouping_mark = " "))
#'
#' ## Specifying strings for NAs
#' parse_number(c("1", "2", "3", "NA"))
#' parse_number(c("1", "2", "3", "NA", "Nothing"), na = c("NA", "Nothing"))
parse_number <- function(x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE, .return_problems = FALSE) {
    parse_vector(x, col_number(), na = na, locale = locale, trim_ws = trim_ws, .return_problems = .return_problems)
}

#' @rdname parse_number
#' @export
col_number <- function() {
    collector("number")
}


#' Parse using the "best" type
#'
#' `parse_guess()` returns the parser vector. This function uses a number of heuristics
#' to determine which type of vector is "best". Generally they try to err of
#' the side of safety, as it's straightforward to override the parsing choice
#' if needed.
#'
#' @inheritParams parse_atomic
#' @param guess_integer If `TRUE`, guess integer types for whole numbers, if
#'   `FALSE` guess numeric type for all numbers.
#' @param guess_max Maximum number of data rows to use for guessing column types. `NA`: uses all data.
#' @family parsers
#' @return a parsed vector
#' @export
#' @examples
#' # Logical vectors
#' parse_guess(c("FALSE", "TRUE", "F", "T"))
#'
#' # Integers and doubles
#' parse_guess(c("1", "2", "3"))
#' parse_guess(c("1.6", "2.6", "3.4"))
#'
#' # Numbers containing grouping mark
#' parse_guess("1,234,566")
#'
#' # ISO 8601 date times
#' parse_guess(c("2010-10-10"))
parse_guess <- function(x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE, guess_integer = FALSE, guess_max = NA, .return_problems = FALSE) {
    parse_vector(x, guess_parser(x, locale, guess_integer = guess_integer, na = na, guess_max = guess_max), na = na, locale = locale, trim_ws = trim_ws,
                 .return_problems = .return_problems)
}

#' @rdname parse_guess
#' @export
col_guess <- function() {
    collector("guess")
}

guess_parser <- function(x, locale = default_locale(), guess_integer = FALSE, na = c("", "NA"), guess_max = 1000) {
    x[x %in% na] <- NA_character_
    stopifnot(is.locale(locale))
    if (is.na(guess_max)) {
        guess_max <- NA_integer_
    }
    stopifnot(is.numeric(guess_max))
    if (abs(guess_max) == Inf || is.nan(guess_max) || guess_max < 1 || is.na(guess_max)) {
        guess_max <- length(x)
    }
    collectorGuess(x, locale, guessInteger = guess_integer, as.integer(guess_max))
}

#' Parse factors
#'
#' `parse_factor()` is similar to [factor()].
#'
#' @param levels Character vector of the allowed levels. When `levels = NULL`
#'   (the default), `levels` are discovered from the unique values of `x`, in
#'   the order in which they appear in `x`.
#' @param ordered Is it an ordered factor?
#' @param include_na If `TRUE` and `x` contains at least one `NA`, then `NA`
#'   is included in the levels of the constructed factor.
#'
#' @inheritParams parse_atomic
#' @return a parsed vector
#' @family parsers
#' @export
#' @examples
#' # discover the levels from the data
#' parse_factor(c("a", "b"))
#' parse_factor(c("a", "b", "-99"))
#' parse_factor(c("a", "b", "-99"), na = c("", "NA", "-99"))
#' parse_factor(c("a", "b", "-99"), na = c("", "NA", "-99"), include_na = FALSE)
#'
#' # provide the levels explicitly
#' parse_factor(c("a", "b"), levels = letters[1:5])
#'
#' x <- c("cat", "dog", "caw")
#' animals <- c("cat", "dog", "cow")
#'
#' # base::factor() silently converts elements that do not match any levels to
#' # NA
#' factor(x, levels = animals)
#'
#' # parse_factor() generates same factor as base::factor() but throws a warning
#' # and reports problems
#' parse_factor(x, levels = animals)
parse_factor <- function(x, levels = NULL, ordered = FALSE, na = c("", "NA"),
                         locale = default_locale(), include_na = TRUE, trim_ws = TRUE, .return_problems = FALSE) {
    parse_vector(x, col_factor(levels, ordered, include_na), na = na, locale = locale, trim_ws = trim_ws,
                 .return_problems = .return_problems)
}

#' @rdname parse_factor
#' @export
col_factor <- function(levels = NULL, ordered = FALSE, include_na = FALSE) {
    if (!(is.null(levels) || is.character(levels))) {
        stop(sprintf("`levels` must be `NULL` or a character vector:\n- `levels` is a '%s'", class(levels)), call. = FALSE)
    }
    collector("factor", levels = levels, ordered = ordered, include_na = include_na)
}

## More complex ------------------------------------------------------------

#' Parse date/times
#'
#' @section Format specification:
#' `minty` (inherited from `readr`) uses a format specification similar to [strptime()].
#' There are three types of element:
#'
#' 1. Date components are specified with "%" followed by a letter. For example
#'   "%Y" matches a 4 digit year, "%m", matches a 2 digit month and "%d" matches
#'   a 2 digit day. Month and day default to `1`, (i.e. Jan 1st) if not present,
#'   for example if only a year is given.
#' 2. Whitespace is any sequence of zero or more whitespace characters.
#' 3. Any other character is matched exactly.
#'
#' `parse_datetime()` recognises the following format specifications:
#'
#' * Year: "%Y" (4 digits). "%y" (2 digits); 00-69 -> 2000-2069, 70-99 ->
#'   1970-1999.
#' * Month: "%m" (2 digits), "%b" (abbreviated name in current locale), "%B"
#'   (full name in current locale).
#' * Day: "%d" (2 digits), "%e" (optional leading space), "%a" (abbreviated
#'   name in current locale).
#' * Hour: "%H" or "%I" or "%h", use I (and not H) with AM/PM, use h (and not H)
#'   if your times represent durations longer than one day.
#' * Minutes: "%M"
#' * Seconds: "%S" (integer seconds), "%OS" (partial seconds)
#' * Time zone: "%Z" (as name, e.g. "America/Chicago"), "%z" (as offset from
#'   UTC, e.g. "+0800")
#' * AM/PM indicator: "%p".
#' * Non-digits: "%." skips one non-digit character, "%+" skips one or more
#'   non-digit characters, "%*" skips any number of non-digits characters.
#' * Automatic parsers: "%AD" parses with a flexible YMD parser, "%AT" parses
#'   with a flexible HMS parser.
#' * Time since the Unix epoch: "%s" decimal seconds since the Unix epoch.
#' * Shortcuts: "%D" = "%m/%d/%y", "%F" = "%Y-%m-%d", "%R" = "%H:%M", "%T" =
#'   "%H:%M:%S", "%x" = "%y/%m/%d".
#'
#' @section ISO8601 support:
#'
#' Currently, `minty` does not support all of ISO8601. Missing features:
#'
#' * Week & weekday specifications, e.g. "2013-W05", "2013-W05-10".
#' * Ordinal dates, e.g. "2013-095".
#' * Using commas instead of a period for decimal separator.
#'
#' The parser is also a little laxer than ISO8601:
#'
#' * Dates and times can be separated with a space, not just T.
#' * Mostly correct specifications like "2009-05-19 14:" and "200912-01" work.
#'
#' @param x A character vector of dates to parse.
#' @param format A format specification, as described below. If set to "",
#'   date times are parsed as ISO8601, dates and times used the date and
#'   time formats specified in the [locale()].
#'
#'   Unlike [strptime()], the format specification must match
#'   the complete string.
#' @return A [POSIXct()] vector with `tzone` attribute set to
#'   `tz`. Elements that could not be parsed (or did not generate valid
#'   dates) will be set to `NA`, and a warning message will inform
#'   you of the total number of failures.
#' @family parsers
#' @inheritParams parse_atomic
#' @export
#' @examples
#' # Format strings --------------------------------------------------------
#' parse_datetime("01/02/2010", "%d/%m/%Y")
#' parse_datetime("01/02/2010", "%m/%d/%Y")
#' # Handle any separator
#' parse_datetime("01/02/2010", "%m%.%d%.%Y")
#'
#' # Dates look the same, but internally they use the number of days since
#' # 1970-01-01 instead of the number of seconds. This avoids a whole lot
#' # of troubles related to time zones, so use if you can.
#' parse_date("01/02/2010", "%d/%m/%Y")
#' parse_date("01/02/2010", "%m/%d/%Y")
#'
#' # You can parse timezones from strings (as listed in OlsonNames())
#' parse_datetime("2010/01/01 12:00 US/Central", "%Y/%m/%d %H:%M %Z")
#' # Or from offsets
#' parse_datetime("2010/01/01 12:00 -0600", "%Y/%m/%d %H:%M %z")
#'
#' # Use the locale parameter to control the default time zone
#' # (but note UTC is considerably faster than other options)
#' parse_datetime("2010/01/01 12:00", "%Y/%m/%d %H:%M",
#'   locale = locale(tz = "US/Central")
#' )
#' parse_datetime("2010/01/01 12:00", "%Y/%m/%d %H:%M",
#'   locale = locale(tz = "US/Eastern")
#' )
#'
#' # Unlike strptime, the format specification must match the complete
#' # string (ignoring leading and trailing whitespace). This avoids common
#' # errors:
#' strptime("01/02/2010", "%d/%m/%y")
#' parse_datetime("01/02/2010", "%d/%m/%y")
#'
#' # Failures -------------------------------------------------------------
#' parse_datetime("01/01/2010", "%d/%m/%Y")
#' parse_datetime(c("01/ab/2010", "32/01/2010"), "%d/%m/%Y")
#'
#' # Locales --------------------------------------------------------------
#' # By default, readr expects English date/times, but that's easy to change'
#' parse_datetime("1 janvier 2015", "%d %B %Y", locale = locale("fr"))
#' parse_datetime("1 enero 2015", "%d %B %Y", locale = locale("es"))
#'
#' # ISO8601 --------------------------------------------------------------
#' # With separators
#' parse_datetime("1979-10-14")
#' parse_datetime("1979-10-14T10")
#' parse_datetime("1979-10-14T10:11")
#' parse_datetime("1979-10-14T10:11:12")
#' parse_datetime("1979-10-14T10:11:12.12345")
#'
#' # Without separators
#' parse_datetime("19791014")
#' parse_datetime("19791014T101112")
#'
#' # Time zones
#' us_central <- locale(tz = "US/Central")
#' parse_datetime("1979-10-14T1010", locale = us_central)
#' parse_datetime("1979-10-14T1010-0500", locale = us_central)
#' parse_datetime("1979-10-14T1010Z", locale = us_central)
#' # Your current time zone
#' parse_datetime("1979-10-14T1010", locale = locale(tz = ""))
parse_datetime <- function(x, format = "", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE, .return_problems = FALSE) {
    parse_vector(x, col_datetime(format), na = na, locale = locale, trim_ws = trim_ws, .return_problems = .return_problems)
}

#' @rdname parse_datetime
#' @export
parse_date <- function(x, format = "", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE, .return_problems = FALSE) {
    parse_vector(x, col_date(format), na = na, locale = locale, trim_ws = trim_ws, .return_problems = .return_problems)
}

#' @rdname parse_datetime
#' @export
parse_time <- function(x, format = "", na = c("", "NA"), locale = default_locale(), trim_ws = TRUE, .return_problems = FALSE) {
    parse_vector(x, col_time(format), na = na, locale = locale, trim_ws = trim_ws, .return_problems = .return_problems)
}

#' @rdname parse_datetime
#' @export
col_datetime <- function(format = "") {
    collector("datetime", format = format)
}

#' @rdname parse_datetime
#' @export
col_date <- function(format = "") {
    collector("date", format = format)
}

#' @rdname parse_datetime
#' @export
col_time <- function(format = "") {
    collector("time", format = format)
}

## Locale

#' Create locales
#'
#' A locale object tries to capture all the defaults that can vary between
#' countries. You set the locale in once, and the details are automatically
#' passed on down to the columns parsers. The defaults have been chosen to
#' match R (i.e. US English) as closely as possible.
#'
#' @param date_names Character representations of day and month names. Either
#'   the language code as string (passed on to [date_names_lang()])
#'   or an object created by [date_names()].
#' @param date_format,time_format Default date and time formats.
#' @param decimal_mark,grouping_mark Symbols used to indicate the decimal
#'   place, and to chunk larger numbers. Decimal mark can only be `,` or
#'   `.`.
#' @param tz Default tz. This is used both for input (if the time zone isn't
#'   present in individual strings), and for output (to control the default
#'   display). The default is to use "UTC", a time zone that does not use
#'   daylight savings time (DST) and hence is typically most useful for data.
#'   The absence of time zones makes it approximately 50x faster to generate
#'   UTC times than any other time zone.
#'
#'   Use `""` to use the system default time zone, but beware that this
#'   will not be reproducible across systems.
#'
#'   For a complete list of possible time zones, see [OlsonNames()].
#'   Americans, note that "EST" is a Canadian time zone that does not have
#'   DST. It is *not* Eastern Standard Time. It's better to use
#'   "US/Eastern", "US/Central" etc.
#' @param encoding Default encoding (not used in `minty`).
#' @param asciify Should diacritics be stripped from date names and converted to
#'   ASCII? This is useful if you're dealing with ASCII data where the correct
#'   spellings have been lost. Requires the \pkg{stringi} package.
#' @export
#' @return a list / S3 object representing the locale information
#' @examples
#' locale()
#' locale("fr")
#'
#' # South American locale
#' locale("es", decimal_mark = ",")
locale <- function(date_names = "en",
                   date_format = "%AD", time_format = "%AT",
                   decimal_mark = ".", grouping_mark = ",",
                   tz = "UTC", encoding = "UTF-8",
                   asciify = FALSE) {
    if (is.character(date_names)) {
        date_names <- date_names_lang(date_names)
    }
    stopifnot(is.date_names(date_names))
    if (asciify && !requireNamespace("stringi", quietly = TRUE)) {
        asciify <- FALSE
    }
    if (asciify) {
        date_names[] <- lapply(date_names, stringi::stri_trans_general, id = "latin-ascii")
    }

    if (missing(grouping_mark) && !missing(decimal_mark)) {
        grouping_mark <- if (decimal_mark == ".") "," else "."
    } else if (missing(decimal_mark) && !missing(grouping_mark)) {
        decimal_mark <- if (grouping_mark == ".") "," else "."
    }

    stopifnot(decimal_mark %in% c(".", ","))
    check_string(grouping_mark)
    if (decimal_mark == grouping_mark) {
        stop("`decimal_mark` and `grouping_mark` must be different", call. = FALSE)
    }

    tz <- check_tz(tz)
    check_encoding(encoding)

    structure(
        list(
            date_names = date_names,
            date_format = date_format,
            time_format = time_format,
            decimal_mark = decimal_mark,
            grouping_mark = grouping_mark,
            tz = tz,
            encoding = encoding
        ),
        class = "locale"
    )
}

is.locale <- function(x) inherits(x, "locale")

#' @export
#' @rdname locale
default_locale <- function() {
    loc <- getOption("readr.default_locale")
    if (is.null(loc)) {
        loc <- locale()
        options("readr.default_locale" = loc)
    }

    loc
}

check_tz <- function(x) {
    check_string(x, nm = "tz")

    if (identical(x, "")) {
        x <- Sys.timezone()

        if (identical(x, "") || identical(x, NA_character_)) {
            x <- "UTC"
        }
    }

    if (x %in% tzdb::tzdb_names()) {
        x
    } else {
        stop("Unknown TZ ", x, call. = FALSE)
    }
}

check_encoding <- function(x) {
    check_string(x, nm = "encoding")

    if (tolower(x) %in% tolower(iconvlist())) {
        return(TRUE)
    }

    stop("Unknown encoding ", x, call. = FALSE)
}

## datetime

#' Create or retrieve date names
#'
#' When parsing dates, you often need to know how weekdays of the week and
#' months are represented as text. This pair of functions allows you to either
#' create your own, or retrieve from a standard list. The standard list is
#' derived from ICU (`http://site.icu-project.org`) via the stringi package.
#'
#' @param mon,mon_ab Full and abbreviated month names.
#' @param day,day_ab Full and abbreviated week day names. Starts with Sunday.
#' @param am_pm Names used for AM and PM.
#' @return a list / S3 object representing data time
#' @export
#' @examples
#' date_names_lang("en")
#' date_names_lang("ko")
#' date_names_lang("fr")
date_names <- function(mon, mon_ab = mon, day, day_ab = day,
                       am_pm = c("AM", "PM")) {
    stopifnot(is.character(mon), length(mon) == 12)
    stopifnot(is.character(mon_ab), length(mon_ab) == 12)
    stopifnot(is.character(day), length(day) == 7)
    stopifnot(is.character(day_ab), length(day_ab) == 7)

    structure(
        list(
            mon = enc2utf8(mon),
            mon_ab = enc2utf8(mon_ab),
            day = enc2utf8(day),
            day_ab = enc2utf8(day_ab),
            am_pm = enc2utf8(am_pm)
        ),
        class = "date_names"
    )
}

#' @export
#' @rdname date_names
#' @param language A BCP 47 locale, made up of a language and a region,
#'   e.g. `"en"` for American English. See `date_names_langs()`
#'   for a complete list of available locales.
date_names_lang <- function(language) {
    check_string(language)

    symbols <- date_symbols[[language]]
    if (is.null(symbols)) {
        stop("Unknown language '", language, "'", call. = FALSE)
    }

    symbols
}

#' @export
#' @rdname date_names
date_names_langs <- function() {
    names(date_symbols)
}

is.date_names <- function(x) inherits(x, "date_names")

## col_spec

#' Create column specification
#'
#' `cols()` includes all columns in the input data, guessing the column types
#' as the default. `cols_only()` includes only the columns you explicitly
#' specify, skipping the rest. In general you can substitute `list()` for
#' `cols()` without changing the behavior.
#'
#' The available specifications are: (with string abbreviations in brackets)
#'
#' * `col_logical()` \[l\], containing only `T`, `F`, `TRUE` or `FALSE`.
#' * `col_integer()` \[i\], integers.
#' * `col_double()` \[d\], doubles.
#' * `col_character()` \[c\], everything else.
#' * `col_factor(levels, ordered)` \[f\], a fixed set of values.
#' * `col_date(format = "")` \[D\]: with the locale's `date_format`.
#' * `col_time(format = "")` \[t\]: with the locale's `time_format`.
#' * `col_datetime(format = "")` \[T\]: ISO8601 date times
#' * `col_number()` \[n\], numbers containing the `grouping_mark`
#' * `col_skip()` \[_, -\], don't import this column.
#' * `col_guess()` \[?\], parse using the "best" type based on the input.
#'
#' @family parsers
#' @param ... Either column objects created by `col_*()`, or their abbreviated
#'   character names (as described in the `col_types` argument of
#'   read_delim). If you're only overriding a few columns, it's
#'   best to refer to columns by name. If not named, the column types must match
#'   the column names exactly.
#' @param .default Any named columns not explicitly overridden in `...`
#'   will be read with this column type.
#' @export
#' @return a list / S3 object representing column specification
#' @examples
#' cols(a = col_integer())
#' cols_only(a = col_integer())
#'
#' # You can also use the standard abbreviations
#' cols(a = "i")
#' cols(a = "i", b = "d", c = "_")
#'
#' # You can also use multiple sets of column definitions by combining
#' # them like so:
#'
#' t1 <- cols(
#'   column_one = col_integer(),
#'   column_two = col_number()
#' )
#'
#' t2 <- cols(
#'   column_three = col_character()
#' )
#'
#' t3 <- t1
#' t3$cols <- c(t1$cols, t2$cols)
#' t3
cols <- function(..., .default = col_guess()) {
    col_types <- list(...)
    is_character <- vapply(col_types, is.character, logical(1))
    col_types[is_character] <- lapply(col_types[is_character], col_concise)
    if (is.character(.default)) {
        .default <- col_concise(.default)
    }
    return(col_spec(col_types, .default))
}

#' @export
#' @rdname cols
cols_only <- function(...) {
    cols(..., .default = col_skip())
}


## col_spec ----------------------------------------------------------------

col_spec <- function(col_types, default = col_guess()) {
    stopifnot(is.list(col_types))
    stopifnot(is.collector(default))

    is_collector <- vapply(col_types, is.collector, logical(1))
    if (any(!is_collector)) {
        stop("Some `col_types` are not S3 collector objects: ",
             paste(which(!is_collector), collapse = ", "),
             call. = FALSE
             )
    }

    structure(
        list(
            cols = col_types,
            default = default
        ),
        class = "col_spec"
    )
}

is.col_spec <- function(x) inherits(x, "col_spec")


#' Generate a column specification
#'
#' This is most useful for generating a specification using the short form
#' @param x Input object
#' @keywords internal
#' @examples
#' as.col_spec("cccnnn")
#' @return a list / S3 object representing column specification
#' @export
as.col_spec <- function(x) UseMethod("as.col_spec")
#' @export
as.col_spec.character <- function(x) {
    if (is_named(x)) {
        return(as.col_spec(as.list(x)))
    }
    letters <- strsplit(x, "")[[1]]
    col_spec(lapply(letters, col_concise), col_guess())
}

#' @export
as.col_spec.NULL <- function(x) {
    col_spec(list())
}

#' @export
as.col_spec.list <- function(x) {
    do.call(cols, x)
}
#' @export
as.col_spec.col_spec <- function(x) x

#' @export
as.col_spec.default <- function(x) {
    stop("`col_types` must be NULL, a list or a string", call. = FALSE)
}

type_to_col <- function(x, ...) {
    UseMethod("type_to_col")
}

#' @export
type_to_col.default <- function(x, ...) {
    col_character()
}

#' @export
type_to_col.logical <- function(x, ...) {
    col_logical()
}

#' @export
type_to_col.integer <- function(x, ...) {
    col_integer()
}

#' @export
type_to_col.double <- function(x, ...) {
    col_double()
}

#' @export
type_to_col.factor <- function(x, ...) {
    col_factor(levels = levels(x), ordered = is.ordered(x), include_na = any(is.na(levels(x))))
}

#' @export
type_to_col.Date <- function(x, ...) {
    col_date()
}

#' @export
type_to_col.POSIXct <- function(x, ...) {
    col_datetime()
}

#' @export
type_to_col.hms <- function(x, ...) {
    col_time()
}

#' @export
as.col_spec.data.frame <- function(x) {
    as.col_spec(lapply(x, type_to_col))
}

col_to_short <- function(x, ...) {
    switch(class(x)[[1]],
           collector_character = "c",
           collector_date = "D",
           collector_datetime = "T",
           collector_double = "d",
           collector_factor = "f",
           collector_guess = "?",
           collector_integer = "i",
           collector_logical = "l",
           collector_number = "n",
           collector_skip = "-",
           collector_time = "t"
           )
}

cols_condense <- function(x) {
    types <- vapply(x$cols, function(xx) class(xx)[[1]], character(1))
    counts <- table(types)
    most_common <- names(counts)[counts == max(counts)][[1]]

    x$default <- x$cols[types == most_common][[1]]
    x$cols <- x$cols[types != most_common]
    x
}

## Change from S3
format_col_spec <- function(x, n = Inf, condense = NULL, ...) {
    if (n == 0) {
        return("")
    }

    ## condense if cols >= n
    condense <- condense %||% (length(x$cols) >= n)
    if (isTRUE(condense)) {
        x <- cols_condense(x)
    }

    ## truncate to minumum of n or length
    cols <- x$cols[seq_len(min(length(x$cols), n))]

    default <- NULL
    if (inherits(x$default, "collector_guess")) {
        fun_type <- "cols"
    } else if (inherits(x$default, "collector_skip")) {
        fun_type <- "cols_only"
    } else {
        fun_type <- "cols"
        type <- sub("^collector_", "", class(x$default)[[1]])
        default <- paste0(".default = col_", type, "()")
    }

    cols_args <- c(
        default,
        vapply(
            seq_along(cols),
            function(i) {
                col_funs <- sub("^collector_", "col_", class(cols[[i]])[[1]])
                args <- vapply(cols[[i]], deparse2, character(1), sep = "\n    ")
                args <- paste(names(args), args, sep = " = ", collapse = ", ")

                col_funs <- paste0(col_funs, "(", args, ")")

                col_names <- names(cols)[[i]] %||% ""

                ## Need to handle unnamed columns and columns with non-syntactic names
                named <- col_names != ""

                non_syntactic <- !is_syntactic(col_names) & named
                col_names[non_syntactic] <- paste0("`", gsub("`", "\\\\`", col_names[non_syntactic]), "`")

                out <- paste0(col_names, " = ", col_funs)
                out[!named] <- col_funs[!named]
                out
            },
            character(1)
        )
    )
    if (length(x$cols) == 0 && length(cols_args) == 0) {
        return(paste0(fun_type, "()\n"))
    }


    out <- paste0(fun_type, "(\n  ", paste(collapse = ",\n  ", cols_args))

    if (length(x$cols) > n) {
        out <- paste0(out, "\n  # ... with ", length(x$cols) - n, " more columns")
    }
    out <- paste0(out, "\n)\n")

    out
}

col_concise <- function(x) {
    switch(x,
           "_" = ,
           "-" = col_skip(),
           "?" = col_guess(),
           c = col_character(),
           f = col_factor(),
           d = col_double(),
           i = col_integer(),
           l = col_logical(),
           n = col_number(),
           D = col_date(),
           T = col_datetime(),
           t = col_time(),
           stop("Unknown shortcut: ", x, call. = FALSE)
           )
}

col_spec_standardise <- function(col_names = TRUE, col_types = NULL,
                                 guessed_types = NULL) {
    ## Figure out column types ----------------------------------------------------

    spec <- as.col_spec(col_types)
    type_names <- names(spec$cols)
    if (length(spec$cols) == 0) {
        ## no types specified so use defaults

        spec$cols <- rep(list(spec$default), length(col_names))
        names(spec$cols) <- col_names
        return(resolve_guess_cols(spec, guessed_types))
    }
    if (is.null(type_names)) {
        ## cases like "?-" or list("?", "-")
        names(spec$cols) <- col_names
        return(resolve_guess_cols(spec, guessed_types))
    }
    ## names types
    bad_types <- !(type_names %in% col_names)
    if (any(bad_types)) {
        warning("The following named parsers don't match the column names: ",
                paste0(type_names[bad_types], collapse = ", "),
                call. = FALSE
                )
        spec$cols <- spec$cols[!bad_types]
        type_names <- type_names[!bad_types]
    }

    default_types <- !(col_names %in% type_names)
    if (any(default_types)) {
        defaults <- rep(list(spec$default), sum(default_types))
        names(defaults) <- col_names[default_types]
        spec$cols[names(defaults)] <- defaults
    }

    spec$cols <- spec$cols[col_names]
    return(resolve_guess_cols(spec, guessed_types))
}

## utils

resolve_guess_cols <- function(spec, guessed_types) {
    ## Guess any types that need to be guessed ------------------------------------

    is_guess <- vapply(spec$cols, function(x) inherits(x, "collector_guess"), logical(1))
    if (any(is_guess)) {
        ## Need to be careful here: there might be more guesses than types/names
        guesses <- guessed_types[seq_along(spec$cols)][is_guess]
        spec$cols[is_guess] <- lapply(guesses, collector_find)
    }
    return(spec)
}

check_string <- function(x, nm = deparse(substitute(x)), optional = FALSE) {
    if (r_is_string_cpp11(x)) {
        return()
    }
    if (optional && is.null(x)) {
        return()
    }
    stop("`", nm, "` must be a string.", call. = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

deparse2 <- function(expr, ..., sep = "\n") {
    paste(deparse(expr, ...), collapse = sep)
}

is_syntactic <- function(x) make.names(x) == x

is_named <- function(x) {
    nms <- names(x)

    if (is.null(nms)) {
        return(FALSE)
    }

    all(nms != "" & !is.na(nms))
}

POSIXct <- function(x, tz = "UTC") {
    structure(x, class = c("POSIXct", "POSIXt"), tzone = tz)
}

## data symbol creation

## library(stringi)

## locs <- stri_locale_list()
## base <- unique(stri_split_fixed(locs, "_", n = 2, simplify = TRUE)[, 1])

## locale_info <- function(x) {
##   full <- stri_datetime_symbols(x, context = "format", width = "wide")
##   abbr <- stri_datetime_symbols(x, context = "format", width = "abbreviated")

##   date_names(
##     mon = full$Month,
##     mon_ab = abbr$Month,
##     day = full$Weekday,
##     day_ab = abbr$Weekday,
##     am_pm = full$AmPm
##   )
## }

## date_symbols <- lapply(base, locale_info)
## names(date_symbols) <- base

## usethis::use_data(date_symbols, internal = TRUE, overwrite = TRUE)
