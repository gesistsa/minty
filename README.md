
<!-- README.md is generated from README.Rmd. Please edit that file -->

# minty <img src="man/figures/logo.png" align="right" height="138" alt = ""/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/chainsawriot/minty/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/chainsawriot/minty/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`minty` (**Min**imal **ty**pe Inferencing and Parsing Tools) is an
experimental package to spin off the type inferencing and parsing tools
(the so-called 1e parsing engine) from `readr` (see this issue
[tidyverse/readr\#1517](https://github.com/tidyverse/readr/issues/1517)).
Since July 2021, these tools are not used internally by `readr` for
parsing text files. Now `vroom` is used by default, unless explicitly
call the first edition parsing engine (see the explanation on
[editions](https://github.com/tidyverse/readr?tab=readme-ov-file#editions)).

`readr`’s 1e type inferencing and parsing tools are used by various R
packages, e.g. `readODS` and `surveytoolbox`, but ironically those
packages do not use the main functions (e.g. `readr::read_delim()`) of
`readr`. As explained in the README of `readr`, those 1e code will be
eventually removed from `readr`.

`minty` aims at providing a set of minimal, long-term, and compatible
type inferencing and parsing tools for those packages. If you need to
parse interactively, please use either `readr` or `vroom`.

## Installation

You can install the development version of minty like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

A character-only data.frame

``` r
text_only <- data.frame(maybe_age = c("17", "18", "019"),
                        maybe_male = c("true", "false", "true"),
                        maybe_name = c("AA", "BB", "CC"),
                        some_na = c("NA", "Not good", "Bad"),
                        dob = c("2019/07/21", "2019/08/31", "2019/10/01"))
str(text_only)
#> 'data.frame':    3 obs. of  5 variables:
#>  $ maybe_age : chr  "17" "18" "019"
#>  $ maybe_male: chr  "true" "false" "true"
#>  $ maybe_name: chr  "AA" "BB" "CC"
#>  $ some_na   : chr  "NA" "Not good" "Bad"
#>  $ dob       : chr  "2019/07/21" "2019/08/31" "2019/10/01"
```

``` r
## built-in function type.convert:
## except numeric, no type inferencing
str(type.convert(text_only, as.is = TRUE))
#> 'data.frame':    3 obs. of  5 variables:
#>  $ maybe_age : int  17 18 19
#>  $ maybe_male: chr  "true" "false" "true"
#>  $ maybe_name: chr  "AA" "BB" "CC"
#>  $ some_na   : chr  NA "Not good" "Bad"
#>  $ dob       : chr  "2019/07/21" "2019/08/31" "2019/10/01"
```

Inferencing the column types

``` r
library(minty, warn.conflicts = FALSE)
data <- type_convert(text_only)
data
#>   maybe_age maybe_male maybe_name  some_na        dob
#> 1        17       TRUE         AA     <NA> 2019-07-21
#> 2        18      FALSE         BB Not good 2019-08-31
#> 3       019       TRUE         CC      Bad 2019-10-01
```

``` r
str(data)
#> 'data.frame':    3 obs. of  5 variables:
#>  $ maybe_age : chr  "17" "18" "019"
#>  $ maybe_male: logi  TRUE FALSE TRUE
#>  $ maybe_name: chr  "AA" "BB" "CC"
#>  $ some_na   : chr  NA "Not good" "Bad"
#>  $ dob       : Date, format: "2019-07-21" "2019-08-31" ...
```

### Type-based parsing tools

``` r
parse_datetime("1979-10-14T10:11:12.12345")
#> [1] "1979-10-14 10:11:12 UTC"
```

``` r
fr <- locale("fr")
parse_date("1 janv. 2010", "%d %b %Y", locale = fr)
#> [1] "2010-01-01"
```

``` r
de <- locale("de", decimal_mark = ",")
parse_number("1.697,31", local = de)
#> [1] 1697.31
```

``` r
parse_number("$1,123,456.00")
#> [1] 1123456
```

``` r
## This is perhaps Python
parse_logical(c("True", "False"))
#> [1]  TRUE FALSE
```

### Type guesser

``` r
parse_guess(c("True", "TRUE", "false", "F"))
#> [1]  TRUE  TRUE FALSE FALSE
```

``` r
parse_guess(c("123.45", "1990", "7619.0"))
#> [1]  123.45 1990.00 7619.00
```

``` r
res <- parse_guess(c("2019-07-21", "2019-08-31", "2019-10-01", "IDK"), na = "IDK")
res
#> [1] "2019-07-21" "2019-08-31" "2019-10-01" NA
```

``` r
str(res)
#>  Date[1:4], format: "2019-07-21" "2019-08-31" "2019-10-01" NA
```

## Differences: `readr` vs `minty`

Unlike `readr` and `vroom`, please note that `minty` is mainly for
**non-interactive usage**. Therefore, `minty` emits fewer messages and
warnings than `readr` and `vroom`.

``` r
data <- minty::type_convert(text_only)
data
#>   maybe_age maybe_male maybe_name  some_na        dob
#> 1        17       TRUE         AA     <NA> 2019-07-21
#> 2        18      FALSE         BB Not good 2019-08-31
#> 3       019       TRUE         CC      Bad 2019-10-01
```

``` r
data <- readr::type_convert(text_only)
#> Registered S3 methods overwritten by 'readr':
#>   method                from 
#>   as.character.col_spec minty
#>   format.col_spec       minty
#>   print.col_spec        minty
#>   print.collector       minty
#>   print.date_names      minty
#>   print.locale          minty
#>   str.col_spec          minty
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   maybe_age = col_character(),
#>   maybe_male = col_logical(),
#>   maybe_name = col_character(),
#>   some_na = col_character(),
#>   dob = col_date(format = "")
#> )
data
#>   maybe_age maybe_male maybe_name  some_na        dob
#> 1        17       TRUE         AA     <NA> 2019-07-21
#> 2        18      FALSE         BB Not good 2019-08-31
#> 3       019       TRUE         CC      Bad 2019-10-01
```

`verbose` option is added if you like those messages, default to
`FALSE`.

``` r
data <- minty::type_convert(text_only, verbose = TRUE)
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   maybe_age = col_character(),
#>   maybe_male = col_logical(),
#>   maybe_name = col_character(),
#>   some_na = col_character(),
#>   dob = col_date(format = "")
#> )
```

At the moment, `minty` does not use [the `problems`
mechanism](https://vroom.r-lib.org/reference/problems.html) by default.

``` r
minty::parse_logical(c("true", "fake", "IDK"), na = "IDK")
#> [1] TRUE   NA   NA
```

``` r
readr::parse_logical(c("true", "fake", "IDK"), na = "IDK")
#> Warning: 1 parsing failure.
#> row col           expected actual
#>   2  -- 1/0/T/F/TRUE/FALSE   fake
#> [1] TRUE   NA   NA
#> attr(,"problems")
#> # A tibble: 1 × 4
#>     row   col expected           actual
#>   <int> <int> <chr>              <chr> 
#> 1     2    NA 1/0/T/F/TRUE/FALSE fake
```

## Similar packages

For parsing ambiguous date(time)

  - [timeless](https://github.com/schochastics/timeless)
  - [anytime](https://github.com/eddelbuettel/anytime)
