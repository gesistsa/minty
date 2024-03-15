
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
type inferencing and parsing tools for those packages.

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
