
<!-- README.md is generated from README.Rmd. Please edit that file -->

# minty

<!-- badges: start -->

<!-- badges: end -->

`minty` (**Min**imal **TY**pe Inferencing and Parsing Tools) is an
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
text_only <- data.frame(maybe_age = c("17", "18", "20"),
                        maybe_male = c("true", "false", "true"),
                        maybe_name = c("AA", "BB", "CC"),
                        some_na = c("NA", "Not good", "Bad"),
                        dob = c("2019/07/21", "2019/08/31", "2019/10/01"))
str(text_only)
#> 'data.frame':    3 obs. of  5 variables:
#>  $ maybe_age : chr  "17" "18" "20"
#>  $ maybe_male: chr  "true" "false" "true"
#>  $ maybe_name: chr  "AA" "BB" "CC"
#>  $ some_na   : chr  "NA" "Not good" "Bad"
#>  $ dob       : chr  "2019/07/21" "2019/08/31" "2019/10/01"
```

Inferencing the column types

``` r
library(minty)
data <- type_convert(text_only)
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
#>   maybe_age = col_double(),
#>   maybe_male = col_logical(),
#>   maybe_name = col_character(),
#>   some_na = col_character(),
#>   dob = col_date(format = "")
#> )
data
#>   maybe_age maybe_male maybe_name  some_na        dob
#> 1        17       TRUE         AA     <NA> 2019-07-21
#> 2        18      FALSE         BB Not good 2019-08-31
#> 3        20       TRUE         CC      Bad 2019-10-01
```

``` r
str(data)
#> 'data.frame':    3 obs. of  5 variables:
#>  $ maybe_age : num  17 18 20
#>  $ maybe_male: logi  TRUE FALSE TRUE
#>  $ maybe_name: chr  "AA" "BB" "CC"
#>  $ some_na   : chr  NA "Not good" "Bad"
#>  $ dob       : Date, format: "2019-07-21" "2019-08-31" ...
```
