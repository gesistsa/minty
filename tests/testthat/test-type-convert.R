test_that("missing values removed before guessing col type", {
    df1 <- data.frame(x = c("NA", "10"), stringsAsFactors = FALSE)
    df2 <- type_convert(df1)

    expect_equal(df2$x, c(NA, 10L))
})

test_that("requires data.frame input", {
    not_df <- matrix(letters[1:4], nrow = 2)
    expect_error(type_convert(not_df), "is.data.frame")
})

test_that("col_types accepts character specifications", {
    df <- data.frame(x = 1:3, y = "3", z = "a", stringsAsFactors = FALSE)
    df_conv <- data.frame(x = 1:3, y = 3L, z = "a", stringsAsFactors = FALSE)

    expect_error(type_convert(df, col_types = "i"), "must have consistent lengths")

    expect_error(type_convert(df, col_types = c("i", "b")), "must be a single string")

    expect_equal(type_convert(df, col_types = "_ic"), df_conv)
})
test_that("col_types accepts cols specifications", {
    df <- data.frame(x = 1:3, y = "3", z = "a", stringsAsFactors = FALSE)
    df_conv <- data.frame(x = 1:3, y = 3L, z = "a", stringsAsFactors = FALSE)

    expect_equal(type_convert(df, col_types = cols(y = "i")), df_conv)

    expect_equal(type_convert(df, col_types = cols(y = col_integer(), z = col_character())), df_conv)

    ## non-character cols silently ignored
    expect_equal(type_convert(df, col_types = cols(x = "c", y = "i")), df_conv)
})

test_that("warning is thrown if there are no character columns (1020)", {
    expect_warning(type_convert(mtcars), "only converts columns")
})

test_that("guess_integer is implemented", {
    df <- data.frame(
        a = c("a", "b", "c"),
        b = c("1", "0", "-12"),
        c = c("1", "0", ".00001"),
        stringsAsFactors = FALSE
    )

    exp <- data.frame(
        a = c("a", "b", "c"),
        b = c(1L, 0L, -12L),
        c = c(1, 0, .00001),
        stringsAsFactors = FALSE
    )

    expect_identical(type_convert(df, guess_integer = TRUE), exp)
})

test_that("skip behaviors, readr#1509 or minty#20", {
    df <- data.frame(x = 1:3, y = "3", z = "a", stringsAsFactors = FALSE)
    ## correct blockage for character col_types
    expect_error(type_convert(df, col_types = "_"))
    expect_error(type_convert(df, col_types = "__")) ## even with a non-character column
    expect_error(type_convert(df, col_types = "____"))
    expect_error(type_convert(df, col_types = "___"), NA)

    ## dput(as.data.frame(sapply(head(PlantGrowth), as.character)))
    text_only <- structure(list(weight = c("4.17", "5.58", "5.18", "6.11", "4.5",  "4.61"),
                                group = c("ctrl", "ctrl", "ctrl", "ctrl", "ctrl", "ctrl")),
                           class = "data.frame", row.names = c(NA, -6L))

    expect_error(type_convert(text_only, col_types = "?"))
    expect_error(type_convert(text_only, col_types = "???"))
    expect_error(type_convert(text_only, col_types = "??"), NA)
    ## skipping
    expect_error(type_convert(text_only, col_types = "?_"), NA)
    expect_error(type_convert(text_only, col_types = "__"), NA)
    expect_error(minty::type_convert(text_only, col_types = list("?", "-")), NA)
    expect_error(minty::type_convert(text_only, col_types = list("-", "-")), NA)
})

test_that("type_convert() trim_ws #32 or tidyverse/readr#1536", {
    ## integration of guess_parse in type_convert()
    x <- type_convert(data.frame(a = c("1 ", "  1"), b = c(" 2", "    2")), trim_ws = TRUE)
    expect_equal(class(x$a), "numeric")
    expect_equal(class(x$b), "numeric")
    x <- type_convert(data.frame(a = c("  1")), trim_ws = FALSE)
    expect_equal(class(x$a), "character")
})

test_that("r_is_string_cpp11", {
    expect_true(r_is_string_cpp11("a"))
    expect_true(r_is_string_cpp11(c("a")))
    expect_false(r_is_string_cpp11(123))
    expect_false(r_is_string_cpp11(c(123, 123)))
    expect_false(r_is_string_cpp11(TRUE))
    expect_false(r_is_string_cpp11(c(TRUE, FALSE)))
    expect_false(r_is_string_cpp11(c("a", "b")))
    expect_false(r_is_string_cpp11(NA))
    expect_false(r_is_string_cpp11(NA_character_))
    expect_false(r_is_string_cpp11(NULL))
})

test_that("integration tests for #37, also ropensci/readODS#211", {
    input <- structure(list(picture_archive_url = c("", "", "", "", ""),
                            video_url = c(" ", " ", " ", " ", " ")),
                       row.names = c(NA, -5L), class = "data.frame")

    expect_error(type_convert(input, trim_ws = TRUE), NA)
    output <- type_convert(input, trim_ws = TRUE)
    expect_true(is.logical(output$video_url))
    output <- type_convert(input, trim_ws = FALSE)
    expect_false(is.logical(output$video_url))
})
