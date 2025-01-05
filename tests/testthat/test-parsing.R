test_that("trimmed before NA detection", {
  expect_equal(parse_logical(c(" TRUE ", "FALSE", " NA ")), c(TRUE, FALSE, NA))
})

test_that("parse_guess() guess_max", {
    ## weird input
    expected_output <- c(1, 2, 3)
    expect_error(parse_guess(c("1", "2", "3"), guess_max = "123"))
    expect_equal(parse_guess(c("1", "2", "3"), guess_max = NA), expected_output)
    expect_equal(parse_guess(c("1", "2", "3"), guess_max = Inf), expected_output)
    expect_equal(parse_guess(c("1", "2", "3"), guess_max = -Inf), expected_output)
    expect_equal(parse_guess(c("1", "2", "3"), guess_max = 3.14), expected_output)
    expect_equal(parse_guess(c("1", "2", "3"), guess_max = -3.14), expected_output)
    expect_equal(parse_guess(c("1", "2", "3"), guess_max = 0), expected_output)
    ## Off by one?
    expect_equal(class(parse_guess(c("1", "2", "abc"), guess_max = 2)), "numeric")
    expect_equal(class(parse_guess(c("1", "2", "abc"), guess_max = 3)), "character")
})

test_that("parse_guess() trim_ws #32 or tidyverse/readr#1536", {
    expect_equal(parse_guess(c(" 1", "2 ", " 3 "), trim_ws = TRUE), c(1, 2, 3))
    expect_equal(parse_guess(c(" 1", "2 ", " 3 "), trim_ws = FALSE), c(" 1", "2 ", " 3 "))
    ## exclusive leading and trim_ws = FALSE, won't be parsed as numeric
    expect_equal(parse_guess(c(" 1", "   2", "     3"), trim_ws = FALSE), c(" 1", "   2", "     3"))
    expect_equal(parse_guess(c(" TRUE", "FALSE ", " T "), trim_ws = TRUE), c(TRUE, FALSE, TRUE))
})

test_that("all empty with trim_ws won't crash, #37 and ropensci/readODS#211", {
    expect_error(minty:::guess_parser(c(" ", "     "), trim_ws = TRUE), NA)
    expect_error(minty:::guess_parser(c(" "), trim_ws = TRUE), NA)
    expect_equal(minty:::guess_parser(c(" "), trim_ws = TRUE), "logical")
    expect_equal(minty:::guess_parser(c(" ", "   "), trim_ws = TRUE), "logical")
    expect_error(minty:::guess_parser(c(" "), trim_ws = FALSE), NA)
    expect_error(minty:::guess_parser(c(" ", "a"), trim_ws = FALSE), NA)
    expect_equal(minty:::guess_parser(c(" ", "a")), "character")
})
