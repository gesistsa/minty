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
