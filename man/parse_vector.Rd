% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parser.R
\name{parse_vector}
\alias{parse_vector}
\title{Parse a character vector.}
\usage{
parse_vector(
  x,
  collector,
  na = c("", "NA"),
  locale = default_locale(),
  trim_ws = TRUE,
  .return_problems = FALSE
)
}
\arguments{
\item{x}{Character vector of elements to parse.}

\item{collector}{Column specification.}

\item{.return_problems}{Whether to hide the \code{problems} tibble from the output}
}
\value{
a parsed vector
}
\description{
Parse a character vector.
}
\examples{
x <- c("1", "2", "3", "NA")
parse_vector(x, col_integer())
parse_vector(x, col_double())
}
\seealso{
Other parsers: 
\code{\link{col_skip}()},
\code{\link{parse_datetime}()},
\code{\link{parse_factor}()},
\code{\link{parse_guess}()},
\code{\link{parse_logical}()},
\code{\link{parse_number}()}
}
\concept{parsers}
\keyword{internal}
