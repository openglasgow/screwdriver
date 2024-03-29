% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_num_rows.R
\name{check_num_rows}
\alias{check_num_rows}
\title{Check the Number of Rows in a Data Frame}
\usage{
check_num_rows(df, num_rows, df_name = "df")
}
\arguments{
\item{df}{A data frame whose rows are to be checked.}

\item{num_rows}{A single positive whole number or a vector of length 2
indicating the exact, minimum, or maximum number of rows allowed. Use \code{NA}
to omit the minimum or maximum constraint.}

\item{df_name}{Optional character string specifying the name of the data frame
being checked. This name is used in error messages for clarity.}
}
\value{
\code{TRUE} if the data frame meets the row count criteria, otherwise
an error is thrown detailing the discrepancy.
}
\description{
Validates the number of rows in a given data frame against specified criteria.
}
\examples{
df_example <- data.frame(x = 1:10)
check_num_rows(df_example, 10, df_name = "df_example") # Exact number of rows
check_num_rows(df_example, c(5, 15), df_name = "df_example") # Range of rows
check_num_rows(df_example, c(NA, 20), df_name = "df_example") # Maximum number of rows
check_num_rows(df_example, c(5, NA), df_name = "df_example") # Minimum number of rows

}
