% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_flextable.R
\name{create_flextable_simple}
\alias{create_flextable_simple}
\title{Create Simple Flextable}
\usage{
create_flextable_simple(
  template_data,
  total_row = FALSE,
  total_col = FALSE,
  subtotals = FALSE
)
}
\arguments{
\item{template_data}{A data frame to be formatted}

\item{total_row}{Logical, whether to add a summary row}

\item{total_col}{Logical, whether to add a summary column}

\item{subtotals}{Logical, whether to add subtotal formatting}
}
\value{
A formatted flextable object
}
\description{
Builds a basic flextable object from a wide-format data frame, with optional row and column totals.
}
