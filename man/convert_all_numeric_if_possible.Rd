% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_flextable.R
\name{convert_all_numeric_if_possible}
\alias{convert_all_numeric_if_possible}
\title{Flextable Creation and Formatting Functions}
\usage{
convert_all_numeric_if_possible(df)
}
\arguments{
\item{df}{A data frame to process}
}
\value{
Data frame with numeric columns converted where appropriate
}
\description{
A comprehensive set of functions for creating and formatting flextables
with various layouts, nested headers, and styling options.
Convert All Numeric Columns If Possible
}
\details{
Attempts to convert all columns in a data frame to numeric where possible,
while preserving the original data type if conversion would introduce NAs.
Special handling for 'jahr' column to keep as character.
}
