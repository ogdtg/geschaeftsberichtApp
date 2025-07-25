% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_flextable.R
\name{create_flextable_grgeko_summary}
\alias{create_flextable_grgeko_summary}
\title{Create GRGEKO Summary Flextable}
\usage{
create_flextable_grgeko_summary(template_data, year)
}
\arguments{
\item{template_data}{A data frame containing Status and Anzahl columns}

\item{year}{The reporting year for status labels}
}
\value{
A formatted flextable object without headers
}
\description{
Creates a specialized summary table for GRGEKO reports with year-specific status entries.
}
