% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_tableview.R
\name{mod_tableview_ui}
\alias{mod_tableview_ui}
\title{Table View UI Module}
\usage{
mod_tableview_ui(id)
}
\arguments{
\item{id}{A unique identifier for the module namespace.}
}
\value{
A UI definition (`tagList`) for use in a Shiny app.
}
\description{
Constructs the user interface for the table viewing module, including a tabbed layout
for displaying both a formatted table (`flextable`) and a raw data table (via DT).
}
