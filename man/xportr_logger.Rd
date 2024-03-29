% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/messages.R
\name{xportr_logger}
\alias{xportr_logger}
\title{Utility Logging Function}
\usage{
xportr_logger(message, type = "none", ...)
}
\arguments{
\item{message}{Output to be sent out for user}

\item{type}{Three types: abort, warn, inform}

\item{...}{additional arguments if needed}
}
\value{
Output to Console
}
\description{
Functions to output user messages, usually relating to differences
found between dataframe and the metacore/metadata object
}
