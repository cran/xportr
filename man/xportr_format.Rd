% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format.R
\name{xportr_format}
\alias{xportr_format}
\title{Assign SAS Format}
\usage{
xportr_format(.df, metadata = NULL, domain = NULL, metacore = deprecated())
}
\arguments{
\item{.df}{A data frame of CDISC standard.}

\item{metadata}{A data frame containing variable level metadata. See
'Metadata' section for details.}

\item{domain}{Appropriate CDSIC dataset name, e.g. ADAE, DM. Used to subset
the metadata object. If none is passed, then name of the dataset passed as
.df will be used.}

\item{metacore}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Previously used to pass
metadata now renamed with \code{metadata}}
}
\value{
Data frame with \code{SASformat} attributes for each variable.
}
\description{
Assigns a SAS format from a variable level metadata to a given data frame. If
no format is found for a given variable, it is set as an empty character
vector. This is stored in the format.sas attribute.
}
\section{Metadata}{
 The argument passed in the 'metadata' argument can either
be a metacore object, or a data.frame containing the data listed below. If
metacore is used, no changes to options are required.

For data.frame 'metadata' arguments three columns must be present:
\enumerate{
\item Domain Name - passed as the 'xportr.domain_name' option. Default:
"dataset". This is the column subset by the 'domain' argument in the
function.
\item Format Name - passed as the 'xportr.format_name' option.
Default: "format". Character values to update the 'format.sas' attribute of
the column. This is passed to \code{haven::write} to note the format.
\item Variable Name - passed as the 'xportr.variable_name' option. Default:
"variable". This is used to match columns in '.df' argument and the
metadata.
}
}

\examples{
adsl <- data.frame(
  USUBJID = c(1001, 1002, 1003),
  BRTHDT = c(1, 1, 2)
)

metadata <- data.frame(
  dataset = c("adsl", "adsl"),
  variable = c("USUBJID", "BRTHDT"),
  format = c(NA, "DATE9.")
)

adsl <- xportr_format(adsl, metadata)
}
