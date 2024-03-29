% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_template_workbook.R
\name{create_template_workbook}
\alias{create_template_workbook}
\title{Create template workbook}
\usage{
create_template_workbook(
  site_ids,
  site_descriptions,
  feature_ids,
  feature_descriptions,
  action_ids,
  action_descriptions,
  parameters,
  site_longitudes = rep("", length(site_ids)),
  site_latitudes = rep("", length(site_ids))
)
}
\arguments{
\item{site_ids}{\code{character} identifiers for sites.
No missing (\code{NA}) values are permitted. This object must contain at least
one value.}

\item{site_descriptions}{\code{character} descriptions of sites.
No missing (\code{NA}) values are permitted. This object must contain at least
one value.}

\item{feature_ids}{\code{character} identifiers for biodiversity features.
No missing (\code{NA}) values are permitted. This object must contain at least
one value.}

\item{feature_descriptions}{\code{character} descriptions of biodiversity
features.
No missing (\code{NA}) values are permitted. This object must contain at least
one value.}

\item{action_ids}{\code{character} identifiers for management actions.
No missing (\code{NA}) values are permitted. This object must contain at least
one value.}

\item{action_descriptions}{\code{character} descriptions of management actions.
No missing (\code{NA}) values are permitted. This object must contain at least
one value.}

\item{parameters}{\code{list} object containing parameters to customize
appearance of worksheet.}

\item{site_longitudes}{\code{numeric} Longitudes for sites.
Defaults to an empty \code{character} vector equal to the number of sites.}

\item{site_latitudes}{\code{numeric} Latitudes for sites.
Defaults to an empty \code{character} vector equal to the number of sites.}
}
\value{
\code{Workbook} object.
}
\description{
This function creates a data template Excel Workbook.
}
