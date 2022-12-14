% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jedn.R
\name{jedn}
\alias{jedn}
\title{Jednoróg: Computation method of SES proposed by Jednoróg,  2012 (CFPS only)}
\usage{
jedn(occup_m, edu_m, data = "CFPS")
}
\arguments{
\item{occup_m}{mother's occupation}

\item{edu_m}{mother's education}

\item{data}{A character indicate 'CFPS' dataset or 'PSID'.And in this case, only CFPS is applicable,because occupation data is not available from PSID}
}
\value{
A single column of SES
}
\description{
Jednoróg: Computation method of SES proposed by Jednoróg,  2012 (CFPS only)
}
