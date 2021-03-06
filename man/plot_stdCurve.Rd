% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_stdCurve.R
\name{plot_stdCurve}
\alias{plot_stdCurve}
\title{Plot standard curve}
\usage{
plot_stdCurve(assay.data.raw, standard.IDs, title)
}
\arguments{
\item{assay.data.raw}{An excel table exported from the Gen5 software.}

\item{standard.IDs}{A 2-column dataframe consisting of 'well_ID' (e.g., A12) and 'standard' (a standard concentration, generally in parts per million, mapped to each well_ID)}

\item{title}{Title of the plot. A string.}
}
\description{
This function serves as a wrapper to quickly visualize an analytical standard curve from a Gen5 software output
}
\examples{
plot_stdCurve

}
\keyword{curve}
\keyword{plot}
\keyword{standard}
