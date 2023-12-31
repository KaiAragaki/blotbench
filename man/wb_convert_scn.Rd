% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{wb_convert_scn}
\alias{wb_convert_scn}
\title{Convert a .scn file to another format}
\usage{
wb_convert_scn(file, dest_name = NULL, overwrite = FALSE)
}
\arguments{
\item{file}{Character. Path to the file to be converted.}

\item{dest_name}{Character. File name and path to save to. If not supplied,
will be same name and location as input, but with a .tif extension.}

\item{overwrite}{Logical. Should the file be overwritten if it already
exists?}
}
\value{
Path to the saved file
}
\description{
The BioRad ChemiDoc produces .scn files that can be converted to .tif(f)
files using bfconvert.
}
\details{
This function requires \href{https://bio-formats.readthedocs.io/en/stable/users/comlinetools/index.html}{bftools} to be installed
}
\examples{
\dontrun{
wb_convert_scn("path/to/file.scn", dest_name = "~/new/path.tif")
}
}
