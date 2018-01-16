% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qgsProject.R
\name{openProject}
\alias{openProject}
\title{Open a given qgs Project}
\usage{
openProject(file, replace = FALSE)
}
\arguments{
\item{file}{Name of the project file. It can include the complete
if the file is not stored in the current directory.}

\item{replace}{Replace global variables if they already exist?
The default value is FALSE.}
}
\description{
Open a given QGIS project stored in a qgs file.
It also creates global variables storing the layers of the project.
}