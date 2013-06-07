\name{getMultiAssayData}
\alias{getMultiAssayData}
\title{getMultiAssayData}
\description{Get data from multiple assays from multiple studies and combine them in one data.frame}
\usage{getMultiAssayData(studyCodes, assayNames, subjectMetadata)}
\arguments{
  \item{studyCodes}{Give a list with study codes, one for each assa.}
  \item{assayNames}{Give a list of assays, same order as studyCodes}
  \item{subjectMetadata}{List of subject metadata}
}

\value{Returns a data.frame, content is based on the study, assay and metadata selection.}

\author{Robert Ernst}





