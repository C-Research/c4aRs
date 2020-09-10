\name{selectByMultCols}
\alias{selectByMultCols}
\title{Select all rows that have a value in certain columns.}
\usage{
selectByMultCols(df, cols, values)
}
\arguments{
\item{df}{The dataframe you wish to select.}
\item{cols}{The names of the columns you wish to select with.}
\item{values}{A character vector that contains that values you wish to select with.}
}
\description{
Takes in a dataframe, and returns a dataframe with all rows where at least one column defined in `cols` contains any of the values defined in `values`. \cr \cr
Example:  \cr
countryCols <- c("origin", "transit", "destination") \cr
df <- fixMultCols(df, countryCols, "United States") \cr
}




