\name{fixMultCols}
\alias{fixMultCols}
\title{Easily replace multiple columns at the same time.}
\usage{
fixMultCols(df, cols, old, new)
}
\arguments{
\item{df}{The dataframe you wish to correct.}
\item{cols}{The names of the columns you wish to correct.}
\item{old}{The text you want to replace.}
\item{new}{The text you want to appear in place of the old text.}
}
\description{
Takes in a dataframe, and returns the same dataframe with corrected columns. \cr \cr
Example:  \cr
countryCols <- c("origin", "transit", "destination") \cr
df <- fixMultCols(df, countryCols, "Untied States", "United States") \cr
}




