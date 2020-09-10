\name{traffickingInstanceCount}
\alias{traffickingInstanceCount}
\title{Select all rows that have a value in certain columns.}
\usage{
traffickingInstanceCount(df, cols, by_col = NA)
}
\arguments{
\item{df}{A dataframe of seizure data.}
\item{cols}{The names of the columns that contain countries where each row was trafficked.}
\item{values}{Optional. The name of a column that you want to striate your TI count by. For instance, `Database1`.}
}
\description{
Takes in a dataframe, and returns a dataframe of Trafficking Instance count.
}




