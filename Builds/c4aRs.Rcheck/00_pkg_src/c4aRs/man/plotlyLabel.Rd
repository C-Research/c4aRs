\name{plotlyLabel}
\alias{plotlyLabel}
\title{Neatly format plotly tooltips}
\usage{
plotlyLabel(df, textNames, colNames, titleCol = NULL, title = NULL)
}
\arguments{
\item{df}{The dataframe you wish to label}
\item{textNames}{The names of the text fields you want to display. Should correspond to colNames.}
\item{colNames}{The names of the columns whose text you want to display. Should correspond to textNames.}
\item{titleCol}{Optional. The name of the column that holds a title for your tooltips.}
\item{title}{Optional. The name you wish to apply to all your tooltips.}
}
\description{
Takes in a dataframe, and returns the same dataframe with a label column formatted for plotly graphs.\cr
Example:  \cr \cr
routes <- c4aRs::plotlyLabel(routes, c("Type", "Name"), c("type", "name"), titleCol = "origin") \cr
plot_ly(data = routes, type = 'scatter', x = ~lat, text = ~label)
}




