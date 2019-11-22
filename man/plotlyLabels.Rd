\name{leafletLabel}
\alias{leafletLabel}
\title{Neatly format plotly tooltips}
\usage{
leafletLabel(routes, c("Type", "Name"), c("type", "name"), title = "Origin")
}
\arguments{
\item{df}{The dataframe you wish to label}
\item{textNames}{The names of the text fields you want to display. Should correspond to colNames.}
\item{colNames}{The names of the columns whose text you want to display. Should correspond to textNames.}
\item{titleCol}{Optional. The name of the column that holds a title for your tooltips.}
\item{title}{Optional. The name you wish to apply to all your tooltips.}
}
\description{
Takes in a dataframe, and returns the same dataframe with a label column formatted for plotly graphs.
Example:  c4aRs::plotlyLabel(routes, c("Type", "Name"), c("type", "name"), titleCol = "origin") %>%
plot_ly(type = 'scatter', x = ~lat, text = ~label)
}




