\name{leafletLabel}
\alias{leafletLabel}
\title{Neatly format leaflet tooltips}
\usage{
leafletLabel(df, textNames, colNames, titleCol = NULL, title = NULL, size = 2, titleSize = 4)
}
\arguments{
\item{df}{The dataframe you wish to label}
\item{textNames}{The names of the text fields you want to display. Should correspond to colNames.}
\item{colNames}{The names of the columns whose text you want to display. Should correspond to textNames.}
\item{titleCol}{Optional. The name of the column that holds a title for your tooltips.}
\item{title}{Optional. The name you wish to apply to all your tooltips.}
\item{size}{Size of regular text, Default 2.}
\item{titleSize}{Size of title text, Default 4.}
}
\description{
Takes in a dataframe, and returns the same dataframe with a label column formatted for leaflet maps. \cr
Example:  \cr \cr
leaf <- leafletLabel(routes, c("Type", "Name"), c("type", "name"), titleCol = "origin") \cr
leaf <- leaflet(leaf) \cr
leaf <- addMarkers(leaf, lng = ~long, lat = ~lat, popup = ~label) \cr
}




