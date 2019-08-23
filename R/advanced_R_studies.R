#' @docType data
#'
#' @usage data(world_map)
#'
#' @return
#'
#' @examples For now, this package does nothing but export the corrected World Map.
#' data(world_map)
#'

world_map <- read.csv("data/world_map.csv", stringsAsFactors = F, sep = ",", colClasses = "character")
world_map$long <- as.numeric(world_map$long)
world_map$lat <- as.numeric(world_map$lat)












