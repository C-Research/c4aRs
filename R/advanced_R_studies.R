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





fixCountryCol <- function(inVec){

  inVec[which(inVec == 'United Kingdom')] <- 'UK'
  inVec[which(inVec == "United States")] <- "USA"
  inVec[which(inVec == "The United States")] <- "USA"
  inVec[which(inVec == "Untied States")] <- "USA"
  inVec[which(inVec == "United Staets")] <- "USA"
  inVec[which(inVec == "The United States of America")] <- "USA"
  inVec[which(inVec == "United Arab Emirates")] <- "UAE"
  inVec[which(inVec == "Iran, Islamic Republic of")] <- "Iran"
  inVec[which(inVec == "Moldova, Republic of")] <- "Moldova"
  inVec[which(inVec == "The Former Yugoslav Republic of Macedonia")] <- "FYR Macedonia"
  inVec[which(inVec == "Union of Myanmar (republic of the)")] <- "Myanmar"
  inVec[which(inVec == "Afghanistan, Islamic Republic of")] <- "Afghanistan"
  inVec[which(inVec == "Syrian Arab Republic")] <- "Syria"
  inVec[which(inVec == "Korea, Republic of")] <- "Korea (Rep. of)"
  inVec[which(inVec == "Cote D'ivoire")] <- "Cote d'Ivoire"
  inVec[which(inVec == "Macao, China")] <- "Macau, China"
  inVec[which(inVec == "The Former Yugoslav Republic of Macedonia")] <- "FYR Macedonia"
  inVec[which(inVec == "Afghanistan, Islamic Republic of")] <- "Afghanistan"
  inVec[which(inVec == "United Arab Emirates")] <- "UAE"
  inVec[which(inVec == "Syrian Arab Republic")] <- "Syria"
  inVec[which(inVec == "Korea, Republic of")] <- "Korea (Rep. of)"
  inVec[which(inVec == "Democratic Republic of the Congo")] <- "Congo (Dem. Rep. of)"
  inVec[which(inVec == "Congo, Republic of the")] <- "Congo (Rep. of)"
  inVec[which(inVec == "Ivory Coast")] <- "Cote d'Ivoire"
  inVec[which(inVec == "Cote D'ivoire")] <- "Cote d'Ivoire"
  inVec[which(inVec == "Macao, China")] <- "Macau, China"
  inVec[which(inVec == "Iran, Islamic Republic of")] <- "Iran"
  inVec[which(inVec == "Us Virgin Islands")] <- "US Virgin Islands"
  inVec[which(inVec == "Moldova, Republic of")] <- "Moldova"
  inVec[which(inVec == "The Former Yugoslav Republic of Macedonia")] <- "FYR Macedonia"
  inVec[which(inVec == "Afghanistan, Islamic Republic of")] <- "Afghanistan"
  inVec[which(inVec == "United Arab Emirates")] <- "UAE"
  inVec[which(inVec == "Syrian Arab Republic")] <- "Syria"
  inVec[which(inVec == "Korea, Republic of")] <- "Korea (Rep. of)"
  inVec[which(inVec == "Democratic Republic of the Congo")] <- "Congo (Dem. Rep. of)"
  inVec[which(inVec == "Congo, Republic of the")] <- "Congo (Rep. of)"
  inVec[which(inVec == "Macao, China")] <- "Macau, China"

  return(inVec)
}


leafletLabel <- function(df, textNames, colNames, titleCol = NULL, title = NULL, size = 2, titleSize = 4){
  if (length(textNames) != length(colNames)){
    print("The number of names doesn't match the number of columns!")
    return("The number of names doesn't match the number of columns!")
  }

  num <- length(textNames)

  if (!is.null(title)){
    df <-
      df %>%
      mutate(label = paste0("<b><font size=", titleSize, ">", title, "</font></b></br>"))
  }else if (!is.null(titleCol)){
    df <-
      df %>%
      mutate(label = paste0("<b><font size=", titleSize, ">", eval(as.name(titleCol)), "</font></b></br>"))
  }else{
    df <-
      df %>%
      mutate(label = "")
  }
  if (num != 1){
    for (i in 1:(num - 1)){
      df <-
        df %>%
        mutate(label = paste0(label, "<b><font size=", size, ">", textNames[i], "</font></b>: ", eval(as.name(colNames[i])), "</br>"))
    }
  }

  df <-
    df %>%
    mutate(label = paste0(label, "<b><font size=", size, ">", textNames[num], "</font></b>: ", eval(as.name(colNames[num]))))

  return(df)
}
plotlyLabel <- function(df, textNames, colNames, titleCol = NULL, title = NULL){
  if (length(textNames) != length(colNames)){
    print("The number of names doesn't match the number of columns!")
    return("The number of names doesn't match the number of columns!")
  }

  num <- length(textNames)

  if (!is.null(title)){
    df <-
      df %>%
      mutate(label = paste0("<b>", title, "</b></br>"))
  }else if (!is.null(titleCol)){
    df <-
      df %>%
      mutate(label = paste0("<b>", eval(as.name(titleCol)), "</b></br>"))
  }else{
    df <-
      df %>%
      mutate(label = "")
  }
  if (num != 1){
    for (i in 1:(num - 1)){
      df <-
        df %>%
        mutate(label = paste0(label, "<b>", textNames[i], "</b>: ", eval(as.name(colNames[i])), "</br>"))
    }
  }

  df <-
    df %>%
    mutate(label = paste0(label, "<b>", textNames[num], "</b>: ", eval(as.name(colNames[num]))))

  return(df)
}

