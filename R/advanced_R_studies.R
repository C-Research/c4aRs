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



fixMultCols <- function(df, cols, old, new){
  df %>%
    mutate_at(cols, ~str_replace(., old, new)) %>%
    mutate_at(cols, trimws) %>%
    return()
}
selectByMultCols <- function(df, cols, values){
  df %>%
    filter_at(cols, any_vars(. %in% values)) %>%
    return()
}



traffickingInstanceCount <- function(df, cols, by_col = NA){
  df <-
    df %>%
    mutate_if(is.factor, as.character)
  if (is.na(by_col)){
      df %>%
      dplyr::select(cols) %>%
      split(seq(nrow(df))) %>%
      map(unlist) %>%
      map(~str_split(., "/")) %>%
      map(unlist) %>%
      map(trimws) %>%
      map(unique) %>%
      map(~.[!is.na(.)]) %>%
      unlist() %>%
      table() %>%
      as.data.frame(stringsAdFactors = F) %>%
      rename(count = Freq, value = ".") %>%
      arrange(-count) %>%
      return()
  }else{
    helpFunc1 <- function(row, cols, by){
      list(
        values = row %>%
          dplyr::select(cols) %>%
          map(unique) %>%
          map(~.[!is.na(.)]) %>%
          unlist() %>%
          unique() %>%
          as.matrix(ncol = 1) %>%
          as.data.frame(stringsAsFactors = F) %>%
          rename(val = "V1") %>%
          separate_rows(val, sep = "/") %>%
          mutate(val = trimws(val)),
        ids = row %>%
          dplyr::select(by)
      ) %>%
        return()
    }
    helpFunc2 <- function(row){
      cbind(row$values, row$ids, row.names = NULL)
    }

      df %>%
      dplyr::select(c(cols, by_col)) %>%
      split(seq(nrow(df))) %>%
      map(helpFunc1, cols = cols, by = by_col) %>%
      map(helpFunc2) %>%
      bind_rows %>%
      group_by_all() %>%
      tally() %>%
      arrange(-n) %>%
      ungroup() %>%
      pivot_wider(id_cols = val, names_from = by_col, values_from = n) %>%
      mutate_if(is.numeric, replace_na, replace = 0) %>%
      mutate(total = rowSums(.[-1])) %>%
      arrange(-total) %>%
      return()
  }
}






