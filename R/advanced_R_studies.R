#' @docType data
#'
#' @usage data(world_map)
#'
#' @return
#'
#' @examples See the vignette for more examples.
#' data(world_map)
#'
#' @importFrom rlang .data


`%>%` <-  magrittr::`%>%`
`%+replace%` <- ggplot2::`%+replace%`


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


leafletLabel <- function(df, textNames, colNames, titleCol = NULL, title = NULL, size = 2, titleSize = 4, digits = 2){

  label <- NULL # make sure does not look like an unbound reference

  if (length(textNames) != length(colNames)){
    print("The number of names doesn't match the number of columns!")
    return("The number of names doesn't match the number of columns!")
  }

  roundFunc <- function(lab, digs = digits){
    if (is.numeric(lab)){
      return(round(lab, digs))
    }else{
      return(lab)
    }
  }

  num <- length(textNames)

  if (!is.null(title)){
    df <-
      df %>%
      dplyr::mutate(label = paste0("<b><font size=", titleSize, ">", title, "</font></b></br>"))
  }else if (!is.null(titleCol)){
    df <-
      df %>%
      dplyr::mutate(label = paste0("<b><font size=", titleSize, ">", eval(as.name(titleCol)), "</font></b></br>"))
  }else{
    df <-
      df %>%
      dplyr::mutate(label = "")
  }
  if (num != 1){
    for (i in 1:(num - 1)){
      df <-
        df %>%
        dplyr::mutate(label = paste0(label, "<b><font size=", size, ">", roundFunc(textNames[i]), "</font></b>: ", eval(as.name(colNames[i])), "</br>"))
    }
  }

  df <-
    df %>%
    dplyr::mutate(label = paste0(label, "<b><font size=", size, ">", roundFunc(textNames[num]), "</font></b>: ", eval(as.name(colNames[num]))))

  return(df)
}
plotlyLabel <- function(df, textNames, colNames, titleCol = NULL, title = NULL){

  label <- NULL # make sure does not look like an unbound reference

  if (length(textNames) != length(colNames)){
    print("The number of names doesn't match the number of columns!")
    return("The number of names doesn't match the number of columns!")
  }

  num <- length(textNames)

  if (!is.null(title)){
    df <-
      df %>%
      dplyr::mutate(label = paste0("<b>", title, "</b></br>"))
  }else if (!is.null(titleCol)){
    df <-
      df %>%
      dplyr::mutate(label = paste0("<b>", eval(as.name(titleCol)), "</b></br>"))
  }else{
    df <-
      df %>%
      dplyr::mutate(label = "")
  }
  if (num != 1){
    for (i in 1:(num - 1)){
      df <-
        df %>%
        dplyr::mutate(label = paste0(label, "<b>", textNames[i], "</b>: ", eval(as.name(colNames[i])), "</br>"))
    }
  }

  df <-
    df %>%
    dplyr::mutate(label = paste0(label, "<b>", textNames[num], "</b>: ", eval(as.name(colNames[num]))))

  return(df)
}



fixMultCols <- function(df, cols, old, new){
  df %>%
    dplyr::mutate_at(cols, ~str_replace(., old, new)) %>%
    dplyr::mutate_at(cols, trimws) %>%
    return()
}
selectByMultCols <- function(df, cols, values){
  `.` <- NULL # make sure does not look like an unbound reference

  df %>%
    dplyr::filter_at(cols, dplyr::any_vars(. %in% values)) %>%
    return()
}



traffickingInstanceCount <- function(df, cols, by_col = NA){

  count <- NULL # make sure does not look like an unbound reference
  n <- NULL # make sure does not look like an unbound reference
  total <- NULL # make sure does not look like an unbound reference
  val <- NULL # make sure does not look like an unbound reference
  `.` <- NULL # make sure does not look like an unbound reference

  df <-
    df %>%
    dplyr::mutate_if(is.factor, as.character)
  if (is.na(by_col)){
      df %>%
      dplyr::select(cols) %>%
      split(seq(nrow(df))) %>%
      purrr::map(unlist) %>%
      purrr::map(~str_split(., "/")) %>%
      purrr::map(unlist) %>%
      purrr::map(trimws) %>%
      purrr::map(unique) %>%
      purrr::map(~.[!is.na(.)]) %>%
      unlist() %>%
      table() %>%
      as.data.frame(stringsAdFactors = F) %>%
      dplyr::rename(count = "Freq", value = ".") %>%
      dplyr::arrange(-count) %>%
      return()
  }else{
    helpFunc1 <- function(row, cols, by){
      list(
        values = row %>%
          dplyr::select(cols) %>%
          purrr::map(unique) %>%
          purrr::map(~.[!is.na(.)]) %>%
          unlist() %>%
          unique() %>%
          as.matrix(ncol = 1) %>%
          as.data.frame(stringsAsFactors = F) %>%
          dplyr::rename(val = "V1") %>%
          tidyr::separate_rows(val, sep = "/") %>%
          dplyr::mutate(val = trimws(val)),
        ids = row %>%
          dplyr::select(by)
      ) %>%
        return()
    }
    helpFunc2 <- function(row){
      if (nrow(row$values) > 0){
        cbind(row$values, row$ids, row.names = NULL) %>%
          return()
      }else{
        return(NULL)
      }

    }

      df %>%
      dplyr::select(c(cols, by_col)) %>%
      split(seq(nrow(df))) %>%
      purrr::map(helpFunc1, cols = cols, by = by_col) %>%
      purrr::map(helpFunc2) %>%
      dplyr::bind_rows() %>%
      dplyr::group_by_all() %>%
      dplyr::tally() %>%
      dplyr::arrange(-n) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(id_cols = val, names_from = by_col, values_from = n) %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
      dplyr::mutate(total = rowSums(.[-1])) %>%
      dplyr::arrange(-total) %>%
      return()
  }
}






c4_col_gradients <- list(
  "greys" = c("#414042", "#636362", '#9B9B9B', '#CFCFCF'),
  "reds" = c("#f75151", '#EF8479', '#F5B1A5', '#F9DAD2'),
  "blues" = c("#043D5D", '#485A77', '#81889F', '#BEBFCD'),
  "olives" = c("#686B30", '#8E894E', '#B2AC83', '#D7D3BF'),
  "teals" = c("#004342", '#3F6363', '#7B8E8E', '#BBC2C2'),
  "purples" = c("#122945", '#484C64', '#7C7C8F', '#B9B7C2'),
  "yellows" = c("#D4AD6D", '#E2C48F', '#EBD8B5', '#F5EADA'),
  "greens" = c("#05A07E", '#6BB298', '#A3CBB9', '#D2E5DD'),
  "light_blues" = c("#A0B9D0", '#C3CFE2', '#D6DEEA', '#EBEFF5'),
  "tans" = c("#F8E5C8", '#FBEFDB', '#FCF4E7', '#FEF9F3'),
  "green_greys" = c("#B3B299", '#BCB69B', '#DDDACD', '#EDECE6')
)

c4_cols <- c(
  "greys" = "#414042",
  "reds" = "#f75151",
  "blues" = "#043D5D",
  "olives" = "#686B30",
  "teals" = "#004342",
  "purples" = "#122945",
  "yellows" = "#D4AD6D",
  "greens" = "#05A07E",
  "light_blues" = "#A0B9D0",
  "tans" = "#F8E5C8",
  "green_greys" = "#B3B299"
)

c4_col_leafy <- c('#f75151', "#414042", '#B3B299', "#05A07E", '#004342')
c4_col_spooky <- c('#f75151', '#414042','#A0B9D0','#122945', '#043D5D')
c4_col_dusty <- c('#f75151', '#414042', '#F8E5C8', '#D4AD6D', '#686B30')


scale_fill_cells <- function(...){
  ggplot2:::manual_scale(
    'fill',
    values = setNames(c("#93261e", "#440154", "#2a4f7b", "#729837", "#2f4f4f", "#fc7247", "#414042"),
                      c("CFIT", "CounterPro", "Data", "ECFC", "NattyR","OCGC", "Management")),
    ...
  )
}
scale_color_cells <- function(...){
  ggplot2:::manual_scale(
    'color',
    values = setNames(c("#93261e", "#440154", "#2a4f7b", "#729837", "#2f4f4f", "#fc7247", "#414042"),
                      c("CFIT", "CounterPro", "Data", "ECFC", "NattyR","OCGC", "Management")),
    ...
  )
}






theme_c4ads <- function(font = "Century Gothic", ...){   #assign font family up front

  ggplot2::theme_minimal() %+replace%    #replace elements we want to change

    ggplot2::theme(


      #since theme_minimal() already strips axis lines,
      #we don't need to do that again

      #text elements
      text = element_text(family = font),

      plot.title = ggplot2::element_text(             #title
        family = font,            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly

      plot.subtitle = ggplot2::element_text(          #subtitle
        family = font,            #font family
        size = 14),               #font size

      plot.caption = ggplot2::element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align

      axis.title = ggplot2::element_text(             #axis titles
        family = font,            #font family
        size = 10),               #font size

      axis.text = ggplot2::element_text(              #axis text
        family = font,            #axis famuly
        size = 9),                #font size

      axis.text.x = ggplot2::element_text(            #margin for axis text
        margin = ggplot2::margin(5, b = 10)),

      ...
    )
}







.onAttach <- function(pkgname, libname) {
  ggplot2::theme_set(theme_c4ads())

  assign("scale_colour_discrete", function(..., values = rev(unname(c4_cols))) ggplot2::scale_colour_manual(..., values = values), globalenv())
  assign("scale_fill_discrete", function(..., values = rev(unname(c4_cols))) ggplot2::scale_fill_manual(..., values = values), globalenv())
}
