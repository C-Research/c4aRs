\name{fixCountryCol}
\alias{fixCountryCol}
\title{Standardize a vector of countries}
\usage{
fixCountryCol <- function(inVec)
}
\arguments{
\item{df}{A vector of countries that are unstandardized}
}
\description{
This function makes the following corrections:

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
}




