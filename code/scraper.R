# Scraper for CDC's Historical Ebola outbreaks data.

# dependencies
library(XML)
library(RCurl)
source('tool/code/write_tables.R')


###################################################
###################################################
######### Scraping a List of Reports List #########
###################################################
###################################################

# function that gets the list of documents from WHO
# website and assembles a nice data.frame
scrapeCDCData <- function() {
  cat('----------------------------------------\n')
  cat("Collecting the table from CDC's website.\n")
  cat('----------------------------------------\n')

  # CDC url
  url = 'http://www.cdc.gov/vhf/ebola/outbreaks/history/chronology.html'

  # getting the html
  doc <- htmlParse(url)

  # collecting the data into a data.frame
  output <- data.frame(
    year =  xpathSApply(doc, '//*[@id="outbreaks"]/tbody/tr/th', xmlValue),
    coutry = xpathSApply(doc, '//*[@id="outbreaks"]/tbody/tr/td[1]', xmlValue),
    ebola_subtype = xpathSApply(doc, '//*[@id="outbreaks"]/tbody/tr/td[2]', xmlValue),
    reported_number_of_human_cases = xpathSApply(doc, '//*[@id="outbreaks"]/tbody/tr/td[3]', xmlValue),
    reported_number_of_deaths_among_cases = xpathSApply(doc, '//*[@id="outbreaks"]/tbody/tr/td[4]', xmlValue),
    situation = xpathSApply(doc, '//*[@id="outbreaks"]/tbody/tr/td[5]', xmlValue)
    )

  # extracting the data from within parentheses in percent
  output$reported_number_of_deaths_among_cases_percent <- regmatches(output$reported_number_of_deaths_among_cases, gregexpr("(?<=\\().*?(?=\\))", output$reported_number_of_deaths_among_cases, perl=T))
  
  # cleaning the parentheses out of the data and the % sign
  output$reported_number_of_deaths_among_cases <- gsub("(?=\\().*?(?<=\\))", "", output$reported_number_of_deaths_among_cases, perl = T)
  output$reported_number_of_deaths_among_cases_percent <- gsub('%', '', output$reported_number_of_deaths_among_cases_percent)
  output$reported_number_of_deaths_among_cases_percent <- as.numeric(output$reported_number_of_deaths_among_cases_percent)
  
  # replacing 0s
  output$reported_number_of_deaths_among_cases_percent <- ifelse(is.na(output$reported_number_of_deaths_among_cases_percent), 0, output$reported_number_of_deaths_among_cases_percent)
  
  # cleaning decimal cases
  specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
  output$reported_number_of_deaths_among_cases_percent <- specify_decimal(output$reported_number_of_deaths_among_cases_percent, 0)
  

  # create ifelse for laboratory confirmed cases
  output$laboratory_confirmed_cases <- ifelse(grepl('\\*', output$reported_number_of_deaths_among_cases), TRUE, FALSE)

  # create ifelse for asymptomatic cases
  output$asymptomatic <- ifelse(grepl('asymptomatic', output$reported_number_of_human_cases), TRUE, FALSE)
  
  # cleaning the parentheses out of the asymptomatic cases
  output$reported_number_of_human_cases <- gsub("(?=\\().*?(?<=\\))", "", output$reported_number_of_human_cases, perl = T)
  
  # adding references
  reference =  xpathSApply(doc, '//*[@id="contentArea"]/div[1]/div/ol/li', xmlValue)

  # returning results
  cat('-------------------------------\n')
  cat('Done!\n')
  cat('-------------------------------\n')
  return(output)
}

# running
cdcOutbreakList <- scrapeCDCData()

# writing output in CSV
write.csv(cdcOutbreakList, 'data/report_list.csv', row.names = F)

# writing table to scraperwiki
writeTables(cdcOutbreakList, 'CDC_Outbreak_Data', 'scraperwiki')