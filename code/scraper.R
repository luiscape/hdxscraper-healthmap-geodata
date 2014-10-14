## Simple HTML scraper.

# Dependencies
library(XML)
library(RCurl)
library(rjson)

# Helper function to handle folders on SW.
onSw <- function(there = FALSE) {
  if (there == TRUE) f = 'tool/'
  else f = ''
  return(f)
}

# Helper function to write tables on ScraperWiki.
source(paste(onSw(F), 'code/write_tables.R', sep=""))


###################################################
###################################################
######## Scraping Case data from HealthMap ########
###################################################
###################################################

# Function that fetches the data available
# in HealthMap's website and transforms
# the results into a data table.
scrapeHealthMap <- function() {
  cat('----------------------------------------\n')
  cat("Collecting the table from HealthMap.\n")
  cat('----------------------------------------\n')

  # HealthMap RSS feed
  url = 'http://healthmap.org/rss/ebola-all.rss'

  # getting the html
  doc <- xmlInternalTreeParse(url)

  # collecting the data into a data.frame
  output <- data.frame(
    title = xpathSApply(doc, '//item/title', xmlValue),
    publication_date = xpathSApply(doc, '//item/pubDate', xmlValue),
    source_url = xpathSApply(doc, '//item/source', xmlGetAttr, 'url'),
    author = xpathSApply(doc, '//item/author', xmlValue),
    country = xpathSApply(doc, '//item/category[@domain="location"]', xmlValue),
    latitude = xpathSApply(doc, '//item/geo:lat', xmlValue),
    longitude = xpathSApply(doc, '//item/geo:long', xmlValue),
    description = xpathSApply(doc, '//item/description', xmlValue)
    )


  # returning results
  cat('-------------------------------\n')
  cat('Done!\n')
  cat('-------------------------------\n')
  return(output)
}

# running
healthMapData <- scrapeHealthMap()

# writing table to scraperwiki
writeTables(healthMapData, 'health_map_data', 'scraperwiki')