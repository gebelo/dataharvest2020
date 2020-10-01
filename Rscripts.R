#uncommment and run the following if you don't have it already
# install.packages("needs")

#loading various helper libraries that give us more programming commands
needs(tidyverse,lubridate,pdftools,jsonlite)

#json - how do deal with this? https://covid19-scoreboard-api.unacastapis.com/api/search/covidcountyaggregatestwolastdays_v3?size=4000

unacast="https://covid19-scoreboard-api.unacastapis.com/api/search/covidcountyaggregatestwolastdays_v3?size=4000"

unacastdata <- fromJSON(unacast)


as.tibble(unacastdata$hits$hits$`_source`)  # data is still a nested column instead of flat

as.tibble(unacastdata$hits$hits$`_source`)$data[[1]]  # peek inside the first data

#create table out of data by walking down the hieararchy - hits , hits, _source
social_distancing<-as.tibble(unacastdata$hits$hits$`_source`) %>%
  #payload data is contained in this element
  # "data":[{"date":"2020-09-24","totalGrade":"C-","travelDistanceMetric":-0.30799748533286286},
 # {"date":"2020-09-23","totalGrade":"D+","travelDistanceMetric":-0.21786248799470298}]
  #code to parse this into a flat table
  mutate(dates=lapply(data,  function(x){paste(x$date, collapse=";") }), #create column with today and yesterday dates
         grades=lapply(data,  function(x){paste(x$totalGrade, collapse=";") }), #create  column w today and yesterday grades
         traveldistance=lapply(data,  function(x){paste(x$travelDistanceMetric, collapse=";") }), #create a column with today, yesterday distance
         first_grade=str_split(grades, ";",simplify=T)[,1], #today grade
         second_grade=str_split(grades, ";",simplify=T)[,2], #yesterday grade
         first_travel=str_split(traveldistance, ";",simplify=T)[,1], #today dist
         second_travel=str_split(traveldistance, ";",simplify=T)[,2]  #yesterday dist
  )



#APIs -- world bank has all kinds of options https://data.worldbank.org/about/get-started
#but once you figure out what you want, it's much more efficient to get the data programatically

year18="http://api.worldbank.org/v2/country/all/indicator/DT.TDS.DEGG.CD?format=json&date=2018&per_page=150"

debt18 <- fromJSON(year18)

countries18 <- as.tibble(debt18[[2]])%>%
  select(indicator, country,countryiso3code, debt18=value)

year10="http://api.worldbank.org/v2/country/all/indicator/DT.TDS.DEGG.CD?format=json&date=2010&per_page=150"

debt10 <- fromJSON(year10)

countries10 <- as.tibble(debt10[[2]])%>%
  select(countryiso3code, debt10=value)

combo<-countries18 %>%
  inner_join(countries10, by=c("countryiso3code"))%>%
  mutate(ratio=debt18/debt10)%>%
  arrange(desc(ratio))


#packages -- eurostat data much easier to work with programatically, thanks to an R package
# http://ropengov.github.io/eurostat/articles/website/eurostat_tutorial.html documentation

#load libraries or install AND load if necessary
needs(eurostat,rvest,knitr)


# Get Eurostat data listing
toc <- get_eurostat_toc()

countries<- label_eurostat() # certain reference tables are built in

head(toc)

# info about passengers
transport<- search_eurostat("passenger transport")

# transportation type - percentage share of each mode of transport in total inland transport, 
#expressed in passenger-kilometres (pkm) based on transport
#by passenger cars, buses and coaches, and trains.


id <- search_eurostat("Modal split of passenger transport",
                      type = "table")$code[1]
print(id)

# original: http://ec.europa.eu/eurostat/tgm/table.do?tab=table&init=1&plugin=1&language=en&pcode=t2020_rk310
# usefull but requires you to download, import into spreadsheet and repeat every time you want to update data.

#using package you can make repeatable code and transform the data faster 
#grab table
eurotransport <- get_eurostat(id, time_format = "num",type = "label")

#what are our vehicle options?
table(eurotransport$vehicle)

# query for most car-dependent in 2018

cars <- eurotransport %>%
  filter(vehicle=="Passenger cars"&time=="2018") %>% # sql where clause
  arrange(desc(values))%>% # sql sort

  

  # SCRAPING
  
needs(rvest)

hot100page <- "https://www.billboard.com/charts/hot-100"
hot100 <- read_html(hot100page)
#what did we get?
hot100
#display what we got in more compact format
str(hot100)

#lets see what components are in the body of the document

body_nodes <- hot100 %>% 
  html_node("body") %>% 
  html_children()

#display what's inside the children
body_nodes %>% 
  html_children()


#if you inspect the source code, you see that the relevant data is wrapped in html <span> tags, 
#with the class names
#chart-element__rank__number,
#chart-element__information__artist and 
#chart-element__information__song.

#get the ranks
rank <- hot100 %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class, 'chart-element__rank__number')]") %>% 
  rvest::html_text()
# get the artists
artist <- hot100 %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class, 'chart-element__information__artist')]") %>% 
  rvest::html_text()
#get the songs
title <- hot100 %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class, 'chart-element__information__song')]") %>% 
  rvest::html_text()

#stich it back together
chart <- data.frame(rank, artist, title)

#note... this is where it's important to remember - with programming you don't need
# to memorize everything...  the way to learn is to start with an example like this
# and use it as a template -- when you go to scrape your site, this gives you the 
# basic concept, but maybe your data , you'll need to find elements by @id, not class
# or maybe you will need to loop through the rows of a table --  every scrape
# is different but having a starting point is the first step to learning.



#STRING PARSING

# PAGE 4

thefile="https://www.govinfo.gov/content/pkg/USCOURTS-ca4-18-01886/pdf/USCOURTS-ca4-18-01886-0.pdf"
thecase=pdf_text(thefile)

thecase

# page 5

#explore via split
str_split(thecase,"Before")


#page 7

#get our slice of interest
judgestring<- str_split(thecase, "Before ")[[1]][2]

judgestring

#page 8
str_split(judgestring, "\n")

#and now get the vector that has our data
judgestring <- str_split(judgestring,"\n")[[1]][1]

judgestring

#page 9

#gsub(search,replace,fromwhat)

judgestring<- gsub("Chief|Judge|Circuit|Judges|and|\\.| ","",judgestring)

judgestring

judgestring <- str_split(judgestring, ",")
judgestring

#page 11

gregexpr("Gregory", thecase, ignore.case=TRUE)[[1]][1:2]
gregexpr("Wynn",thecase,ignore.case=TRUE)[[1]][1:2]
gregexpr("Thacker",thecase,ignore.case=TRUE)[[1]][1:2]



