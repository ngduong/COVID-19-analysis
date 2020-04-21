library(RCurl)
library(tidyverse)

########## Data from CourseWorks
covid = read.csv(file.path(getwd(), "covid19-1.csv")) %>%
  janitor::clean_names() %>%
  dplyr::select(-id) %>%
  mutate(date = as.character(date),
         province_state = as.character(province_state),
         country_region = as.character(country_region))

########## Additional days
http = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"

dates = seq.Date(as.Date("03-24-2020", format = "%m-%d-%Y"), 
                 as.Date("04-20-2020", format = "%m-%d-%Y"), 
                 by = "day")
dates = format(dates, "%m-%d-%Y")

urls = paste0(http, dates, ".csv")

files = lapply(1:length(dates), function(i){
  d = read.csv(urls[[i]])
  d$date = dates[[i]]
  
  return(d)
})

df = dplyr::bind_rows(files) %>%
  janitor::clean_names() %>%
  dplyr::select(province_state, 
                country_region, 
                lat, 
                long,
                date,
                confirmed,
                deaths) %>%
  rename(confirmed_cases = confirmed,
         fatalities = deaths)



########## Combine datasets for final

covid_final = bind_rows(covid, df)

#write.csv(covid_final, file = file.path(getwd(), "covid_final.csv"))
