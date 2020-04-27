library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(data.table)
# Switching over to the state specfici data source because New York broke on 4-23

new.23 <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/04-23-2020.csv") %>% 
  clean_names
new.24 <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/04-24-2020.csv") %>% 
  clean_names
new.25 <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/04-25-2020.csv") %>% 
  clean_names

new.states <- bind_rows(new.23, new.24, new.25) %>% 
  select(date = last_update, state = province_state, positive = confirmed, death = deaths) %>% 
  left_join(state.abrv) %>% 
  select(date, state = abrv, positive, death) %>% 
  mutate(date = date(date) - ddays(1)) %>% 
  filter(!is.na(state)) 

old.states.counties.forstates <- states.counties.forstates %>% # from index_initialize-old.R 
  filter(date < "2020-04-23") %>% 
  mutate(positive = replace(positive, state == "NY" & date == "2020-04-22", 263292),
         death = replace(death, state == "NY" & date == "2020-04-22", 19413))

states.counties.comp <- old.states.counties.forstates %>% 
  bind_rows(new.states, states.track.hist) #also from index_initialize-old.R
    
fwrite(states.counties.comp, "states/states_comp_2020-04-25.csv")   
    
    
    
    
