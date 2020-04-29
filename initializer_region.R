library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(data.table)




today <- ymd(format(Sys.time(), "%Y-%m-%d"))
yesterday <- ymd(format(Sys.time(), "%Y-%m-%d")) - days(1)

counties.pop <- read_csv("initial_files/counties_pop_fromscrape-200325.csv") %>% 
  select(-lat, -long)

state.abrv <- read_xlsx("initial_files/state_pop.xlsx") %>% 
  clean_names() %>% 
  select(state, abrv, region)

state.names <- state.abrv %>% 
  rename(state_long = state, state = abrv)

state.pop <- read_xlsx("initial_files/state_pop.xlsx") %>%  
  select(state = abrv, population, region)

target.states <- read_csv("target_states.csv") %>% 
  select(state = state_code)

#testing numbers 
tracking <- read_csv("http://covidtracking.com/api/states/daily.csv") %>% 
  clean_names() %>% 
  mutate(date = ymd(date)) %>% 
  rename(positive.tests = positive, negative.test = negative) %>% 
  rename(death.track = death)

csse_csvs <- tibble(dates = list.files("states")) %>% 
  mutate(dates = ymd(str_extract(dates, "[:digit:]+-[:digit:]+-[:digit:]+")))

states.comp.pull <- read_csv(paste0("states/states_comp_",max(csse_csvs$dates),".csv")) 


if (!(today %in% states.comp.pull$date)) {
  url.tod <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/", format(today, "%m-%d-%Y"),".csv")
  
  if(url.exists(url.tod)) { #check if today's csv is up
    
    new.states <- read_csv(url.tod) %>% 
      clean_names() %>% 
      select(date = last_update, state = province_state, positive = confirmed, death = deaths) %>% 
      left_join(state.abrv) %>% 
      select(date, state = abrv, positive, death) %>% 
      mutate(date = date(date)) %>% 
      mutate(date = if_else(today == date, date, date - days(1))) %>% 
      filter(!is.na(state)) 
    
    states.comp <- states.comp.pull %>% 
      bind_rows(new.states)
    
    fwrite(states.comp, paste0("states/states_comp_",today,".csv"))
  } else { #check if we have yesterday 
    
    if (!(yesterday %in% states.comp.pull$date)) {
      
      url.yest <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/", format(yesterday, "%m-%d-%Y"),".csv")
      
      if(url.exists(url.yest)) {
        
        new.states <- read_csv(url.yest) %>% 
          clean_names() %>% 
          select(date = last_update, state = province_state, positive = confirmed, death = deaths) %>% 
          left_join(state.abrv) %>% 
          select(date, state = abrv, positive, death) %>% 
          mutate(date = date(date)) %>% 
          mutate(date = if_else(yesterday == date, date, date - days(1))) %>% 
          filter(!is.na(state)) 
        
        states.comp <- states.comp.pull %>% 
          bind_rows(new.states)
        
        fwrite(states.comp, paste0("states/states_comp_",yesterday,".csv"))
      } else {
        states.comp <- states.comp.pull
    }  
    } else {
      states.comp <- states.comp.pull
    }
  }
} else {
  states.comp <- states.comp.pull
}
  
states.comp <- states.comp %>% 
  inner_join(target.states)

states.pos <- states.comp %>%  
  filter(positive >= 10) %>% 
  group_by(state) %>% 
  mutate(since10 = rank(date)) %>% 
  left_join(state.pop) %>% 
  mutate(pos100k = positive/population * 100000,
         death100k = death/population * 100000)

states.deaths <- states.comp %>% 
  filter(death >= 10) %>% 
  group_by(state) %>% 
  mutate(since10 = rank(date)) %>% 
  left_join(state.pop) %>% 
  mutate(pos100k = positive/population * 100000,
         death100k = death/population * 100000)


#remember this is for all states with > 10 cases only
states.top10<- states.pos %>% 
  filter(date == max(states.comp$date)) %>%  
  arrange(desc(positive)) %>% 
  head(10) %>% 
  left_join(state.names)

states.top10.all <- states.pos %>% 
  filter(state %in% states.top10$state)

#top 20 for the state comparisons 
states.top20 <- states.pos %>% 
  filter(date == max(states.comp$date)) %>% 
  arrange(desc(positive)) %>% 
  head(20)

states.top20.all <- states.pos %>% 
  filter(state %in% states.top20$state)

#deaths 
#remember this is for all states with > 10 deaths only
states.top10d <- states.deaths %>% 
  filter(date == max(states.comp$date) ) %>% 
  arrange(desc(death)) %>% 
  head(10)

states.top10d.all <- states.deaths %>% 
  filter(state %in% states.top10d$state)

#top 20 for the state comparisons 
states.top20d<- states.deaths %>% 
  filter(date == max(states.comp$date) ) %>% 
  arrange(desc(death)) %>% 
  head(20)

states.top20d.all <- states.deaths %>% 
  filter(state %in% states.top20d$state)


#state new cases
states.newcases <- states.pos %>% 
  group_by(state) %>% 
  arrange(state, date) %>% 
  mutate(new.cases = ifelse(is.na(lag(positive)), -1, positive - lag(positive)),
         new.deaths = ifelse(is.na(lag(death)), -1, death - lag(death))) 

states.new.top10 <- states.newcases %>% 
  filter(state %in% states.top10$state)

states.new.top20 <- states.newcases %>% 
  filter(state %in% states.top20$state)

#new deaths 
states.newdeaths <- states.deaths %>% 
  group_by(state) %>% 
  arrange(state, date) %>% 
  mutate(new.cases = ifelse(is.na(lag(positive)), -1, positive - lag(positive)),
         new.deaths = ifelse(is.na(lag(death)), -1, death - lag(death))) 

states.newdeaths.top10 <- states.newdeaths %>% 
  filter(state %in% states.top10$state)

states.newdeaths.top20 <- states.newdeaths %>% 
  filter(state %in% states.top20$state)

