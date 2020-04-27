library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(data.table)


#time index for daily updates
#JHU turns over days at 1800


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

#testing numbers 
tracking <- read_csv("http://covidtracking.com/api/states/daily.csv") %>% 
  clean_names() %>% 
  mutate(date = ymd(date)) %>% 
  rename(positive.tests = positive, negative.test = negative) %>% 
  rename(death.track = death)

csse_csvs <- tibble(dates = list.files("csse_files")) %>% 
  mutate(dates = ymd(str_extract(dates, "[:digit:]+-[:digit:]+-[:digit:]+")))

states.counties.hist.pull <- read_csv(paste0("csse_files/states_counties_hist_",max(csse_csvs$dates),".csv")) %>% 
  filter(!is.na(state)) 
#there are unassigned states from the past 


if (!(today%in% states.counties.hist.pull$date)) {
  url.yest <- "https://opendata.arcgis.com/datasets/628578697fb24d8ea4c32fa0c5ae1843_0.csv"
  
  csse.yesterday <- read_csv(url.yest) %>% 
    clean_names() %>% 
    rename(state = province_state) %>%
    filter(country_region == "US") %>% 
    left_join(state.abrv) %>% 
    pivot_longer(confirmed:deaths, names_to = "type", values_to = "value") %>% 
    select(state = abrv, county = admin2, date = last_update, type, value, lat, long) %>% 
    left_join(counties.pop) %>% 
    select(state, county, population, date, type, value, lat, long) %>% 
    mutate(type = ifelse(str_detect(type, "confir"), "cases",type),
           date = date(date)) %>% 
    mutate(date = if_else(today == date, date, date - days(1)))
  
  states.counties.hist <- states.counties.hist.pull %>% 
    bind_rows(csse.yesterday)
  
  fwrite(states.counties.hist, paste0("csse_files/states_counties_hist_",today,".csv"))
} else {
  states.counties.hist <- states.counties.hist.pull
}

states.counties <- states.counties.hist %>% 
  filter(!is.na(state)) %>% 
  group_by(date, state, county, type) %>% 
  filter(value == max(value)) %>% 
  distinct(date, state, county, type, .keep_all = TRUE) #I'm going to assume double entries are errors not additive



# state level data --------------------------------------------------------------
states.track.hist <- read_csv("initial_files/states_track_hist.csv")

states.counties.forstates <- states.counties %>% 
  filter(date >= as.Date("2020-03-22")) %>% 
  select(-population, -lat, -long) %>% 
  pivot_wider(id_cols = c("date", "state", "county"), names_from = "type", values_from = "value",) %>% 
  group_by(date, state) %>% 
  summarise(positive = sum(cases),
            death = sum(deaths)) %>% 
  mutate(positive = replace(positive, state == "NY" & date == "2020-04-23", 263460),
         death = replace(death, state == "NY" & date == "2020-04-23", 20973))

states.pos <- states.track.hist %>% 
  bind_rows(states.counties.forstates) %>% 
  filter(positive >= 10) %>% 
  group_by(state) %>% 
  mutate(since10 = rank(date)) %>% 
  left_join(state.pop) %>% 
  mutate(pos100k = positive/population * 100000,
         death100k = death/population * 100000)

states.deaths <- states.track.hist %>% 
  bind_rows(states.counties.forstates) %>% 
  filter(death >= 10) %>% 
  group_by(state) %>% 
  mutate(since10 = rank(date)) %>% 
  left_join(state.pop) %>% 
  mutate(pos100k = positive/population * 100000,
         death100k = death/population * 100000)


#remember this is for all states with > 10 cases only
states.top10.today <- states.pos %>% 
  filter(date == today ) %>% 
  arrange(desc(positive)) %>% 
  head(10)

states.top10.yesterday <- states.pos %>% 
  filter(date == yesterday ) %>% 
  arrange(desc(positive)) %>% 
  head(10)

if (nrow(states.top10.today) > 0) {
  states.top10 <- states.top10.today
} else {
  states.top10 <- states.top10.yesterday
}

states.top10.all <- states.pos %>% 
  filter(state %in% states.top10$state)

#top 20 for the state comparisons 
states.top20.today <- states.pos %>% 
  filter(date == today ) %>% 
  arrange(desc(positive)) %>% 
  head(20)

states.top20.yesterday <- states.pos %>% 
  filter(date == yesterday ) %>% 
  arrange(desc(positive)) %>% 
  head(20)


if (nrow(states.top20.today) > 0) {
  states.top20 <- states.top20.today
} else {
  states.top20 <- states.top20.yesterday
}

states.top20.all <- states.pos %>% 
  filter(state %in% states.top20$state)

#deaths 
#remember this is for all states with > 10 deaths only
states.top10d.today <- states.deaths %>% 
  filter(date == today ) %>% 
  arrange(desc(death)) %>% 
  head(10)

states.top10d.yesterday <- states.deaths %>% 
  filter(date == yesterday ) %>% 
  arrange(desc(death)) %>% 
  head(10)

if (nrow(states.top10d.today) > 0) {
  states.top10d <- states.top10d.today
} else {
  states.top10d <- states.top10d.yesterday
}

states.top10d.all <- states.deaths %>% 
  filter(state %in% states.top10d$state)

#top 20 for the state comparisons 
states.top20d.today <- states.deaths %>% 
  filter(date == today ) %>% 
  arrange(desc(death)) %>% 
  head(20)

states.top20d.yesterday <- states.pos %>% 
  filter(date == yesterday ) %>% 
  arrange(desc(death)) %>% 
  head(20)


if (nrow(states.top20d.today) > 0) {
  states.top20d <- states.top20d.today
} else {
  states.top20d <- states.top20d.yesterday
}

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

