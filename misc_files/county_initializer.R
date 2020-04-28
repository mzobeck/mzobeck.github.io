library(tidyverse)
library(lubridate)
library(readxl)


#time index for daily updates
#JHU turns over days at 1800

if(as.numeric(format(Sys.time(), "%H")) < 18) {
  today <- ymd(format(Sys.time(), "%Y-%m-%d"))
} else {
  today <- ymd(format(Sys.time(), "%Y-%m-%d")) + days(1)
}


if(as.numeric(format(Sys.time(), "%H")) < 18) {
  yesterday <- ymd(format(Sys.time(), "%Y-%m-%d")) - days(1)
} else {
  yesterday <- ymd(format(Sys.time(), "%Y-%m-%d")) 
}

target.states <- read_csv("target_states.csv")

counties.pop <- read_csv("initial_files/counties_pop_fromscrape-200325.csv") %>% 
  select(-lat, -long)

state.abrv <- read_xlsx("initial_files/state_pop.xlsx") %>% 
  clean_names() %>% 
  select(state, abrv, region)

state.pop <- read_xlsx("initial_files/state_pop.xlsx") %>%  
  select(state = abrv, population, region)

csse_csvs <- tibble(dates = list.files("csse_files")) %>% 
  mutate(dates = ymd(str_extract(dates, "[:digit:]+-[:digit:]+-[:digit:]+")))

states.counties.hist.pull <- read_csv(paste0("csse_files/states_counties_hist_",max(csse_csvs$dates),".csv")) %>% 
  filter(!is.na(state)) #there are unassigned states from the past 


if (!(yesterday %in% states.counties.hist.pull$date)) {
  url.yest <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", format(yesterday, "%m-%d-%Y"),".csv")
  
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
           date = date(date))
  
  states.counties.hist <- states.counties.hist.pull %>% 
    bind_rows(csse.yesterday)
  
  fwrite(states.counties.hist, paste0("csse_files/states_counties_hist_",yesterday,".csv"))
} else {
  states.counties.hist <- states.counties.hist.pull
}

states.counties <- states.counties.hist %>% 
  filter(!is.na(state)) %>% 
  group_by(date, state, county, type) %>% 
  filter(value == max(value)) %>%  #I'm going to assume double entries are errors not additive
  filter(state %in% target.states$abrv)

# TX county data
tx.county <- states.counties %>% 
  filter(state == "TX" ) %>% 
  filter(!is.na(county))

#tx county cases first
tx.county.cases <- tx.county %>% 
  filter(type == "cases") %>% 
  filter(value > 0) %>% 
  group_by(county) %>% 
  mutate(since1 = rank(date))

txcounty.top10 <- tx.county.cases %>% 
  filter(date == yesterday ) %>% 
  arrange(desc(value)) %>% 
  head(10)

txcounty.top10.all <- tx.county.cases %>% 
  filter(county %in% txcounty.top10$county)


#tx county cases deaths

d1.index <- tx.county.cases %>% 
  select(county, date, since1)

tx.county.deaths <- tx.county %>% 
  filter(type == "deaths") %>% 
  left_join(d1.index)

txcounty.deaths.top10.all <- tx.county.deaths %>% 
  filter(county %in% txcounty.top10$county)

# houston data 
houston.metro <- c("Harris", "Fort Bend", "Montgomery", "Galveston", "Brazoria", "Liberty", "Chambers", "Waller")  

dfw.metro <- c("Collin", 
               "Dallas", 
               "Denton",
               "Ellis",
               "Hunt", 
               "Kaufman", 
               "Rockwall", 
               "Hood", 
               "Johnson", 
               "Parker", 
               "Somervell", 
               "Tarrant", 
               "Wise") 

austin.metro <- c("Bastrop", "Caldwell", "Hays", "Travis","Williamson")
san.antonio.metro <- c("Atascosa",
                       "Bandera",
                       "Bexar",
                       "Comal",
                       "Guadalupe",
                       "Kendall",
                       "Medina",
                       "Wilson")

waco.metro <-c("McLennan", "Falls")

houston.metro.cases <- tx.county.cases %>% 
  filter(county %in% houston.metro) 

dfw.metro.cases <- tx.county.cases %>% 
  filter(county %in% dfw.metro)

austin.cases <- tx.county.cases %>% 
  filter(county %in% austin.metro)

sa.cases <- tx.county.cases %>% 
  filter(county %in% san.antonio.metro)

waco.cases <- tx.county.cases %>% 
  filter(county %in% waco.metro)

lbk.cases <- tx.county.cases %>% 
  filter(county == "Lubbock")

ep.cases <- tx.county.cases %>% 
  filter(county == "El Paso")


#Deaths 
houston.metro.deaths<- tx.county.deaths %>% 
  filter(county %in% houston.metro) 

dfw.metro.deaths <- tx.county.deaths %>% 
  filter(county %in% dfw.metro)

austin.deaths <- tx.county.deaths %>% 
  filter(county %in% austin.metro)

sa.deaths <- tx.county.deaths %>% 
  filter(county %in% san.antonio.metro)

waco.deaths <- tx.county.deaths %>% 
  filter(county %in% waco.metro)

lbk.deaths <- tx.county.deaths %>% 
  filter(county == "Lubbock")

ep.deaths <- tx.county.deaths %>% 
  filter(county == "El Paso")



