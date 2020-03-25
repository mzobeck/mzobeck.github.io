
#im going to use these data from prior to 3/22/20 bc 3/22 is when JH dashboard started doing county stuff
#scrape.comp <- read_csv("https://coronadatascraper.com/timeseries-tidy.csv", 
#                        col_types = "ccccdddcccd") %>% 
#  mutate(date = ymd(date)) %>% 
#  mutate(county = str_extract(county, ".*(?= County)")) 

#states.counties <- scrape.comp %>% 
#  filter(country == "USA") %>% 
#  filter(!is.na(county))


# fwrite(scrape.comp, "scrape_comp_last-200322.csv")


states.counties.save <- states.counties %>% 
  select(state, county, population, lat, long, date, type, value) %>% 
  filter(date < as.Date("2020-03-22")) %>% 
  filter(type == "deaths"|type == "cases") %>% 
  select(-lat, -long)

data.table::fwrite(states.counties.save, file = "states_counties_scrape-thru200322.csv")

counties.pop <- state.counties.save %>% 
  distinct(county, state, .keep_all = TRUE) %>% 
  select(state, county, population, lat, long)

data.table::fwrite(counties.pop, "counties_pop_fromscrape-200325.csv")


# jhsph tidying  ----------------------------------------------------------
state.abrv <- read_xlsx("state_pop.xlsx") %>% 
  clean_names() %>% 
  select(state, abrv, region)


csse.322 <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-22-2020.csv") 
csse.323 <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-23-2020.csv"))
csse.324 <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-24-2020.csv"))

csse.322 <- csse.322 %>%
  clean_names() %>% 
  mutate(fips = as.character(fips),
         last_update = mdy_hm(last_update))

csse.323 <- csse.323 %>%
  clean_names() 

csse.324 <- csse.324 %>% 
  clean_names() 


csse.combine <- bind_rows(csse.322, csse.323, csse.324) %>%
  rename(state = province_state) %>%
  filter(country_region == "US") %>% 
  left_join(state.abrv) %>% 
  pivot_longer(confirmed:deaths, names_to = "type", values_to = "value") %>% 
  select(state = abrv, county = admin2, date = last_update, type, value, lat, long) 


csse.latlong <- csse.combine %>% 
  select(state,county, lat, long) %>% 
  distinct(state, county, .keep_all = TRUE)

csse.combine2 <- csse.combine %>% 
  left_join(counties.pop) %>% 
  select(state, county, population, date, type, value) %>% 
  mutate(type = ifelse(str_detect(type, "confir"), "cases",type),
         date = date(date))


# combine the two big spreadsheets ----------------------------------------
states.counties.to324 <- csse.combine2 %>% 
  bind_rows(states.counties.save) %>% 
  left_join(csse.latlong)

states.tots <- states.counties.to324 %>% 
    group_by(state) %>% 
    filter(type == "cases") %>% 
    filter(date == "2020-03-24") %>% 
    summarise(tot_cases = sum(value))
  
naniar::vis_miss(states.counties.to324)

fwrite(states.counties.to324, "states_counties_to324.csv")


states.counties <- read_csv("states_counties_to324.csv")

fwrite(states.counties, "state_counties_hist.csv")


# check that i can add all of these for states counts ---------------------


states.tots <- states.counties %>% 
  group_by(state) %>% 
  filter(type == "cases") %>% 
  filter(date == "2020-03-24") %>% 
  summarise(tot_cases = sum(value))



states.counties %>% 
  filter(type == "cases") %>% 
  filter(date == "2020-03-24") %>% 
  summarise(tot_cases = sum(value))


jh.data.today <- read_csv("https://opendata.arcgis.com/datasets/628578697fb24d8ea4c32fa0c5ae1843_0.csv")

states.counties.today <-   jh.data.today %>% 
  clean_names() %>% 
  rename(state = province_state) %>%
  filter(country_region == "US") %>% 
  left_join(state.abrv) %>% 
  pivot_longer(confirmed:deaths, names_to = "type", values_to = "value") %>% 
  select(state = abrv, county = admin2, date = last_update, type, value) %>% 
  left_join(counties.pop) %>% 
  select(state, county, population, lat, long, date, type, value) %>% 
  mutate(type = ifelse(str_detect(type, "confir"), "cases",type),
         date = date(date))

## This is check that state totals reflet JHU dashboard totals 
states.counties.today %>% 
  filter(type == "cases") %>% 
  filter(state == "TX") %>% 
  summarise(tot_cases = sum(value))



# So what i need in my script ---------------------------------------------
counties.pop <- read_csv("counties_pop_fromscrape-200325.csv") %>% 
  select(-lat, -long)

state.abrv <- read_xlsx("state_pop.xlsx") %>% 
  clean_names() %>% 
  select(state, abrv, region)

states.counties.hist <- read_csv("states_counties_to324.csv")

jh.data.today <- read_csv("https://opendata.arcgis.com/datasets/628578697fb24d8ea4c32fa0c5ae1843_0.csv")

states.counties.today <- jh.data.today %>% 
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

states.counties <- states.counties.today %>% 
  bind_rows(states.counties.hist)

view(states.counties)
