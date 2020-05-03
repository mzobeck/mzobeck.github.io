states.counties.forstates <- states.counties %>% 
  filter(date >= as.Date("2020-03-22")) %>% 
  select(-population, -lat, -long) %>% 
  pivot_wider(id_cols = c("date", "state", "county"), names_from = "type", values_from = "value") %>% 
  group_by(date, state) %>% 
  summarise(positive = sum(cases),
            death = sum(deaths))


ahhhh <- states.counties[states.counties %>% 
                           group_by(date, state, county, type) %>% 
                           duplicated(), ] %>% 
  ungroup %>% 
  select(date, state, county)

states.counties %>% distinct(date) %>% view
blegh <- states.counties %>% 
  inner_join(ahhhh)

eh <- states.counties %>% 
  head(4000) %>% 
  filter(date >= as.Date("2020-03-22")) %>% 
  select(-population, -lat, -long) %>% 
  ungroup() %>% 
  mutate(date = if_else(date == "2020-04-24", date, date) )
class(states.counties$date)

str(eh)
ymd("2020-03-22")
18343/365
2020-50


try.again <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-23-2020.csv")
get.rid <- states.counties.hist %>% 
  filter(date != "2020-04-23")

csse.yesterday <- try.again %>% 
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

states.counties.hist <- get.rid %>% 
  bind_rows(csse.yesterday) %>% 
  mutate(date = if_else(date == "2020-04-24", ymd("2020-04-23"), date)) %>%
  ungroup %>% 
  distinct(state, county, date, type, .keep_all = TRUE)

yesterday <- ymd("2020-04-24")

states.counties <- states.counties.hist %>% 
  filter(!is.na(state)) %>% 
  group_by(date, state, county, type) %>% 
  filter(value == max(value)) #I'm going to assume double entries are errors not additive


  ahhhh <- states.counties[states.counties %>% 
                                       ungroup() %>% 
                             select(date, state, county, type) %>% 
                             duplicated(), ]  
  ungroup %>% 
  select(date, state, county)
  
  states.counties.forstates %>% 
    group_by(date, state, county) %>% 
    duplicated(date, state, county)
  

gravy <- states.counties %>% 
  ungroup %>% 
  group_by(date, state, county, type) %>% 
  summarise(count = n()) %>% 
  filter(count > 1) %>% 
  select(date, state, county) %>% 
    ungroup
  
states.counties %>% distinct(date) %>% view
str(gravy)
str(states.counties)

blegh <- states.counties %>%
  ungroup %>% 
  inner_join(gravy) 
  distinct(state, county, date, type, .keep_all = TRUE)
class(states.counties)
# state level data --------------------------------------------------------------
states.track.hist <- read_csv("initial_files/states_track_hist.csv")
rm(states.counties.forstates)

states.counties.forstates <- states.counties %>% 
  filter(date >= as.Date("2020-03-22")) %>% 
  select(-population, -lat, -long) %>% 
  pivot_wider(id_cols = c("date", "state", "county"), names_from = "type", values_from = "value") %>% 
  group_by(date, state) %>% 
  summarise(positive = sum(cases),
            death = sum(deaths)) %>% 
  mutate(positive = replace(positive, state == "NY" & date == "2020-04-23", 263460),
         death = replace(death, state == "NY" & date == "2020-04-23", 20973))


states.counties.forstates %>% filter(date == "2020-04-23") %>% view

states.counties.hist <- states.counties.hist %>% 
  filter(date != "2020-04-25")



# new day, new problem ----------------------------------------------------

states.counties.hist.pull
states.counties.forstates
new <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/04-23-2020.csv") %>% 
  clean_names

new.states <- new %>% 
  select(date = last_update, state = province_state, positive = confirmed, death = deaths) %>% 
  left_join(state.abrv) %>% 
  select(date, state = abrv, positive, death) %>% 
  mutate(date = date(date) - ddays(1)) %>% 
  filter(!is.na(state)) 

factorial(10)/(factorial(7)*factorial(3))
-0.6*log2(0.6) + -0.4*log2(0.4)
-log2(1/6)
0.6/5
-0.4*log2(0.4) + -5*(0.12*log2(0.12)) 

glue(states.top10$state_long[1])

cpd.hmc.stand %>% 
  summarize(tot = sum(cpd == 1)/nrow(.))


tracking <- read_csv("https://covidtracking.com/api/v1/states/daily.csv") %>% 
  clean_names() %>% 
  select(date, state, positive.tests = positive, negative.test = negative) %>% 
  mutate(date = ymd(date)) 

?url.ex
library(Rcurl)
library(RCurl)
url.ex

miss429 <-  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/04-29-2020.csv")
miss430 <-  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/04-30-2020.csv")
miss501 <-  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/05-01-2020.csv")

missing <- bind_rows(miss429, miss430, miss501)

missing.states <- missing %>% 
  clean_names() %>% 
  select(date = last_update, state = province_state, positive = confirmed, death = deaths) %>% 
  left_join(state.abrv) %>% 
  select(date, state = abrv, positive, death) %>% 
  mutate(date = date(date)) %>% 
  mutate(date = if_else(today == date, date, date - days(1))) %>% 
  filter(!is.na(state)) 

states.comp2 <- states.comp %>% 
  bind_rows(missing.states)

states.counties %>% ungroup %>%  select(date) %>% distinct %>% arrange(desc(date))

miss426 <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-24-2020.csv")
miss427 <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-27-2020.csv")
miss428 <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-28-2020.csv")
miss429 <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-29-2020.csv")
miss430 <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-30-2020.csv")
miss501 <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/05-01-2020.csv")
miss502 <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/05-02-2020.csv")


missing <- bind_rows(miss429, miss430, miss501, miss428, miss502, miss426, miss427)

csse.today<- missing %>% 
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
  mutate(date = if_else(date == today, date, date - days(1)))

csse.today <- csse.today %>% 
  mutate(date = replace(date, date == "2020-05-03", ymd("2020-05-02")))


states.counties.hist.pull2 <- states.counties.hist.pull %>% 
  filter(date <= ymd("2020-04-25"))

states.counties.hist2 <- states.counties.hist.pull2 %>% 
  bind_rows(csse.today)
