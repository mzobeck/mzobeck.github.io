library(gganimate)
library(tidyverse)
library(ggthemes)
library(readxl)
library(lubridate)
library(janitor)

csse_csvs <- tibble(dates = list.files("states")) %>% 
  mutate(dates = ymd(str_extract(dates, "[:digit:]+-[:digit:]+-[:digit:]+")))

states.comp <- read_csv(paste0("states/states_comp_",max(csse_csvs$dates),".csv")) 

state.abrv <- read_xlsx("initial_files/state_pop.xlsx") %>% 
  clean_names() %>% 
  select(state, abrv, region) %>% 
  mutate(Region = case_when(
    region == "w" ~ "West",
    region == "s" ~ "South",
    region == "ne" ~ "Northeast",
    region == "mw" ~ "Midwest"
  ))


states.race <- states.comp %>%
  rename(abrv = state) %>% 
  left_join(state.abrv) %>% 
  group_by(state) %>% 
  arrange(state, date) %>% 
  mutate(new.cases = ifelse(is.na(lag(positive)), -1, positive - lag(positive)),
         new.deaths = ifelse(is.na(lag(death)), -1, death - lag(death))) %>% 
  group_by(date) %>% 
  mutate(rank = row_number(desc(new.cases))) %>% 
  filter(rank <= 10) 



p <- states.race %>% 
  filter(date >= "2020-03-01") %>% 
  ggplot(aes(x = - rank, y = new.cases, group = state)) +
  geom_tile(aes(y = new.cases/2, height = new.cases, fill = Region), width = 0.9) + 
  geom_text(aes(label = state), hjust = "right", colour = "black", fontface = "bold", nudge_y = -50) +
  geom_text(aes(label = scales::comma(new.cases)), hjust = "left", nudge_y = 50, colour = "grey30") +
  scale_fill_manual(name = 'Region', values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +
  coord_flip(clip="off") +
  scale_x_discrete("") +
  scale_y_continuous("",labels=scales::comma) +
  theme_fivethirtyeight() +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = c(0.7, 0.1),
        axis.text.y=element_blank(),
        legend.text = element_text(size = "10"),
        legend.title = element_text(size = "12"),
        plot.title = element_text(size = "14"),
        plot.subtitle = element_text(size = "12")) +
  # gganimate code to transition by year:
  transition_time(date) +
  ease_aes('cubic-in-out') +
    labs(title='The New COVID-19 cases per day for the top 10 cities in the United States',
         subtitle='New cases on {round(frame_time,0)}',
         caption='Data Source: Johns Hopkins University COVID-19 Dashboard\nBuilt by @markzobeck')

animate(p, nframes = 750, fps = 30, end_pause = 50, width = 1200, height = 900)
anim_save("state_race_newcases.gif")

