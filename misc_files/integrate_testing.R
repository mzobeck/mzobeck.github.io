top.prev <- function(x) {
  x %>% 
    ggplot(aes(since10, positive, group = state, color = state)) +
    geom_line(alpha = 0.7, size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
    theme_minimal_hgrid() +
    scale_color_calc() +
    labs(y= "Reported Cases", x = "Days since reporting 10th case", color = "State") +
    ggtitle("Cumulative reported cases by days\nsince the 10th reported case.")
}

top.prev(states.top10.all)




#This goes in initialize and initialize index 
tracking <- read_csv("http://covidtracking.com/api/states/daily.csv") %>% 
  clean_names() %>% 
  mutate(date = ymd(date)) %>% 
  rename(positive.tests = positive, negative.test = negative) %>% 
  rename(death.track = death)
  
tracking %>%  filter(state == "TX") %>% view

#this goes in the state_functions
the.state <- "TX"
  
state.testing <- states.pos %>% 
  filter(state == the.state) %>% 
  left_join(tracking, by = c("date", "state")) %>% 
  select(date, state, population, since10, negative_increase, positive_increase) %>% 
  group_by(date, state, since10, population) %>% 
  pivot_longer(negative_increase:positive_increase) %>% 
  mutate(val100k = (value/population)*100000)

#inster these two graphs into the state_functions script
states.pos %>% 
  filter(state == the.state) %>% 
  ggplot(aes(since10, positive, group = state, color = state)) +
  geom_col(data = state.testing, aes(x = since10, value, fill = name),
           position="stack", alpha = 0.4, color = "white") +
  scale_fill_manual(values = c("deepskyblue", "red"),
                    labels = c("Negative", "Positive"))+
  geom_line(size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
  theme_minimal_hgrid() +
  scale_color_calc() +
  labs(y= "Reported Cases or Tests", x = "Days since reporting 10th case", 
       color = "State", fill = "Tests Per Day") +
  ggtitle("Cumulative reported cases and tests per day\n by days since the 10th reported case.") 


states.pos %>% 
  filter(state == the.state) %>% 
  ggplot(aes(since10, pos100k, group = state, color = state)) +
  geom_col(data = state.testing, aes(x = since10, val100k, fill = name),
           position="stack", alpha = 0.4, color = "white") +
  scale_fill_manual(values = c("deepskyblue", "red"),
                    labels = c("Negative", "Positive"))+
  geom_line(size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
  theme_minimal_hgrid() +
  scale_color_calc() +
  labs(y= "Reported Cases or Tests per 100,000 People ", x = "Days since reporting 10th case", 
       color = "State", fill = "Tests Per Day") +
  ggtitle("Cumulative reported cases and tests per day per\n100,000 people by days since the 10th reported case.") 

