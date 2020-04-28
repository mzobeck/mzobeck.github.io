the.state <- states.top10$state[1]

current.state <- states.pos %>% 
  filter(state == the.state)

current.state.death <- states.deaths %>% 
  filter(state == the.state)

current.state.new <- states.newcases %>% 
  filter(state == the.state)

current.state.newdeath <- states.newdeaths %>% 
  filter(state == the.state)

state.testing <- states.pos %>% 
  filter(state == the.state) %>% 
  left_join(tracking, by = c("date", "state")) %>% 
  select(date, state, population, since10, negative_increase, positive_increase) %>% 
  mutate(pct.increase = positive_increase/(negative_increase + positive_increase)) %>% 
  group_by(date, state, since10, population, pct.increase) %>% 
  pivot_longer(negative_increase:positive_increase) %>% 
  mutate(val100k = (value/population)*100000) %>% 
  mutate(name = factor(name,
                        levels = c("negative_increase","positive_increase"),
                        labels = c("Negative","Positive")))
  
test.pct <- state.testing %>% 
  ungroup() %>% 
  filter(name == "Negative") %>% 
  distinct() %>% 
  mutate(pct.increase = ifelse(is.nan(pct.increase), NA, pct.increase),
         pct.increase = ifelse(value < 500, pct.increase == NA, pct.increase ),
         pct.increase = ifelse(is.na(pct.increase), NA, paste0(round(pct.increase*100), "%")),
         value = 250)

state.testing %>% 
  ggplot() + 
  geom_col(aes(x = since10, y = value, fill = name), position="stack", alpha = 0.4, color = "white") + 
  scale_fill_manual(values = c( "deepskyblue", "red")) 
  geom_line(data = current.state, aes(since10, positive, group = state, color = state), color = "blue",
            size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) +  
  theme_minimal_hgrid() +
  scale_color_calc() +
  labs(y= "Reported Cases or Tests", x = "Days since reporting 10th case", 
       color = "State", fill = "Tests Per Day",
       caption = "\nBlue arrow represent the total cumulative cases in the state.")  +
  ggtitle("Tests per day by days since the 10th reported case.") +
  geom_text(data = test.pct, 
            mapping = aes(x = since10, y = value, label = pct.increase), 
            colour = "black", size = 2.5, 
            position=position_stack(vjust=0.5))




###
?scale_fill_manual
current.state %>% 
  ggplot(aes(since10, positive, group = state, color = state)) +
  geom_col(data = state.testing, aes(x = since10, value, fill = name),
           position="stack", alpha = 0.4, color = "white") +
  scale_fill_manual(values = c("red", "deepskyblue"))
geom_line(size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
  theme_minimal_hgrid() +
  scale_color_calc() +
  labs(y= "Reported Cases or Tests", x = "Days since reporting 10th case", 
       color = "State", fill = "Tests Per Day",
       caption = "\nBlue arrow represent the total cumulative cases in the state.") +
  ggtitle("Tests per day by days since the 10th reported case.") 


state.testcase(state.testing)

ggplot() +
geom_col(data = state.testing, aes(x = since10, value, 
             fill = name),
         position="stack", alpha = 0.4, color = "white")

%>% 
  mutate(name = factor(name, levels = c("negative_increase","positive_increase")))

state.testing %>% 
  ggplot() +
  geom_col(data = state.testing, aes(x = since10, value, fill = name),
                         position="stack", alpha = 0.4, color = "white") +
             scale_fill_manual(values = c("deepskyblue", "red"),
                               labels = c("Negative", "Positive"))+
  geom_line(data = current.state, 
            aes(since10, positive, group = state, color = state),
            size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
             theme_minimal_hgrid() +
             scale_color_calc() +
             labs(y= "Reported Cases or Tests", x = "Days since reporting 10th case", 
                  color = "State", fill = "Tests Per Day",
                  caption = "\nBlue arrow represent the total cumulative cases in the state.") +
             ggtitle("Tests per day by days since the 10th reported case.") 
             
             
             
             
             current.state %>% 
               ggplot(aes(since10, positive, group = state, color = state)) +
               geom_col(data = state.testing %>% 
                          mutate(name = factor(name,
                                               levels = c("negative_increase","positive_increase"),
                                               labels = c("Negative","Positive"))), 
                        aes(x = since10, value, fill = name),
                        position="stack", alpha = 0.4, color = "white") +
               scale_fill_manual(values = c("deepskyblue", "red"))+
               geom_line(size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
               theme_minimal_hgrid() +
               scale_color_calc() +
               labs(y= "Reported Cases or Tests", x = "Days since reporting 10th case", 
                    color = "State", fill = "Tests Per Day",
                    caption = "\nBlue arrow represent the total cumulative cases in the state.") +
               ggtitle("Tests per day by days since the 10th reported case.") 
f             
             
state.testcase(state.testing)
state.testcase100(state.testing)
###
state.testcase <- function(x) {
  current.state %>% 
    ggplot(aes(since10, positive, group = state, color = state)) +
    geom_col(data = x, aes(x = since10, value, fill = name),
             position="stack", alpha = 0.4, color = "white") +
    scale_fill_manual(values = c("deepskyblue", "red"),
                      labels = c("Negative", "Positive"))+
    geom_line(size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
    theme_minimal_hgrid() +
    scale_color_calc() +
    labs(y= "Reported Cases or Tests", x = "Days since reporting 10th case", 
         color = "State", fill = "Tests Per Day",
         caption = "\nBlue arrow represent the total cumulative cases in the state.") +
    ggtitle("Tests per day by days since the 10th reported case.") 
}

state.testcase100 <- function(x) {
  current.state %>% 
    ggplot(aes(since10, pos100k, group = state, color = state)) +
    geom_col(data = x, aes(x = since10, val100k, fill = name),
             position="stack", alpha = 0.4, color = "white") +
    scale_fill_manual(values = c("deepskyblue", "red"),
                      labels = c("Negative", "Positive"))+
    geom_line(size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
    theme_minimal_hgrid() +
    scale_color_calc() +
    labs(y= "Reported Cases or Tests per 100,000 People ", x = "Days since reporting 10th case", 
         color = "State", fill = "Tests Per Day",
         caption = "\nBlue arrow represent the total cumulative cases per 100,000 people in the state.") +
    ggtitle("Tests per day per 100,000 people\nby days since the 10th reported case.") 
}
             