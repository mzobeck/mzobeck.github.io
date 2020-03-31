# State functions 
state.prev <- function(x) {
  x %>% 
    ggplot(aes(since10, positive, group = state, color = state)) +
    geom_line(data=states.top20.all, aes(since10, positive), color = "grey", alpha = 0.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.08, "inches"))) +
    geom_label_repel(data = states.top10.all %>%
                       filter(date == max(date)) %>% 
                       arrange(desc(since10)) %>% 
                       filter(state != the.state) %>% 
                       head(6),
                     aes(label = state), show.legend = FALSE,
                     nudge_x = 0.25, nudge_y = 0.25, 
                     point.padding = 0.2, box.padding = 0.35, color = "grey") +
    geom_line(alpha = 0.7, size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
    theme_minimal_hgrid() +
    scale_color_calc() +
    labs(y= "Reported Cases", x = "Days since reporting 10th case", color = "State", caption = "\nGrey arrows represent the top 20 states by reported prevalence.") +
    ggtitle("Cumulative reported cases by days\nsince the 10th reported case.")
}

state.logprev <- function(x) {
  x %>% 
    ggplot(aes(since10, positive, group = state, color = state)) +
    geom_line(data=states.top20.all, aes(since10, positive), color = "grey", alpha = 0.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.08, "inches"))) +
    geom_label_repel(data = states.top10.all %>%
                       filter(date == max(date)) %>% 
                       arrange(desc(positive)) %>% 
                       filter(state != the.state) %>% 
                       head(6),
                     aes(label = state), show.legend = FALSE,
                     nudge_x = 0.25, nudge_y = 0.25, 
                     point.padding = 0.2, box.padding = 0.35, color = "grey") +
    geom_line(alpha = 0.7, size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
    theme_minimal_hgrid() +
    scale_color_calc() +
    scale_y_log10() +
    labs(y= "Logarithmic Reported Cases", x = "Days since reporting 10th case", color = "State", caption = "\nGrey arrows represent the top 20 states by reported prevalence with the top 6 labeled.") +
    ggtitle("Logarithmic cumulative reported cases by days\nsince the 10th reported case.")
}



state.death <- function(x) {
  x %>% 
    ggplot(aes(since10, death, group = state, color = state)) +
    geom_line(data=states.top20d.all, aes(since10, death), color = "grey", alpha = 0.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.08, "inches"))) +
    geom_label_repel(data = states.top10d.all %>%
                       filter(date == max(date)) %>% 
                       arrange(desc(since10)) %>% 
                       filter(state != the.state) %>% 
                       head(6),
                     aes(label = state), show.legend = FALSE,
                     nudge_x = 0.25, nudge_y = 0.25, 
                     point.padding = 0.2, box.padding = 0.35, color = "grey") +
    geom_line(alpha = 0.7, size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) +
    theme_minimal_hgrid() +
    scale_color_calc()+ 
    labs(y= "Reported Deaths", x = "Days since reporting 10th death", color = "State", caption = "\nGrey arrows represent the top 20 states by reported deaths.") +
    ggtitle("Number of cumulative reported deaths by days\nsince the 10th reported death.")
}




state.ldeath <- function(x) {
  x %>% 
    ggplot(aes(since10, death, group = state, color = state)) +
    geom_line(data=states.top20d.all, aes(since10, death), color = "grey", alpha = 0.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.08, "inches"))) +
    geom_label_repel(data = states.top10d.all %>%
                       filter(date == max(date)) %>% 
                       arrange(desc(since10)) %>% 
                       filter(state != the.state) %>% 
                       head(6),
                     aes(label = state), show.legend = FALSE,
                     nudge_x = 0.25, nudge_y = 0.25, 
                     point.padding = 0.2, box.padding = 0.35, color = "grey") +
    geom_line(alpha = 0.7, size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
    theme_minimal_hgrid() +
    scale_color_calc() + 
    scale_y_log10() + 
    labs(y= "Logarithmic reported deaths", x = "Days since reporting 10th death", color = "State", caption = "\nGrey arrows represent the top 20 states by reported deaths") +
    ggtitle("Logarithmic number of cumulative reported deaths\nby days since the 10th reported death.")
}



state.prev100 <- function(x) {
  x %>% 
    ggplot(aes(since10, pos100k, group = state, color = state)) +
    geom_line(data=states.top20.all, aes(since10, pos100k), color = "grey", alpha = 0.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.08, "inches"))) +
    geom_label_repel(data = states.top10.all %>%
                       filter(date == max(date)) %>% 
                       arrange(desc(since10)) %>% 
                       filter(state != the.state) %>% 
                       head(6),
                     aes(label = state), show.legend = FALSE,
                     nudge_x = 0.25, nudge_y = 0.25, 
                     point.padding = 0.2, box.padding = 0.35, color = "grey") +
    geom_line(alpha = 0.7, size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
    theme_minimal_hgrid() +
    scale_color_calc()  +
    labs(y= "Reported Cases", x = "Days since reporting 10th case", color = "State", caption = "\nGrey arrows represent the top 20 states by reported prevalence.") +
    ggtitle("Cumulative reported cases per 100,000 people by days\nsince the 10th reported case.")
}



state.lprev100 <- function(x) {
  x %>% 
    ggplot(aes(since10, pos100k, group = state, color = state)) +
    geom_line(data=states.top20.all, aes(since10, pos100k), color = "grey", alpha = 0.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.08, "inches"))) +
    geom_label_repel(data = states.top10.all %>%
                       filter(date == max(date)) %>% 
                       arrange(desc(since10)) %>% 
                       filter(state != the.state) %>% 
                       head(6),
                     aes(label = state), show.legend = FALSE,
                     nudge_x = 0.25, nudge_y = 0.25, 
                     point.padding = 0.2, box.padding = 0.35, color = "grey") +
    geom_line(alpha = 0.7, size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
    theme_minimal_hgrid() +
    scale_color_calc() +
    scale_y_log10() +
    labs(y= "Logarithmic reported cases", x = "Days since reporting 10th case", color = "State") +
    ggtitle("Logarithmic cumulative reported cases\nper 100,000 people by days since the 10th reported case.")
}



state.death100 <- function(x) {
  x %>% 
    ggplot(aes(since10, death100k, group = state, color = state)) +
    geom_line(data=states.top20d.all, aes(since10, death100k), color = "grey", alpha = 0.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.08, "inches"))) +
    geom_label_repel(data = states.top10d.all %>%
                       filter(date == max(date)) %>% 
                       arrange(desc(since10)) %>% 
                       filter(state != the.state) %>% 
                       head(6),
                     aes(label = state), show.legend = FALSE,
                     nudge_x = 0.25, nudge_y = 0.25, 
                     point.padding = 0.2, box.padding = 0.35, color = "grey") +
    geom_line(alpha = 0.7, size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) +
    theme_minimal_hgrid() +
    scale_color_calc()+ 
    labs(y= "Reported Deaths per 100,000 people", x = "Days since reporting 10th death", color = "State", caption = "\nGrey arrows represent the top 20 states by reported prevalence.") +
    ggtitle("Number of cumulative reported deaths per\n100,000 people by days since the 10th reported death.")
}


state.ldeath100 <- function(x) {
  x %>% 
    ggplot(aes(since10, death100k, group = state, color = state)) +
    geom_line(data=states.top20d.all, aes(since10, death100k), color = "grey", alpha = 0.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.08, "inches"))) +
    geom_label_repel(data = states.top10d.all %>%
                       filter(date == max(date)) %>% 
                       arrange(desc(since10)) %>% 
                       filter(state != the.state) %>% 
                       head(6),
                     aes(label = state), show.legend = FALSE,
                     nudge_x = 0.25, nudge_y = 0.25, 
                     point.padding = 0.2, box.padding = 0.35, color = "grey") +
    geom_line(alpha = 0.7, size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) +
    theme_minimal_hgrid() +
    scale_color_calc() + 
    scale_y_log10()+
    labs(y= "Reported deaths per 100,000 people", x = "Days since reporting 10th death", color = "State", caption = "\nGrey arrows represent the top 20 states by reported prevalence.") +
    ggtitle("Logarithmic number of cumulative reported deaths per\n100,000 people by days since the 10th reported death.")
}



state.casenew <- function(x) {
  x %>% 
    ggplot(aes(since10, new.cases, group = state, color = state)) +
    geom_line(data = states.new.top20, aes(since10, new.cases), color = "grey", alpha = 0.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.08, "inches"))) + 
    geom_label_repel(data = states.new.top20 %>%
                       filter(date == max(date)) %>% 
                       arrange(desc(since10)) %>% 
                       filter(state != the.state) %>% 
                       head(6),
                     aes(label = state), show.legend = FALSE,
                     nudge_x = 0.25, nudge_y = 0.25, 
                     point.padding = 0.2, box.padding = 0.35, color = "grey") +
    geom_line(alpha = 0.7, size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
    theme_minimal_hgrid() +
    scale_color_calc()+ 
    labs(y= "New reported cases", x = "Days since reporting 10th case", color = "State", caption = "\nGrey arrows represent the top 20 states by reported prevalence.") +
    ggtitle("Number of reported new cases\nby days since the 10th reported case.")
}



state.lcasenew <- function(x) {
  x %>% 
    ggplot(aes(since10, new.cases, group = state, color = state)) +
    geom_line(data = states.new.top20, aes(since10, new.cases), color = "grey", alpha = 0.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.08, "inches"))) + 
    geom_label_repel(data = states.new.top20 %>%
                       filter(date == max(date)) %>% 
                       arrange(desc(since10)) %>% 
                       filter(state != the.state) %>% 
                       head(6),
                     aes(label = state), show.legend = FALSE,
                     nudge_x = 0.25, nudge_y = 0.25, 
                     point.padding = 0.2, box.padding = 0.35, color = "grey") +
    theme_minimal_hgrid() +
    geom_line(alpha = 0.7, size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
    scale_color_calc() +
    scale_y_log10()+
    labs(y= "Logarithmic new reported cases", x = "Days since reporting 10th case", color = "State", caption = "\nGrey arrows represent the top 20 states by reported prevalence.") +
    ggtitle("Logarithmic number of daily reported new cases\nby days since the 10th reported case.")
}




state.deathnew <- function(x) {
  x %>% 
    ggplot(aes(since10, new.deaths, group = state, color = state)) +
    geom_line(data = states.newdeaths.top20, aes(since10, new.deaths), color = "grey", alpha = 0.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.08, "inches"))) + 
    geom_label_repel(data = states.newdeaths.top20 %>%
                       filter(date == max(date)) %>% 
                       arrange(desc(since10)) %>% 
                       filter(state != the.state) %>% 
                       head(6),
                     aes(label = state), show.legend = FALSE,
                     nudge_x = 0.25, nudge_y = 0.25, 
                     point.padding = 0.2, box.padding = 0.35, color = "grey") +
    geom_line(alpha = 0.7, size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
    theme_minimal_hgrid() +
    scale_color_calc() + 
    labs(y= "New reported deaths", x = "Days since reporting 10th case", color = "State", caption = "\nGrey arrows represent the top 20 states by reported prevalence.") +
    ggtitle("Number of reported new deaths\nby days since the 10th reported case.")
}





state.ldeathnew <- function(x) {
  x %>% 
    ggplot(aes(since10, new.deaths, group = state, color = state)) +
    geom_line(data = states.newdeaths.top20, aes(since10, new.deaths), color = "grey", alpha = 0.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.08, "inches"))) + 
    geom_label_repel(data = states.newdeaths.top20 %>%
                       filter(date == max(date)) %>% 
                       arrange(desc(since10)) %>% 
                       filter(state != the.state) %>% 
                       head(6),
                     aes(label = state), show.legend = FALSE,
                     nudge_x = 0.25, nudge_y = 0.25, 
                     point.padding = 0.2, box.padding = 0.35, color = "grey") +
    geom_line(alpha = 0.7, size = 1.5, arrow = arrow(angle = 15, type = "closed", length = unit(0.1, "inches"))) + 
    theme_minimal_hgrid() +
    scale_color_calc() +
    scale_y_log10() +
    labs(y= "Logarithmic new reported deaths", x = "Days since reporting 10th case", color = "State", caption = "\nGrey arrows represent the top 20 states by reported prevalence.") +
    ggtitle("Logarithmic number of reported new deaths\nby days since the 10th reported case.")
}

