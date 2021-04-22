library(dplyr)
library(ggplot2)
library(stringr)
library(gganimate)
library(reticulate) #Python connection for encoding of weird letters

use_python("C:/Users/Richard/anaconda3",required=TRUE)
source_python("day17_pop_culture/change_encoding.py")

#fonts

sysfonts::font_add_google(name = "Jura", "Jura")
showtext::showtext_auto()

df <- read.csv("C:/Richard/R and Python/Datasets/soccer_results_kaggle.csv")

germany_df <- df %>% 
  mutate(city = sapply(city, change_encoding)) %>%
  filter(str_detect(home_team, "Germany") | str_detect(away_team, "Germany")) %>%
  add_count(tournament) %>%
  mutate(result = case_when(away_score == home_score ~ "Draw",
    str_detect(home_team, "Germany") & home_score > away_score ~ "Victory",
    str_detect(home_team, "Germany") & home_score < away_score ~ "Defeat",
    str_detect(away_team, "Germany") & home_score > away_score ~ "Defeat",
    str_detect(away_team, "Germany") & home_score < away_score ~ "Victory",
    TRUE ~ "Error")) %>%
  mutate(goal_difference = abs(away_score - home_score),
         goal_sum = away_score + home_score,
         result_difference = ifelse(result == "Defeat", -goal_difference, goal_difference)) %>%
  mutate(date = as.Date(date),
         year = as.numeric(format(date, "%Y")),
         date_id = rank(date,ties.method = "first")) %>%
  mutate(scoreboard = paste0(home_team," ", home_score, 
                             " : ", away_score," ", away_team,
                            "\n",tournament,"\n",city,", ",country,", ",year)) %>%
  group_by(tournament) %>%
  mutate(highest_diff = rank(-goal_difference, ties.method = "first"),
         most_goals = rank(-goal_sum, ties.method = "first")) %>%
  ungroup() %>%
  mutate(highlight = ifelse((n > 100 & 
                              (highest_diff < 5 | most_goals < 3)) |
                              (date_id %in% c(1, 208, 212, 305, 362, 385, 442, 
                                              516, 556, 631, 880, 881, 958, 961)), 
                            TRUE, FALSE)) %>%
  mutate(date_id_final = ifelse(highlight == TRUE, date_id, NA))
  



g <- germany_df %>%
  ggplot(aes(x = date, y = result_difference, xend = date, yend = 0)) + 
  geom_label(aes(label = scoreboard, x = as.Date("1970-01-01"), y = 15)) +
  geom_segment(data = germany_df %>% select(-date_id_final)) +
  geom_point(data = germany_df %>% select(-date_id_final),
             aes(col = result)) +
  geom_segment(size = 1, col = "red") +
  geom_point(size = 4, col = "red") +
  scale_color_manual(values = c("goldenrod","red","black")) +
  labs(title = "A brief history of Germany's international soccer matches",
       subtitle = "Remarkable results quickly highlighted",
       y = "Goal difference", x = "") +
  expand_limits(y = 17) +
  theme_light() +
  theme(legend.position = c(0.1,0.8)) +
  transition_states(date_id_final, state_length = 5) +
  ease_aes('linear')

anim <- animate(g, nframes = 500, width = 650, height = 550)


anim_save("day22_animation/plot.gif")
