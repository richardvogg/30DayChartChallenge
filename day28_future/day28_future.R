
library(dplyr)
library(rtweet)
library(ggplot2)
library(stringr)
library(ggfx)
library(harrypotter)


sysfonts::font_add_google(name = "Just Another Hand", "JustHand")
sysfonts::font_add_google(name = "Teko", "Teko")
showtext::showtext_auto()

#secret keys
api_key = '***'
api_secret_key = '***'
access_token = '***'
access_secret_token = '***'

token <- create_token("Richards Postman App", api_key, api_secret_key, access_token, access_secret_token)

df <- get_timeline(user = "richard_vogg")

df %>%
  filter(str_detect(text, "#30DayChartChallenge")) %>% 
  tidyr::separate(text, into = c(NA, "Day", "text"), sep = " - ")  %>%
  mutate(Day = readr::parse_number(Day)) %>% 
  mutate(text = str_replace_all(text, "\\n"," ") %>%
           str_remove("Code:.*$")) %>% 
  add_row(Day = 28, text = "Future  All my tweets for the challenge so far with count of likes. What is uncertain is the future: What will be my results for the last days?", favorite_count = 1) %>%
  add_row(Day = 29, text = "Deviations", favorite_count = 1) %>%
  add_row(Day = 30, text = "3D", favorite_count = 1) %>%
  ggplot(aes(x = Day, y = favorite_count)) + 
  as_reference(
    geom_col(aes(fill = factor((Day-1)  %/% 6 %% 2)), width = 1, col = NA),
    id = "column"
  ) +
  with_blend(
    geom_text(aes(label = text, y = 0), hjust = 0, size = 3),
    bg_layer = "column", blend_type = "xor"
  ) +
  geom_text(aes(label = Day, y = -1), size = 3, family = "Teko") +
  coord_flip(xlim = c(30,1)) +
  harrypotter::scale_fill_hp(option = "Ravenclaw", discrete = TRUE) +
  labs(y = "likes", x = "",
       title = "An overview over my tweets for this challenge so far.",
       subtitle = "Number of likes is shown with the color bar.") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = "JustHand"),
        plot.title = element_text(size = 40),
        plot.subtitle = element_text(size = 30),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18))
