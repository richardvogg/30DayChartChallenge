library(dplyr)
library(ggplot2)
library(ggfx)
library(ggtext)

sysfonts::font_add_google(name = "Itim", "Itim")
showtext::showtext_auto()

df <- read.csv("C:/Richard/R and Python/Datasets/mean-height-males-vs-females.csv")

df_final <- df %>%
  filter(Year >= 1996) %>% 
  add_count(Entity) %>%
  filter(n == 2) %>% 
  group_by(Entity) %>%
  summarise(male_mean_height = max(Mean.male.height..cm., na.rm = TRUE),
            female_mean_height = max(Mean.female.height..cm., na.rm = TRUE),
            continent = last(Continent)) %>%
  tidyr::pivot_longer(cols = ends_with("height"), names_to = "sex", values_to = "height")


df_final %>%
  tidyr::separate(sex, into = "sex") %>%
  ggplot(aes(x = continent, y = height, fill = sex, col = sex)) + 
  with_inner_glow(
    geom_violin(alpha = 0.9)
  ) +
  scale_fill_manual(values = c("darkblue", "orange")) +
  scale_colour_manual(values = c("darkblue", "orange")) +
  labs(title = "<span style='color:orange;'>Male</span> and <span style='color:darkblue;'>female</span> body height distribution across continents",
       subtitle = "Showing average height per country in each continent.",
       caption = "Data: Our World in Data",
       y = "height [cm]",
       x = "") +
  theme_light() +
  theme(plot.title = element_markdown(size = 20),
        legend.position = "none",
        text = element_text(family = "Itim"),
        plot.subtitle = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
  
