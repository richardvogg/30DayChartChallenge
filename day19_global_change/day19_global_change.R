library(ggplot2)
library(dplyr)
library(readxl)
library(ggrepel)
library(ggfx)

sysfonts::font_add_google(name = "Prata", "Prata")
showtext::showtext_auto()

# Load data (same as day 3)

df <- lapply(2006:2020,function(x) {
  read_xlsx(paste0("day03_historical/data/fsi-",x,".xlsx")) %>%
    select(1:16)
}) %>% 
  do.call(rbind,.) %>%
  mutate(Year=format(Year,"%Y"))


# Select only necessary columns
df_small <- df %>%
  select(Country, Year, starts_with("S1"), starts_with("C2")) %>%
  mutate(Year = as.numeric(Year))

# Find interesting countries (min, max, biggest change etc.)

country_summ <- df_small %>%
  group_by(Country) %>%
  summarise(last = last(`S1: Demographic Pressures`),
            first = 10-first(`S1: Demographic Pressures`),
            max = max(`S1: Demographic Pressures`),
            min = 10-min(`S1: Demographic Pressures`),
            max_mean = mean(`S1: Demographic Pressures`),
            change = max - (10-min),
            neg_change = last - (10-first),
            pos_change = (10-first) - last)

country_names <- lapply(names(country_summ)[-c(1,2,3,4)], function(x) {
  country_summ %>%
    slice_max(eval(parse(text=x)), n = 1, with_ties = FALSE) %>%
    .$Country
}) %>% unlist() %>% unique()


df_summ <- df_small %>%
  group_by(Year) %>%
  summarise(across(c(`S1: Demographic Pressures`,`C2: Factionalized Elites`), mean))


df_small %>%
  ggplot(aes(x = Year, y = `S1: Demographic Pressures`)) +
  geom_line(aes(group = Country), alpha = 0.5, col = "grey") +
  with_inner_glow(
    geom_line(data = df_small %>% filter(Country %in% country_names), 
              aes(group = Country), col = "chocolate4"),
    colour = "chocolate4"
  ) +
  with_outer_glow(
    geom_line(data = df_summ, aes(group = 1), col = "chocolate4", size = 2),
    colour = "yellow"
  ) +
  geom_text_repel(data = df_small %>% filter(Country %in% country_names, Year == 2020),
            aes(label = Country), 
            col = "chocolate4", hjust = "left", nudge_x = 0.1, size = 4, family = "Prata",
            min.segment.length = 5) +
  geom_text_repel(data = df_summ %>% filter(Year == 2020),
            aes(label = "Global mean"), 
            col = "chocolate4", hjust = "left", nudge_x = 0.1, size =5, family = "Prata") +
  scale_y_continuous(breaks = c(0.5,10), labels = c("stable","vulnerable")) +
  expand_limits(x= 2022) +
  labs(title = "Demographic Pressures",
       subtitle = "Worldwide Average: Demographic Pressures are contributing \nless to vulnerability of states.",
       caption = "Data: The Fragile State Index") +
  theme_light() +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 20, family = "Prata", color = "chocolate4"),
        plot.subtitle = element_text(size = 15, family = "Prata"),
        axis.text = element_text(size = 12, family = "Prata"),
        plot.caption = element_text(size = 11, family = "Prata"))
