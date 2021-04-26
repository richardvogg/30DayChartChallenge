library(dplyr)
library(ggplot2)
library(forcats)
library(ggfx) # filter to smooth lines

# Data from http://ghdx.healthdata.org/gbd-results-tool

sysfonts::font_add_google(name = "Montserrat", "Montserrat")
showtext::showtext_auto()

df <- lapply(1:8, function(x) {
  data.table::fread(
    paste0("C:/Richard/R and Python/Datasets/IHME-GBD_2019/IHME-GBD_2019_DATA-88134883-",
           x,".csv"))
}) %>% do.call(rbind,.) %>%
  filter(location %in% c("Chile","Germany", "United States of America",
                         "Nigeria", "Japan", "Peru"), 
         metric == "Percent", measure == "Deaths")

top3 <- df %>%
  group_by(cause, location) %>%
  summarise(mean_val = mean(val)) %>%
  group_by(location) %>%
  mutate(rank = rank(-mean_val, ties.method = "first")) %>%
  filter(rank <= 3)



top12_country <- df %>%
  inner_join(top3, by = c("cause", "location")) 


label_df <- top12_country %>%
  filter((rank == 1 & year == 1991) |
           (rank == 2 & year == 1999) |
           (rank == 3 & year == 2000))



top12_country %>%
  ggplot(aes(x = year, y = val, col = cause, group = cause)) + 
  with_outer_glow(
    geom_line(size = 1), colour = "white"
  ) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = cause), 
              alpha = 0.2, col = NA) +
  geom_text(data = subset(label_df, rank == 3),
            aes(label = cause), hjust = 0, nudge_y = -0.015) +
  geom_text(data = subset(label_df, rank != 3),
            aes(label = cause), hjust = 0, nudge_y = 0.015) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2019)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  facet_wrap(~ location) +
  labs(title = "Most frequent death causes per country over time",
       subtitle = "Confidence bands show the realiability of the results",
       caption = paste("Global Burden of Disease Study 2019 (GBD 2019) Results.",
        "\nSeattle, United States: Institute for Health Metrics and Evaluation (IHME), 2020.",
        "\nAvailable from http://ghdx.healthdata.org/gbd-results-tool.")) +
  theme_light() +
  theme(legend.position = "none",
        text = element_text(family = "Montserrat"),
        plot.title = element_text(size = 20),
        axis.title = element_blank(),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 15),
        strip.background = element_rect(fill = "grey20"))

