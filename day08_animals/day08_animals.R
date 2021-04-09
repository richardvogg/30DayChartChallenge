devtools::install_github("rensa/ggflags")

library(ggflags)
library(ggplot2)
library(dplyr)
library(tradestatistics)
library(ggtext)

df <- ots_products

animals <- df %>% 
  filter(product_code %in% c('0101','0102','0103','0104','0105'))

trades <- ots_create_tidy_data(
  years = c(1998, 2018),
  reporters = "all",
  products = animals$product_code,
  table = "yrc"
) %>%
  mutate(year = factor(year),
         import_value_usd = ifelse(import_value_usd == 0, 1, import_value_usd))


#find interesting countries

trades %>%
  group_by(reporter_iso, year) %>%
  summarise(n=n(), sum(import_value_usd)) %>%
  arrange(desc(n), reporter_iso, year) %>% View()


selected <- trades %>% 
  filter(reporter_iso %in% c("chn")) %>%
  mutate(flag = case_when(
    reporter_iso == "chn" ~ "cn"
  ))
  
library(ggfx)

trades %>%
  ggplot(aes(x = import_value_usd, colour = year)) + 
  with_outer_glow(
    geom_density(size = 1), sigma = 1
  ) +
  geom_density(aes(fill = year), alpha = 0.1) +
  with_outer_glow(
    geom_point(data = selected, 
               aes(y=0.3, col = year), size = 7, shape = 21, stroke = 2),
    sigma = 1
  ) +
  with_inner_glow(
    geom_flag(data = selected, 
              aes(y=0.3, country = flag), size = 7),
    sigma = 1, colour = "white"
  ) +
  scale_x_log10(labels = scales::dollar_format()) +
  scale_color_manual(values = c("darkorange", "blue")) +
  facet_grid(product_shortname_english ~ .) +
  labs(title = "Countries and animal trade",
       subtitle = paste0("Import of living animals has increased between ",
       "<span style='color:darkorange;'>1998</span> ",
       "and <span style='color:blue;'>2018</span>. <br>",
       "Import value distribution of all countries shown, and China as an example for a country with large increase."), 
       x = "Import value [$]", 
       y = "Proportion of countries") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(),
        plot.subtitle = element_markdown(vjust = 0.1, lineheight = 1.5),
        legend.position = "none")

