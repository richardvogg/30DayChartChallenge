devtools::install_github("rensa/ggflags")

library(ggflags)
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

final <- df %>%
  filter(Year %in% c(2010, 2020)) %>%
  select(Country, Year, Total) %>%
  tidyr::pivot_wider(id_cols = Country, names_from = Year, 
                     values_from = Total) %>%
  mutate(change = `2020`-`2010`) %>%
  tidyr::pivot_longer(cols = 2:3, names_to = "Year", values_to = "Total") %>%
  mutate(code_icons=case_when(Country=="Chile" ~ "cl",
                              Country=="Venezuela" ~ "ve",
                              Country=="Cuba" ~ "cu",
                              Country=="United Arab Emirates" ~ "ae",
                              Country=="Greece" ~ "gr",
                              Country=="Brazil" ~ "br",
                              Country=="Finland" ~ "fi",
                              Country=="Israel" ~ "il",
                              Country=="Sweden" ~ "se",
                             #Country=="Netherlands" ~ "nl",
                             #Country=="Egypt" ~ "eg",
                             #Country=="France" ~ "fr",
                             Country=="Germany" ~ "de",
                             Country=="Bahrain" ~ "bh",
                             #Country=="United Kingdom" ~ "gb",
                             Country=="United States" ~ "us",
                             Country=="Peru" ~ "pe",
                             #Country=="South Korea" ~ "kr",
                             Country=="China" ~ "cn",
                             Country=="Colombia"~"co",
                             Country=="Bangladesh"~"bd",
                             #Country=="Vietnam"~"vn",
                             Country=="Japan"~"jp")) %>%
  filter(!is.na(code_icons))

## Final viz

#Adding glow to lines and flags to make them look smoother

final %>%
  ggplot(aes(x = Year, y = Total)) +
  with_outer_glow(
    geom_line(aes(group = Country, 
                  col = change >= 0), 
              size = 1)
  ) +
  with_inner_glow(
    geom_flag(aes(country = code_icons),size=8),
    id = "flags"
  ) +
  with_outer_glow(
    "flags",colour = "white"
  ) +
  geom_text_repel(data = subset(final, Year == 2010),
            aes(label = Country), hjust = 1, nudge_x = -0.06,
            segment.color = NA, family = "Prata") +
  geom_text_repel(data = subset(final, Year == 2020),
            aes(label = Country), hjust = 0, nudge_x = 0.06,
            segment.color = NA, family = "Prata") +
  scale_x_discrete(position = "top") +
  scale_color_manual(values = c("darkgreen","red")) +
  labs(title = "The Fragile State Index",
       subtitle = "The index shows the vulnerability of countries, \nhigher values indicate less stability.",
       caption = "Showing selected countries | Data: The Fragile State Index") +
  theme_light() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15, family = "Prata"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 20, family = "Prata"),
        plot.subtitle = element_text(size = 15, family = "Prata"),
        plot.caption = element_text(size = 10, family = "Prata"),
        panel.background = element_rect(fill = "khaki1"))
  


