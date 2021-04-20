#Data: https://scrippsco2.ucsd.edu/data/atmospheric_co2/primary_mlo_co2_record.html

library(dplyr)
library(ggplot2)
library(ggfx)
library(ggrepel)
library(patchwork)

sysfonts::font_add_google(name = "Jura", "Jura")
showtext::showtext_auto()

df <- read.csv("day20_upwards/monthly_in_situ_co2_mlo.csv", skip = 3)

df_final <- df %>%
  slice(-c(1,2)) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1900-01-01"),
         CO2.1 = as.numeric(CO2.1),
         Mn = as.numeric(Mn)) %>% 
  filter(CO2.1>0)


one_year <- df_final %>%
  filter(Yr == "1990") %>%
  ggplot(aes(x = Date, y = CO2.1, group = 1)) + 
  with_outer_glow(
    geom_line(size = 1),
    colour = "green"
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b") +
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(colour = "darkgreen"))


df_final %>%
  ggplot(aes(x=Date, y = CO2.1, group = 1)) + 
  with_outer_glow(
    geom_line(size = 1), colour = "green"
  ) +
  with_outer_glow(
    annotate("text", x = as.Date("1948-01-01"), y = 315, 
             label = "March 1958 \n315 ppm", size = 5, hjust = 0), 
    colour = "green"
  ) +
  with_outer_glow(
    annotate("text", x = as.Date("2022-01-01"), y = 415, 
             label = "January 2021 \n415 ppm", size = 5, hjust = 0), 
    colour = "green"
  ) +
  with_outer_glow(
    annotate("text", x = as.Date("1958-03-01"), y = 410,
             label = "The Keeling Curve", size = 10, hjust = 0, family = "Jura"),
    colour = "green"
  ) +
  annotate("text", x = as.Date("1958-03-01"), y = 400,
           label = paste0("Charles Keeling started measuring carbon dioxide \nat the ",
                          "Mauna Loa observatory in Hawaii in 1958. \nFar away from larger sources ",
                          "of pollution, \nit measures carbon dioxide almost without noise. ",
                          "\nThe curve shows the concerning upwards \ntrend of concentration of ",
                          "carbon dioxide \nin the atmosphere with a seasonal \ntrend - ",
                          "during summer on \nthe northern hemisphere \nconcentration decreases."),
           hjust = 0, vjust = 1, size = 5, family = "Jura") +
  expand_limits(x = c(as.Date("1950-01-01"), as.Date("2030-01-01"))) +
  inset_element(one_year, left = 0.6, right = 0.95, bottom = 0.05, top = 0.4) &
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 12),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"))

# output 1000 x 600
  