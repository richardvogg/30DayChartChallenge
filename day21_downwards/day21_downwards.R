library(hockeystick)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggfx)

sysfonts::font_add_google(name = "Jura", "Jura")
showtext::showtext_auto()

ice <- get_seaice()


argentina <- ggplot()+
  borders("world", regions = "Argentina", fill = "turquoise3") +
  coord_quickmap() +
  theme_void()

germany <- ggplot() +
  borders("world", regions = "Germany", fill = "turquoise3") +
  coord_quickmap() +
  theme_void()


germanys <- germany + germany + germany + germany + germany + germany + germany 

ggplot(ice, aes(x = date, y = extent, group = 1)) + 
  with_outer_glow(geom_line(size = 1) )  +
  with_outer_glow(
    geom_smooth(method = "lm", se = FALSE, col = "turquoise3", linetype = "dashed")
    ) +
  labs(x = "", y = expression(paste("Million ", km^2)),
       title = "Arctic Sea Ice extent (in July)",
       subtitle = paste("In the last 40 years the Arctic Sea Ice has shrunk by almost 3 Million square kilometers.",
                        "\nThis is as much as the total area of Argentina or more than 7 times Germany."),
       caption = "Data: National Snow & Ice Data Center / {hockeystick} package") +
  theme(axis.title = element_text(size = 14, family = "Jura"),
        axis.text = element_text(size = 12, family = "Jura"),
        panel.grid.major = element_line(colour = "grey80")) +
  inset_element(argentina, left = 0, bottom = 0.1, right = 0.3, top = 0.6) +
  inset_element(germanys, left = 0.1, bottom = 0.1, right = 0.5, top = 0.5) &
  theme(plot.title = element_text(size = 20, family = "Jura"),
        plot.subtitle = element_text(size = 14, family = "Jura"),
        plot.caption = element_text(size = 10, family = "Jura"),
        panel.background = element_blank())


