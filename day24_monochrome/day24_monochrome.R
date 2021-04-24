library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)


sysfonts::font_add_google(name = "Jura", "Jura")
showtext::showtext_auto()

viveeko <- read_excel("day24_monochrome/ViveEko.xlsx",sheet=1)

for(i in 1:nrow(viveeko)) {
  if(is.na(viveeko$Lugar[i])) {
    viveeko$Lugar[i] <- viveeko$Lugar[i-1]
    viveeko$Fecha[i] <- viveeko$Fecha[i-1]
  }
}

viveeko_final <- viveeko %>% group_by(Lugar, Fecha) %>% 
  summarise(env_plastic = max(Envases),
            env_metal = min(Envases),
            ekopesos = max(Ekopesos, na.rm=T)) %>%
  mutate(env_plastic = ifelse(env_plastic == env_metal & 
                                grepl("METAL",env_plastic), 0, env_plastic),
         env_metal = ifelse(env_plastic == env_metal & 
                              grepl("PLASTIC", env_metal), 0, env_metal)) %>%
  mutate(env_plastic = gsub("PLASTIC|[:]|[[:blank:]]", "", env_plastic),
         env_metal = gsub("METAL|[:]|[[:blank:]]", "", env_metal)) %>%
  mutate(env_plastic = as.numeric(env_plastic),
         env_metal = as.numeric(env_metal)) %>%
  mutate(date = as.Date(Fecha),
         hour = as.numeric(format(Fecha, "%H"))) %>%
  group_by(date) %>%
  summarise(env_metal = sum(env_metal),
            env_plastic = sum(env_plastic)) %>%
  tidyr::pivot_longer(cols = starts_with("env"), names_to = "type") %>%
  mutate(value = ifelse(value>50, 50, value))


plastic <- viveeko_final %>%
  filter(type == "env_plastic") %>%
  ggplot(aes(x = date, y = value, group = type)) + 
  annotate("text", x = as.Date("2019-03-01"), y = 40, label = "Plastic bottles",
           size = 16, col = "#182607", family = "Jura") +
  geom_point(col = "#91CE46") +
  geom_segment(aes(xend = date, yend = 0), col = "#91CE46") +
  scale_y_continuous(breaks = c(0,25,50)) +
  theme(axis.text = element_text(colour = "#91CE46", size = 14, family = "Jura"))


metal <- viveeko_final %>%
  filter(type == "env_metal") %>%
  ggplot(aes(x = date, y = value, group = type)) + 
  annotate("text", x = as.Date("2019-03-01"), y = 40, label = "Metal cans",
           size = 16, col = "#182607", family = "Jura") +
  geom_point(col = "#91CE46") +
  geom_segment(aes(xend = date, yend = 0), col = "#91CE46") +
  expand_limits(y = 50) +
  scale_y_reverse(breaks = c(0,25,50)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "#91CE46", size = 14, family = "Jura"))



plastic + metal + plot_layout(nrow = 2) +
  plot_annotation(title = "Recycling efforts",
                  subtitle = paste("Number of metal cans and plastic bottles recycled by my wife",
                                    "\nand me in the recycling stations in Valparaíso / Viña del Mar.")) &
  theme(plot.background = element_rect(fill = "#26380F", colour = NA),
        panel.background = element_rect(fill = "#26380F"),
        plot.title = element_text(colour = "#91CE46", size = 28, family = "Jura"),
        plot.subtitle = element_text(colour = "#91CE46", size = 20, family = "Jura"),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())
