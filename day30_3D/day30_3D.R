library(rayshader)
library(dplyr)
library(ggplot2)
library(forcats)

#Center for Climate and Resilience Research at cr2.cl

sysfonts::font_add_google(name = "Montserrat", "Montserrat")
showtext::showtext_auto()

df_temp <- read.csv("C:/Richard/R and Python/Datasets/chile-avg-temp.csv")
df_prec <- read.csv("C:/Richard/R and Python/Datasets/chile-precipitation.csv")
df_max_temp <- read.csv("C:/Richard/R and Python/Datasets/chile-max-temp.csv")
df_fires <- read.csv("C:/Richard/R and Python/Datasets/chile-fires.csv") %>%
  filter(region == "RM") %>%
  transmute(nombre = "Pudahuel Santiago", month = month_num, year, 
            forest_fire_cnt)

temps <- df_temp %>% 
  inner_join(df_prec, by = c("nombre", "year", "month", "altura", 
                             "latitud", "longitud")) %>%
  inner_join(df_max_temp, by = c("nombre", "year", "month", "altura", 
                                 "latitud", "longitud"), 
             suffix = c("_avg","_max")) %>%
  inner_join(df_fires, by = c("nombre", "year", "month")) %>% 
  filter(nombre == "Pudahuel Santiago") %>%
  ggplot(aes(x = temp_max, y = prec, col = forest_fire_cnt)) +
  geom_point() +
  facet_wrap( ~ fct_reorder(month.abb[month], month)) +
  scale_color_gradientn(colours = c("grey50", "tomato", "darkred"),
                        values = c(0,0.1,1)) +
  labs(title = "Forest fires, temperatures and precipitation in Chile's Metropolitan region",
       subtitle = "Measuring station Pudahuel Santiago",
       caption = "Data: Center for Climate and Resilience Research at cr2.cl / CONAF",
       x= "Max Temperature", y = "Precipitation", col = "Forest fires") +
  expand_limits(y = -50) +
  theme_light() +
  theme(strip.background = element_rect(fill = "grey20"),
        text = element_text(family = "Montserrat", size = 18),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 14),
        legend.title = element_text(margin = margin(b = 50), size = 16))
    

plot_gg(temps, width = 5, height = 3, scale = 300, 
        multicore = TRUE, windowsize = c(1000, 800))
render_camera(fov = 70, zoom = 0.55, theta = 30, phi = 60)

