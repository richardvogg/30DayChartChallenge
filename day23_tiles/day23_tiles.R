library(raster)
library(dplyr)
library(ggplot2)
library(tmap)
library(sf)
library(patchwork)
library(ggtext)

#data from ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip

sysfonts::font_add_google(name = "Jura", "Jura")
showtext::showtext_auto()

#Long term mean
ltm <- brick("C:/Richard/R and Python/Environmental Data Science/GIS/ChileClimate/Data_raw/precip.day.1981-2010.ltm.nc") %>%
  rotate()

#Selected countries
data("World")
chile <- World %>%
  filter(name %in% c("Chile")) %>%
  st_transform(crs(ltm)@projargs)

center <- chile %>%
  st_crop(xmin = -74, xmax = -69, ymin = -38.5, ymax = -32)


#Shows the region of interest in a map
map <- ggplot(data = chile) +
  #with_outer_glow() +
  with_inner_glow(geom_sf()) +
  with_inner_glow(geom_sf(data = center, fill = "darkred")) +
  theme_void()


summarise_precip <- function(df) {
  out <- df %>% 
    mask(center) %>% 
    rasterToPoints() %>% 
    data.frame() %>% 
    tidyr::pivot_longer(cols=-c(x,y),names_to="date",values_to="value") %>% 
    mutate(month=substr(date,7,8) %>% as.numeric()) %>%
    group_by(x,y,month) %>% 
    summarise(precip=sum(value)) %>% 
    ungroup()
  
  return(out)
}

crs(ltm) <- crs(center)

ltm_df <- summarise_precip(ltm)
rm(ltm)

#this runs for a few minutes (~ 20 minutes for me)
for(year in 2011:2019) {
  dat <- brick(paste0("C:/Richard/R and Python/Environmental Data Science/GIS/ChileClimate/Data_raw/precip.",year,".nc")) %>%
    rotate()
  
  crs(dat) <- crs(countries)
  
  assign(paste0("df", year), summarise_precip(dat) %>% mutate(year = year))
}

avg_df <- rbind(df2011, df2012, df2013, df2014, df2015, 
                df2016, df2017, df2018, df2019) %>%
  group_by(year, month) %>%
  summarise(prec=mean(precip))

avg_ltm_df <- ltm_df %>%
  group_by(month) %>%
  summarise(ltm = mean(precip))

final <- avg_df %>%
  inner_join(avg_ltm_df,by=c("month")) %>%
  mutate(abs_change = prec - ltm,
         perc_change = (prec - ltm)/ltm) %>%
  mutate(month_name = factor(month.abb[month], levels = month.abb))



tiles <- final %>%
  ggplot(aes(x = year, y = month_name, 
             fill = abs_change)) +
  geom_tile(col = "white", size = 2) +
  scale_fill_gradient2(low = "darkred", high = "blue") +
  #scale_alpha_continuous(range = c(0.5, 1)) +
  scale_y_discrete(position = "right") +
  labs(fill = "Change \n[mm]") +
  theme_light() +
  theme(legend.position = "top",
        legend.title = element_text(family = "Jura"),
        legend.text = element_text(family = "Jura"),
        axis.title = element_blank(),
        axis.text = element_text(family = "Jura", size = 12))

months <- final %>%
  group_by(month_name) %>%
  summarise(prec = mean(prec), ltm = max(ltm)) %>% 
  tidyr::pivot_longer(cols = -month_name, names_to = "which", values_to = "prec") %>%
  ggplot(aes(x = month_name, y = prec, fill = which)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("darkblue", "turquoise3")) +
  coord_flip() +
  labs(y = "Average \n[mm]") +
  theme_void() +
  theme(axis.text.x = element_text(family = "Jura"),
        axis.title.x = element_text(family = "Jura"),
        axis.ticks.y = element_line(),
        legend.position = "none")

years <- final %>%
  group_by(year) %>%
  summarise(prec = sum(prec), ltm = sum(ltm)) %>%
  tidyr::pivot_longer(cols = -year, names_to = "which", values_to = "prec") %>%
  ggplot(aes(x = year, y = prec, fill = which)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("darkblue", "turquoise3")) +
  scale_y_reverse() +
  labs(y = "Total \n[mm]") +
  theme_void() +
  theme(axis.text.y = element_text(family = "Jura"),
        axis.title.y = element_text(family = "Jura"),
        legend.position = "none")

layout <- "
ACCCD
BCCCD
BCCCD
BEEEF
"


guide_area() + map + tiles + months + years + plot_spacer() + 
         plot_layout(design = layout, guides = "collect") +
  plot_annotation(
    title = "Dry years in central Chile",
    subtitle = paste0("Less precipitation in <b><span style='color:turquoise3;'>recent years</span></b>",
    " in comparison <br> with the <b><span style='color:darkblue;'>long-term mean</span></b>."),
    caption = "CPC Global Unified Precipitation data \nprovided by the NOAA/OAR/ESRL PSL, \nBoulder, Colorado, USA, from their website \nat https://psl.noaa.gov/data/gridded/\ndata.cpc.globalprecip.html"
  ) &
  theme(plot.title = element_text(family = "Jura", size = 24, face = "bold"),
        plot.subtitle = element_markdown(family = "Jura", size = 18),
        plot.caption = element_text(family = "Jura",vjust = 30))

