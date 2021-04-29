library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggfx)
library(hms) # for hour-minute-second variable type
library(patchwork)

sysfonts::font_add_google(name = "Comfortaa", "Comfortaa")
showtext::showtext_auto()

sheet_names <- excel_sheets("day29_deviations/CO2-Messungen.xlsx")

df <- lapply(2:10, function(x) {
  read_xlsx("day29_deviations/CO2-Messungen.xlsx", skip = 3, sheet = x,
            col_names = c("hour","ppm")) %>%
    mutate(hour_time = hms::as_hms(hour),
           hour = as.numeric(format(hour, "%H")),
           date = sheet_names[x]) %>%
    separate(date, into = c("day","month","year")) %>%
    mutate(day = as.numeric(day)) %>%
    mutate(date = as.Date(paste0("20",year,"-",month,"-",day))) %>%
    mutate(date_range = case_when(
      month == 6 ~ "June 3-7, 2010",
      month == 8 & day < 10 ~ "August 5/6, 2010",
      month == 8 & day > 10 ~ "August 20/21, 2010",
      TRUE ~ "Error"
    ))
}) %>%
  do.call(rbind, .)

get_in_hm <- function(time1, time2) {
  format(as.POSIXct(as.numeric(difftime(time1, time2, units = 'secs')), 
                    origin = '1970-01-01', tz = 'UTC'), '%H:%M')  
}

  
suntime <- data.frame(
  month  = c("August","June"),
  sunrise = hms::as_hms(c("06:18:00","05:12:00")),
  sunset = hms::as_hms(c("20:26:00","21:23:00"))
) %>%
  mutate(sunlight_hours = get_in_hm(sunset, sunrise))


points <- df %>%
  filter(month == 6 | day > 10) %>%
  ggplot(aes(x = hour_time, y = ppm, col = date_range)) + 
  geom_point() +
  with_outer_glow(
    geom_smooth(aes(fill = date_range),
                method = "lm", formula = y ~ poly(x,4), alpha = 0.3)
  ) +
  scale_fill_manual(values = c("#ff8f00","#0070ff")) +
  scale_colour_manual(values = c("#ff8f00","#0070ff")) +
  scale_x_time(labels = scales::time_format(format = "%H:%M")) +
  expand_limits(x = c(as_hms("00:00:00"), as_hms("24:00:00"))) +
  labs(title = bquote("Sunlight hours influence variation in" ~CO[2]~ "concentration"),
       subtitle = "Measurements in Veitshöchheim, Germany",
       y = bquote(~CO2[2]~ "in ppm")) +
  theme_light() +
  theme(legend.position = c(0.7, 0.8),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))



sunriseplot <- function(data, month_name, col_name) {
  data %>%
    filter(month == month_name) %>%
    ggplot(aes(x = sunrise, y = 1)) +
    geom_rect(aes(xmin = as_hms("00:00:00"), xmax = sunrise, ymin = 0, ymax = 2),
              fill = "black")+
    geom_rect(aes(xmin = sunset, xmax = as_hms("24:00:00"), ymin = 0, ymax = 2),
              fill = "black") +
    geom_rect(aes(xmin = sunrise, xmax = sunset, ymin = 0, ymax = 2),
              fill = "#fff777") +
    geom_vline(aes(xintercept = sunrise), size = 2, col = col_name) +
    geom_vline(aes(xintercept = sunset), size = 2, col = col_name) + 
    coord_cartesian(ylim = c(0.5, 1.5)) +
    expand_limits(x = c(as_hms("00:00:00"), as_hms("24:00:00"))) +
    annotate("text", x = as_hms("01:00:00"), y = 1, 
             label = month_name, col = "white", 
             hjust = 0, family = "Comfortaa", size = 5) +
    annotate("text", x = data[data$month==month_name,2] + 300, y = 1, 
             label = substr(data[data$month==month_name,2], 1, 5),
             hjust = 0, family = "Comfortaa", size = 3) +
    annotate("text", x = data[data$month==month_name,3] -300 , y = 1, 
             label = substr(data[data$month==month_name,3], 1, 5),
             hjust = 1, family = "Comfortaa", size = 3) +
    annotate("text", x = as_hms("14:00:00"), y = 1,
             label = paste(data[data$month==month_name,4], "hours of sunlight"),
             family = "Comfortaa", size = 5) +
    theme_void()
}


sunlight_june <- suntime %>%
  sunriseplot("June","#0070ff")

sunlight_august <- suntime %>%
  sunriseplot("August","#ff8f00")
  
  
points + sunlight_august + sunlight_june + 
  plot_layout(ncol = 1, heights = c(5, 1, 1)) +
  plot_annotation(caption = "Data and Visualization: Richard Vogg") &
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 18),
        text = element_text(family = "Comfortaa"))

