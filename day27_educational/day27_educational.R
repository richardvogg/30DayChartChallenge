library(dplyr)
library(ggplot2)
library(cowplot)

sysfonts::font_add_google(name = "Jura", "Jura")
showtext::showtext_auto()


test <- lapply(1:4, function(wdh) {
  lapply(c(1, 2, 5, 10), function(y) {
    lapply(c(4,8,16,32,64), function(x) {
      tibble(wdh = wdh, sd = y, n = x, val = rnorm(x, mean = 0, sd = y))
    }) %>% do.call(rbind, .)
  }) %>% do.call(rbind, .)
}) %>% do.call(rbind, .)



summ_df <- test %>%
  group_by(sd, n, wdh) %>%
  summarise(n = n(),
            avg_val = mean(val),
    conf_int_lower = avg_val - 1.96 * sd(val)/sqrt(n),
            conf_int_upper = avg_val + 1.96 * sd(val)/sqrt(n))

plot <- ggplot(test) + 
  geom_jitter(aes(x = val, y = wdh), height = 0.02, col = "blue", alpha = 0.4) +
  geom_point(data = summ_df, aes(x = avg_val, y = wdh-0.3), col = "darkorange") +
  geom_errorbarh(data = summ_df, 
                 aes(xmin = conf_int_lower, xmax = conf_int_upper, y = wdh-0.3),
                 height = 0.3) +
  geom_vline(xintercept = 0, col = "blue", size = 0.5) +
  facet_grid(sd ~ n) +
  expand_limits(y = c(0.5, 4.5)) +
  labs(y = "Standard deviation",
       x = "Sample size") +
  theme(axis.text = element_blank(),
        axis.title.x.bottom = element_blank(),
        axis.title.y.left = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = NA, colour = "sandybrown"),
        strip.text = element_text(size = 16),
        strip.background = element_rect(fill = "sandybrown"),
        text = element_text(family = "Jura"))



ggdraw(xlim = c(0, 1.2), ylim = c(-0.2,1.2)) +
  draw_plot(plot,x = 0, y = -0.2) +
  draw_line(x = c(0.1,0.8), y = c(0.9,0.9), arrow = arrow(), lineend = "butt",
            size = 2, col = "sandybrown") +
  draw_line(x = c(1.1,1.1), y = c(0.6,-0.1), arrow = arrow(), lineend = "butt",
            size = 2, col = "sandybrown") +
  draw_text(text = "Sample size", size = 16, x = 0.45, y = 0.85, family = "Jura") +
  draw_text(text = "Standard deviation", size = 16, x = 1.05, y = 0.25, 
            angle = -90, family = "Jura") +
  draw_text(text = "Estimating the mean of a sample", size = 24, 
            x = 0.01, y = 1.15, hjust = 0, family = "Jura") +
  draw_text(text = "Vertical blue line shows true mean, orange point is the estimate, \nconfidence intervals depend on sample size and standard deviation", 
            size = 15, x = 0.01, y = 1.07, hjust = 0, family = "Jura", vjust = 1)
