library(emojifont)
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggtext)

showtext::showtext_auto()

emoji(search_emoji("person"))

x <- 1:9
y <- runif(9)
d <- data.frame(x,y,label=emoji(search_emoji("person")))

ggplot(d, aes(x, y, label=label)) +
  geom_text(family="EmojiOne", size=12)


y <- rnorm(100,50,8) %>%
  {ifelse(runif(100) > 0.7, rnorm(100, 30, 5), .)} %>%
  {ifelse(runif(100) > 0.9, rexp(100, 0.05), .)} %>%
  round()

d <- data.frame(id = 1:100,
                y=y,
                label=fontawesome("fa-user")) %>%
  mutate(group = runif(100,-y,y*y) %>%
           cut(4, labels = 1:4))

arrowplot <- ggplot() + 
  geom_segment(aes(x = 1, y = 1, xend = 2, yend = 1),
               lineend = "butt", linejoin = "round", size = 2,
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_text(aes(x = 1.5, y = 1.1, label = "Abstraction"), size = 8) +
  expand_limits(y=c(0.8,1.4)) +
  theme_void()

set.seed(64)

people <- ggplot(d, aes(runif(100), runif(100), label=label,
                    col = group)) +
  geom_text(family='fontawesome-webfont', size=6)+
  annotate("text", x = 0.45, y = 0.3, label = "I am 54\n years old") +
  annotate("segment", x = 0.5, y = 0.35, xend = 0.6, yend = 0.45) +
  xlab(NULL)+ylab(NULL) +
  theme_void() +
  theme(legend.text=element_text(family='fontawesome-webfont'),
        legend.position = "none")

table <- ggplot(d[1:10,], aes(y = -id)) +
  geom_text(data = d[1,], aes(x=0, label = "id", y = 0)) +
  geom_text(data = d[1,], aes(x=1, y= 0, label = "age")) +
  geom_text(data = d[1,], aes(x=2, y= 0, label = "group")) +
  geom_text(data = d[1,], aes(x=0, label = "...", y = -11)) +
  geom_text(data = d[1,], aes(x=1, label = "...", y = -11)) +
  geom_text(data = d[1,], aes(x=2, label = "...", y = -11)) +
  geom_text(aes(x=0, label = id)) +
  geom_text(aes(x=1, label = round(y))) +
  geom_text(aes(x=2, label = group)) +
  geom_rect(aes(xmin = -0.4, xmax = 2.4, ymin = -12, ymax = 1), fill = NA,
            col = "black") +
  expand_limits(x=c(-0.5,2.5)) +
  theme_void()

dens <- ggplot(d, aes(x = y, col = group)) + 
  geom_density(size = 2) +
  labs(x = "age", y = "density") +
  theme_void() +
  theme(legend.position = "none",
    axis.line = element_line(),
        axis.title = element_text(),
        axis.text.x = element_text())

histo <- ggplot(d, aes(x = y)) + 
  geom_histogram() +
  labs(x = "age", y = "frequency") +
  theme_void() +
  theme(axis.line = element_line(),
        axis.title = element_text(),
        axis.text.x = element_text())

boxpl <- ggplot(d, aes(x = y, y = group, col = group)) + 
  geom_boxplot(size = 1) +
  labs(x = "age", y = "") +
  theme_void() +
  theme(legend.position = "none",
    axis.line = element_line(),
        axis.title = element_text(),
        axis.text.x = element_text())

avg_diff <- d %>%
  group_by(group) %>%
  summarise(avg_age = mean(y)) %>%
  summarise(out = max(avg_age)-min(avg_age)) %>% .$out



output <- ggplot(data=data.frame(),aes(x=1,y=1)) +
  geom_textbox(aes(label = paste("<span style='color:#C77CFF; font-size:16pt'>Group 4</span>",
                     "<span style='font-size:16pt'>is on average<span style='font-size:20pt'>", round(avg_diff,1), 
                     "<span style='font-size:16pt'>years older",
                     " than <span style='color:#F8766D;'>Group 1</span>"))) +
  theme_void()






test <- (people + table + 
           (dens + histo + plot_layout(nrow = 2)) + boxpl + output) + 
  plot_layout(nrow = 1)

arrowplot + test + plot_layout(nrow = 2, heights = c(0.3,0.7))
