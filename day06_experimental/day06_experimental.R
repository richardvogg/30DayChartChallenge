library(ggforce) # for geom_voronoi
library(viridis) # the color palette "plasma"
library(ggfx) # add glow


x <- seq(-1,1,by=0.01)
y <- sqrt(1 - x^2)
x <- c(x,-x)
y <- c(y,-y)

circle <- cbind(x, y)

test <- data.frame(first = rnorm(300, sd = 0.3), 
                   second = rnorm(300, sd = 0.3),
                   filler = -rexp(300, rate = 2), 
                   grouper = rep(1:3, 100))


ggplot(test, aes(first, second, group = 1)) +
  with_outer_glow(
    geom_voronoi_tile(aes(fill = filler), colour = NA, bound = circle), 
    sigma = 5,
    id="voronoi", col = "white"
  ) +
  with_inner_glow(
    "voronoi", color = "yellow", sigma = 5
  ) +
  facet_wrap(~ grouper) +
  scale_fill_viridis(option = "plasma")+
  coord_equal() +
  theme_void() +
  theme(legend.position = "none",
        strip.text = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"))
