### The many faces of the Beta distribution

library(gt)
library(ggplot2)
library(dplyr)

sysfonts::font_add_google(name = "Exo", "Exo")
showtext::showtext_auto()

#These are the parameters we are comparing
alpha <- c(0.5,1,2,8)
beta <- c(0.5,1,2,8)

#All combinations, but I filter some of them because their
#densities look very similar
combs <- expand.grid(alpha,beta) %>%
  rename(alpha=Var1,beta=Var2) %>%
  arrange(desc(alpha==beta),desc(alpha<beta)) %>%
  filter(!(alpha==0.5&beta>1),!(alpha>1&beta==0.5))




out <- lapply(1:nrow(combs), function(i) {
  alpha <- combs[i,1]
  beta <- combs[i,2]
  
  x <- seq(0,1,0.01)
  data.frame(x=x, y=dbeta(x,alpha,beta), alpha = alpha, beta = beta,
             id = paste(alpha,"|",beta))
}) %>%
  do.call(rbind,.)


ggplot(data = out, aes(x = x, y = y, group = id)) +
  geom_area(aes(fill=id), color = "#7C7287", size = 2) +
  geom_point(aes(x=0.4, y = 2), shape = 21, size = 5) +
  geom_point(aes(x=0.6, y = 2), shape = 21, size = 5) +
  geom_point(aes(x=0.41, y = 2), size = 3) +
  geom_point(aes(x=0.61, y = 2), size = 3) +
  expand_limits(y=3) +
  facet_wrap(~id, scales = "free") +
  labs(title = "The many faces of the Beta distribution",
       subtitle = paste("Different parameter combinations for alpha and beta")) +
  theme_void() +
  theme(panel.background = element_rect(colour = "grey"),
        legend.position = "none",
        strip.text = element_text(family = "Exo", size = 10, vjust = 0.1, 
                                  margin = margin(1,0,1,0)),
        plot.title = element_text(family = "Exo", size = 20),
        plot.subtitle = element_text(family = "Exo", size =13))

        