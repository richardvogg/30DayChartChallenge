library(waffle)
library(ggplot2)
library(dplyr)
library(ggtext)

### Check this tutorial to get your fonts to work!
#https://www.listendata.com/2019/06/create-infographics-with-r.html

sysfonts::font_add_google(name = "Sriracha", "Sriracha")
showtext::showtext_auto()

df <- data.frame(
  animal = c("dog","cat"),
  USA = c(69929000,74059000),
  China = c(27400000, 53100000),
  Russia = c(12520000, 17800000),
  Japan = c(12000000, 7300000),
  Argentina = c(9200000, 3000000),
  Germany = c(5300000, 8200000),
  `South Africa` = c(7400000, 2000000),
  France = c(7570000, 11480000),
  UK = c(9000000,8000000)
) %>%
  tidyr::pivot_longer(-animal, names_to = "country") %>%
  tidyr::pivot_wider(id_cols = country, names_from ="animal") %>%
  mutate(cat_prop = cat/(cat+dog)) %>%
  tidyr::pivot_longer(c(cat,dog), names_to = "animal") %>%
  mutate(country = forcats::fct_reorder(country,cat_prop))


ggplot(df) +
  geom_pictogram(aes(col = animal, label = animal, values = value), 
                 flip = TRUE, make_proportional = TRUE, size=4,
                 family = "FontAwesome5Free-Solid") +
  scale_label_pictogram(name = NULL, values = c("cat","dog")) +
  scale_color_manual(values = c("orange","steelblue2"))+
  facet_wrap(~ country) +
  labs(title = "Proportion of <span style='color:orange;'>cats</span> and <span style='color:steelblue2;'>dogs</span> as pets <br>in selected countries",
       caption = "Data: PetSecure") +
  theme_void() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_markdown(size = 20, family="Sriracha",margin=margin(5,0,10,0)),
        strip.text = element_text(size = 14, family = "Sriracha", margin = margin(0,0,5,0)),
        plot.caption = element_text(size = 10, family = "Sriracha"))



## Other ideas

fa_grep("pizza")

df <- data.frame(id=1:100, random = sample(c("a","b"),100, replace = TRUE))

ggplot(df) +
  geom_pictogram(aes(col = random, label = random, values = 1),
                 family =  "FontAwesome5Free-Solid") +
  scale_label_pictogram(
    name = NULL,
    values = c("house-damage", "pizza-slice")
  ) + 
  theme_void() +
  theme(legend.position="none")


fa_grep("snake")

df <- data.frame(
  animal = c("bird","cat","dog","fish"),
  germany = c(3500000, 8200000,5300000, 2000000),
  us = c(8300000,74059000,69929000,57750000),
  russia = c(2800000,17800000,12520000,770000),
  uk = c(1000000,8000000,9000000,20000000)
)