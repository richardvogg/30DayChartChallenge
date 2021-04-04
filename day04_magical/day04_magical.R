devtools::install_github("bradleyboehmke/harrypotter")


library(ggstream)
library(harrypotter)
library(stringr)
library(dplyr)
library(ggplot2)
library(viridis)


sysfonts::font_add_google(name = "Indie Flower", "Indie")
showtext::showtext_auto()

hp_texts <- list(
  harrypotter::philosophers_stone,
  chamber_of_secrets,
  prisoner_of_azkaban,
  goblet_of_fire,
  order_of_the_phoenix,
  half_blood_prince,
  deathly_hallows
)

str_count(prisoner_of_azkaban,"Expecto Patronum",)

df <- sapply(hp_texts, function(x) {
  expl <- sum(str_count(x, "[e|E]xpelliarmus"))
  lum <- sum(str_count(x, "[L|l]umos"))
  wil <- sum(str_count(x, "[w|W]ingardium [L|l]eviosa"))
  acc <- sum(str_count(x, "[A|a]ccio"))
  expt <- sum(str_count(x, "[E|e]xpecto [P|p]atronum"))
  
  c(expl,lum,wil, acc, expt)
}) %>% 
  as.data.frame() %>%
  mutate(spell = c("Expelliarmus", "Lumos", "Wingardium Leviosa",
                   "Accio", "Expecto Patronum"))

df_long <- df %>%
  tidyr::pivot_longer(cols = 1:7, names_to = "book", 
                      values_to = "occurrences") %>%
  mutate(book = substr(book,2,2) %>% as.numeric())

library(ggfx)

df_long %>%
  ggplot(aes(x = book, y = occurrences, fill = spell)) +
  with_outer_glow(geom_stream(),
    sigma=10, colour = "white") +
  scale_fill_manual(values = c("yellow", "orange", "red", "violet", "darkviolet")) +
  labs(title = "Occurrences of spells in Harry Potter books") +
  theme_void() +
  theme(legend.position = c(0.2,0.25),
        legend.text = element_text(colour = "white", size = 20,
                                   family = "Indie"),
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(size = 30, colour = "white", 
                              family = "Indie",
                              vjust = -7, hjust = 0.2))
