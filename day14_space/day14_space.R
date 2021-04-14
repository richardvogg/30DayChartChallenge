library(dplyr)
library(ggplot2)
library(ggfx)
library(stringr)

sysfonts::font_add_google(name = "Courgette", "Courgette")
sysfonts::font_add_google(name = "Prata", "Prata")
showtext::showtext_auto()


df <- read.csv("C:/Richard/R and Python/Datasets/the_big_five_kaggle.csv", 
               sep = "\t",nrows = 30000) %>%
  select(1:50) %>%
  mutate(id = row_number())


questions <- read.csv("C:/Richard/R and Python/Datasets/Big_5_questions.csv",
                      sep=";") %>%
  mutate(type = case_when(
    str_detect(question_id, "EXT") ~ "extraversion",
    str_detect(question_id, "EST") ~ "neurocitism",
    str_detect(question_id, "AGR") ~ "agreeableness",
    str_detect(question_id, "CSN") ~ "conscientiousness",
    str_detect(question_id, "OPN") ~ "openness"
  ))

df_final <- df %>%
  tidyr::pivot_longer(cols = 1:50, names_to = "question_id") %>%
  filter(value != 0) %>%
  left_join(questions, by = "question_id") %>%
  mutate(value_normalized = ifelse(pos_flag == "P", value, 6-value)) %>%
  group_by(id, type) %>%
  summarise(value = sum(value_normalized)) %>%
  tidyr::pivot_wider(id_cols = id, names_from = "type", values_from = "value")
  
  
df_final  %>%
  ggplot(aes(x = agreeableness, y = openness, size = conscientiousness,
             alpha = conscientiousness)) + 
  with_outer_glow(geom_jitter(data = subset(df_final, extraversion > 20 & neurocitism <=30),
                              col = "white",
                              height = 0.5, width = 0.5),
                  colour = "blue", sigma = 1) +
  with_outer_glow(geom_jitter(data = subset(df_final, extraversion > 20 & neurocitism > 30),
                              col = "white",
                              height = 0.5, width = 0.5),
                  colour = "white", sigma = 1) +
  geom_jitter(data = subset(df_final, extraversion <= 20), 
              col = "white",
              height = 0.5, width = 0.5) +
  scale_size_continuous(range = c(0.1, 1), trans = "exp") +
  scale_alpha_continuous(trans = "exp", range = c(0.3,1)) +
  guides(size = guide_legend(override.aes = list(alpha = 1, col = "black") ) ) +
  coord_cartesian(xlim = c(20,45), ylim = c(20, 45)) +
  labs(title = "The Big 5 personality test universe",
       subtitle = "Responses from 30000 people - \nwhite and blue glow is added based on extraversion and neurocitism values",
       caption = "Data: Kaggle") +
  theme(panel.background = element_rect(fill = "black"),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Courgette", size = 20, 
                                  vjust = -70, hjust = 0.1, colour = "white"),
        plot.subtitle = element_text(family = "Prata", size = 13),
        plot.caption = element_text(family = "Prata", size = 8),
        legend.title = element_text(family = "Prata", size = 12),
        legend.text = element_text(family = "Prata", size = 12),
        axis.text = element_text(family = "Prata", size = 12),
        axis.title = element_text(family = "Prata", size = 12))
