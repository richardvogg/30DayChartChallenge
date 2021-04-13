remotes::install_github("r-link/corrmorant")

library(dplyr)
library(ggplot2)
library(corrmorant)
library(ggfx)

sysfonts::font_add_google(name = "Prata", "Prata")
showtext::showtext_auto()


df <- read.csv("C:/Richard/R and Python/Datasets/the_big_five_kaggle.csv", 
               sep = "\t",nrows = 10000) %>%
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
  mutate(value = as.numeric(value),
         question = stringr::str_wrap(question,19)) %>%
  tidyr::pivot_wider(id_cols = id, names_from = "question", 
                     values_from = "value")


ques <- c(2,4,12,14,25)

ggcorrm(df_final[,ques], aes(col = .corr, fill = .corr)) +
  lotri(geom_jitter(alpha = 0.03, width = 0.5, height = 0.5)) +
  with_outer_glow(lotri(geom_smooth(method = "lm", col = "goldenrod2")), 
                  sigma=1) +
  utri_heatmap(alpha = 0.5) +
  utri_corrtext(size = 5) +
  dia_histogram(lower = 0.3, bins = 5, fill = "grey", alpha = 0.8) +
  dia_names(y_pos = 0.3, size = 3) +
  scale_fill_viridis_c(option = "plasma") +
  scale_color_viridis_c(option = "plasma") +
  labs(title = "Correlation between selected Big 5 \npersonality test questions",
       caption = "Data: Kaggle") +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, family = "Prata"),
        plot.caption = element_text(family = "Prata"))
