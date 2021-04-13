library(dplyr)
library(ggplot2)
library(stringr)


df <- read.csv("C:/Richard/R and Python/Datasets/the_big_five_kaggle.csv", 
               sep = "\t",nrows = 3000) %>%
  select(1:50)

questions <- read.csv("C:/Richard/R and Python/Datasets/Big_5_questions.csv",
                      sep=";") %>%
  mutate(type = case_when(
    str_detect(question_id, "EXT") ~ "extraversion",
    str_detect(question_id, "EST") ~ "neurocitism",
    str_detect(question_id, "AGR") ~ "agreeableness",
    str_detect(question_id, "CSN") ~ "conscientiousness",
    str_detect(question_id, "OPN") ~ "openness"
  ))

df_long <- df %>%
  tidyr::pivot_longer(cols = everything(), names_to = "question_id") %>%
  left_join(questions, by = "question_id")

df_mean <- df_long %>%
  group_by(question,type) %>%
  summarise(value = mean(value))


df_long %>%
  filter(value != 0) %>%
  ggplot(aes(x = str_wrap(question,35), y = value, col = type)) + 
  geom_jitter(alpha = 0.01, width = 0.2, height = 0.5) +
  geom_point(data = df_mean, col = "black") +
  scale_color_brewer(palette = "Dark2") +
  scale_y_discrete(labels = c("Disagree", "", "Neutral", "", "Agree")) +
  coord_flip() +
  facet_wrap(~ type, scales = "free_y", ncol = 2) +
  labs(caption = "Responses of 3000 participants \nto the Big 5 personality test \n(Data: Kaggle)") +
  theme(axis.title = element_blank(),
        axis.text = element_text(lineheight = 0.7),
        legend.position = "none",
        plot.caption = element_text(size = 18, vjust = 15, family = "Sans"))


