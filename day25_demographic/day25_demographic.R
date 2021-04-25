
library(dplyr)
library(ggplot2)
library(stringr)
library(countrycode)
library(ggtext)

sysfonts::font_add_google(name = "Jura", "Jura")
showtext::showtext_auto()

df <- read.csv("C:/Richard/R and Python/Datasets/the_big_five_kaggle.csv", 
               sep = "\t") %>%
  select(1:10, country) %>%
  group_by(country) %>%
  slice(1:200) %>%
  mutate(id = row_number()) %>%
  add_count(country) %>%
  filter(n > 100)

questions <- read.csv("C:/Richard/R and Python/Datasets/Big_5_questions.csv",
                      sep=";") %>%
  mutate(type = case_when(
    str_detect(question_id, "EXT") ~ "extraversion",
    str_detect(question_id, "EST") ~ "neurocitism",
    str_detect(question_id, "AGR") ~ "agreeableness",
    str_detect(question_id, "CSN") ~ "conscientiousness",
    str_detect(question_id, "OPN") ~ "openness"
  ))

countrycodes <- countrycode::codelist %>%
  select(iso2c, country.name.en)

df_final <- df %>%
  tidyr::pivot_longer(cols = 1:10, names_to = "question_id") %>%
  left_join(questions, by = "question_id") %>%
  mutate(value = as.numeric(value),
         value = ifelse(value == 0, NA, value),
    value_adj = ifelse(pos_flag == "P", value, 6-value)) %>%
  tidyr::pivot_wider(id_cols = c(id,country), names_from = "question_id", 
                     values_from = "value_adj") %>%
  na.omit() %>%
  rowwise() %>%
  transmute(
    id, country, ext_score = sum(c_across(EXT1:EXT10))
  ) %>%
  add_count(country, wt = mean(ext_score)) %>%
  left_join(countrycodes, by = c("country"="iso2c"))

df_summ <- df_final %>%
  group_by(country, country.name.en) %>%
  summarise(n = n(),
            mean_ext_score = mean(ext_score),
            conf_int_80 = 1.282 * sd(ext_score)/sqrt(n),
            conf_int_95 = 1.960 * sd(ext_score)/sqrt(n),
            conf_int_99 = 2.575 * sd(ext_score)/sqrt(n))

df_summ %>% arrange(mean_ext_score) %>% View()

df_summ <- df_summ %>%
  filter(country %in% c("DE", "US", "NL", "GB", "IN", "CL", "AR", "BR",
                        "VE", "ZR", "FR", "MX", "JP", "LB"))

df_final %>%
  filter(country %in% c("DE", "US", "NL", "GB", "IN", "CL", "AR", "BR",
                        "VE", "ZR", "FR", "MX", "JP", "LB")) %>%
  ggplot(aes(y = reorder(country.name.en, n))) + 
  geom_jitter(aes(x = ext_score), height = 0.2, alpha = 0.2, col = "grey70") +
  geom_errorbarh(data = df_summ,
                 aes(xmin = mean_ext_score - conf_int_80,
                     xmax = mean_ext_score + conf_int_80), size = 1.5,
                 height = 0.2,
                 col = "blue") +
  geom_errorbarh(data = df_summ,
                 aes(xmin = mean_ext_score - conf_int_95,
                     xmax = mean_ext_score + conf_int_95), size = 1.25,
                 alpha = 0.75,
                 height = 0.2,
                 col = "blue") +
  geom_errorbarh(data = df_summ,
                 aes(xmin = mean_ext_score - conf_int_99,
                     xmax = mean_ext_score + conf_int_99), size = 1,
                 height = 0.2,
                 alpha = 0.5,
                 col = "blue") +
  geom_point(data = df_summ,
             aes(x = mean_ext_score), col = "darkorange", size = 3) +
  labs(title = "Extraversion scores from The Big 5 personality test",
       subtitle = "Showing <span style='color:darkorange;'>mean values </span> and<span style='color:blue;'> 80 / 95 / 99% confidence intervals </span>.",
       caption = "Data: Open Psychometrics / Kaggle",
       x = "Extraversion",
       y = "") +
  theme_light() +
  theme(plot.title = element_text(size = 20, family = "Jura"),
        plot.subtitle = element_markdown(size = 16, family = "Jura"),
        plot.caption = element_text(size = 12, family = "Jura"),
        axis.text = element_text(size = 14, family = "Jura"),
        axis.title = element_text(size = 16, family = "Jura"))
