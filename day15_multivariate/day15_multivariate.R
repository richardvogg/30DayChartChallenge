devtools::install_github("richardvogg/fuzzymatch")

library(dplyr)
library(ggplot2)
library(tidymodels)
library(tidytext)
library(ggfx)
library(stringr)
library(emojifont)
library(fuzzymatch)


library(extrafont)
font_import() # You just have to run this once to import all the fonts present in your system
loadfonts()

showtext::showtext_auto()

df <- read.csv("C:/Richard/R and Python/Datasets/IceCream_Kaggle.csv")

df_clean <- df %>%
  select(key, brand, name, ingredients) %>%
  mutate(ingredients = ingredients %>%
           tolower() %>%
           str_remove("\\s*\\(.*\\)")) #%>% # remove parentheses (...)
  
df_unnested <- df_clean %>%
  tidytext::unnest_tokens("ingredient",ingredients,token = "regex", 
                          pattern = ", ") %>% 
  mutate(ingredient = fuzzy_dedupes(ingredient, cutoff_distance = 0.08))


df_unique_frequent <- df_unnested %>%
  add_count(ingredient) %>%
  filter(n > 20) %>%
  select(-n) %>%
  mutate(value = 1) %>%
  distinct(key, ingredient, .keep_all = TRUE) %>%
  na.omit()

df_wide <- df_unique_frequent %>%
  tidyr::pivot_wider(names_from = ingredient, values_from = value, 
                     values_fill = 0) %>%
  janitor::clean_names() %>%
  na.omit()

pca_rec <- recipe(~., data = df_wide) %>%
  update_role(key, brand, name, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep <- prep(pca_rec)


tidied_pca <- tidy(pca_prep, 2)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = forcats::fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

pcs <- tidied_pca %>%
  filter(component %in% paste0("PC", 1:2)) %>%
  group_by(component) %>%
  top_n(10, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, value, component)) %>%
  ggplot(aes(value, terms)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    x = "Value of contribution",
    y = NULL
  ) +
  theme_minimal()

df_final <- juice(pca_prep) %>% 
  mutate(name_shown = sample(0:1,nrow(juice(pca_prep)),replace=TRUE, 
                             prob = c(10,1)))

pca_ice <- df_final %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = brand), alpha = 0.7, size = 2) +
  geom_text(data=subset(df_final, name_shown==1),
            aes(label = name), alpha = 0.5,
            check_overlap = TRUE, hjust = "inward") +
  labs(color = "brand",
       title = "Principal components of ice cream ingredients") +
  theme_minimal()

icecream <- ggplot() + 
  geom_emoji("icecream", size = 30, x =1, color = "#C77CFF") +
  geom_emoji("icecream", size = 30, x =2, color = "#F8766D") +
geom_emoji("icecream", size = 30, x =3, color = "#7CAE00") +
geom_emoji("icecream", size = 30, x =4, color = "#00BFC4") +
  expand_limits(x=c(0,5)) +
  theme_void()

library(patchwork)

layout <- "
AACC
AACC
BBCC
BBCC
"

icecream + pcs + pca_ice + plot_layout(design = layout) +
  plot_annotation(caption = "Data: Kaggle")
