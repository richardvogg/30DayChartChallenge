library(dplyr)
library(ggplot2)
library(tidytext)
library(reticulate) #Python connection
library(forcats)

use_python("C:/Users/Richard/anaconda3",required=TRUE)
source_python("day17_pop_culture/change_encoding.py")

options(scipen = 10)

youtube <- read.csv("C:/Richard/R and Python/Datasets/Youtube_USvideos.csv") %>%
  mutate(year = substr(trending_date,1,2),
         day = substr(trending_date,4,5),
         month = substr(trending_date,7,8),
         trending_date = as.Date(paste0("20",year,"-",month,"-",day)),
         publish_time = as.Date(substr(publish_time,1,10)),
         title = sapply(title, change_encoding)) %>%
  group_by(video_id) %>%
  summarise(across(trending_date:comment_count, last)) %>%
  mutate(like_percentage = likes / views,
         dislike_percentage = dislikes / views,
         comment_percentage = comment_count / views)


make_top_10 <- function(vari, title_name) {
  youtube %>%
    top_n(10, {{vari}}) %>%
    arrange(desc({{vari}})) %>%
    mutate(rank = rank({{vari}})) %>%
    ggplot(aes(x = {{vari}},
               y = fct_reorder(title,{{vari}}))) + 
    geom_col(width = 0.3) + 
    geom_text(aes(x = 0, y = rank,  
                  label = stringr::str_wrap(title,40)),
              color = "black", hjust = 0, 
              position = position_nudge(y = 0.4),
              fontface = "bold", size = 4, lineheight = 0.6) +
    geom_text(aes(x = {{vari}}, y = rank, 
                  label = scales::comma({{vari}})),
                  #label = scales::percent({{vari}}, accuracy = 0.1)),
              color = "goldenrod", hjust = 1.1,
              fontface = "bold", size = 4) +
    labs(title = title_name) +
    theme_void() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          text = element_text(lineheight = 0.5))
}

views <- make_top_10(views, "Most viewed")

likes <- make_top_10(like_percentage, "Most likes per view")

#dislikes <- make_top_10(dislike_percentage, "Most disliked")

comments <- make_top_10(comment_percentage, "Most comments per view")


library(patchwork)

views + likes + comments + plot_layout(nrow = 1) +
  plot_annotation(title = "Trending Youtube Videos in 2017/18",
                  caption = "Data: Kaggle") &
  theme(plot.title = element_text(size = 15))


