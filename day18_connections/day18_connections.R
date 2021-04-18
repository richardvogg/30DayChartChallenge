library(dplyr)
library(ggplot2)
library(tidytext) # unnest the tags columns
library(reticulate) #Python connection
library(widyr) #prepare the data for the network
library(igraph) # network
library(ggraph) # network
library(ggfx) # for outer glow

use_python("C:/Users/Richard/anaconda3",required=TRUE)
source_python("day17_pop_culture/change_encoding.py")

sysfonts::font_add_google(name = "Fredoka One", "Fredoka")
showtext::showtext_auto()


options(scipen = 10)

youtube <- read.csv("C:/Richard/R and Python/Datasets/Youtube_USvideos.csv") %>%
  mutate(category = case_when(
    category_id == 1 ~ 'Film and Animation',
    category_id == 2 ~ 'Cars and Vehicles',
    category_id == 10 ~ 'Music',
    category_id == 15 ~ 'Pets and Animals',
    category_id == 17 ~ 'Sport',
    category_id == 19 ~ 'Travel and Events',
    category_id == 20 ~ 'Gaming',
    category_id == 22 ~ 'People and Blogs',
    category_id == 23 ~ 'Comedy',
    category_id == 24 ~ 'Entertainment',
    category_id == 25 ~ 'News and Politics',
    category_id == 26 ~ 'How to and Style',
    category_id == 27 ~ 'Education',
    category_id == 28 ~ 'Science and Technology',
    category_id == 29 ~ 'Non Profits and Activism',
    TRUE ~ 'Other'
  )) %>%
  top_n(400,views) %>%
  mutate(year = substr(trending_date,1,2),
         day = substr(trending_date,4,5),
         month = substr(trending_date,7,8),
         trending_date = as.Date(paste0("20",year,"-",month,"-",day)),
         publish_time = as.Date(substr(publish_time,1,10)),
         title = sapply(title, change_encoding),
         tags = sapply(tags, change_encoding)) %>%
  group_by(video_id) %>%
  summarise(across(c(trending_date:comment_count,category), last))

all_tags <- youtube %>%
  select(title,views,tags) %>%
  unnest_tokens("tags", tags,
                token = "regex",pattern = "[|]") %>%
  mutate(value = 1) %>%
  pairwise_similarity(title, tags, value, upper = FALSE)
  #pairwise_dist(title,tags,value,upper=FALSE) 



net <- all_tags %>%
  graph_from_data_frame()

test <- data.frame(name=names(V(net))) %>%
  left_join(youtube, by=c("name"="title"))

V(net)$views <- test$views
V(net)$category <- test$category

set.seed(64)

net %>%
  ggraph(layout="fr") +
  with_outer_glow(
    geom_edge_link(aes(edge_alpha = similarity)),
    sigma = 0.1
  ) +
  with_outer_glow(
    geom_node_point(aes(col=factor(category), size = views)),
    id = "nodes", col = "white"
  ) +
  with_inner_glow("nodes", sigma = 0.1)+
  geom_node_text(aes(label=name), size=3, repel=TRUE)+
  scale_edge_alpha(range=c(0.5,0.1))+
  scale_edge_width(range=c(0.1,3)) +
  scale_color_brewer(palette = "Dark2") +
  guides(size = FALSE, edge_alpha = FALSE) +
  labs(title = "Top Youtube Trending Videos in the US 2017/18",
       subtitle = paste("If two videos share a common tag on Youtube, they have a connection.",
                        "\nSize of the circle shows the number of views."),
       colour = "Category") +
  theme_void() +
  theme(panel.background = element_rect(fill = "grey80"),
        plot.background = element_rect(fill = "grey80"),
        plot.title = element_text(size = 20, family = "Fredoka"),
        plot.subtitle = element_text(size = 16),
        legend.position = c(0.1,0.15))

