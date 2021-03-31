library(ggplot2)
library(dplyr)
library(tidyr)

#Indicator 10.6.1: Proportion of members and voting rights 
# of developing countries in international organizations

df <- read.csv("C:/Richard/R and Python/Datasets/UN_Proportion_of_voting_rights.csv")

df <- df %>% 
  select(nameOfInternationalInstitution1,
         geoAreaName, ISO3,
         value_2010,value_2015,value_2019)

df_long <- df %>%
  group_by(nameOfInternationalInstitution1) %>%
  summarise(n=n(),
    developing_2010=sum(value_2010,na.rm=TRUE),
    developed_2010 = 100 - developing_2010,
    developing_2019 = sum(value_2019, na.rm = TRUE),
    developed_2019 = 100 - developing_2019
    ) %>%
  filter(n>100) %>%
  tidyr::pivot_longer(cols=3:6) %>%
  tidyr::separate(col = name, into = c("type","year"))
  


ggplot(data=df_long,aes(x = "", y = value, fill = type )) + 
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(label = round(value)), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_grid(year ~ stringr::str_wrap(nameOfInternationalInstitution1,15))  +
  scale_fill_manual(values=c("grey70","goldenrod2"))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(family="sans",size=12,colour="midnightblue")) + 
  labs(caption="Percentage")+
  theme(legend.position="none")  
