library(waffle)
library(ggplot2)
library(dplyr)

### Check this tutorial to get your fonts to work!
#https://www.listendata.com/2019/06/create-infographics-with-r.html

showtext::showtext_auto()

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


fa_grep("")

df <- data.frame(
  animal = c("bird","cat","dog","horse","fish","reptile","small animal"),
  number = c(5.7, 42.7, 63.4, 1.6, 13.1, 4.5, 5.4)
)

ggplot(subset(df,animal %in% c("bird","cat","dog","horse","fish"))) +
  geom_pictogram(aes(col=animal,label=animal,values=number),n_rows = 5, flip=TRUE)+
  facet_grid(.~animal)+
  scale_label_pictogram(
    name=NULL,
    values = c("kiwi-bird","cat","dog","horse","fish")
  ) +
  scale_color_manual(values = c("red","orange","black","brown","blue"))+
  theme_void()
