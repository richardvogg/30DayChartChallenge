library(dplyr)
library(readxl)
library(ggplot2)
library(ggradar)
library(patchwork)

sysfonts::font_add_google(name = "Prata", "Prata")
showtext::showtext_auto()


df <- lapply(2006:2020,function(x) {
  read_xlsx(paste0("day03_historical/data/fsi-",x,".xlsx")) %>%
    select(1:16)
}) %>% 
  do.call(rbind,.) %>%
  mutate(Year=format(Year,"%Y"))

# calculate global mean values
df_long <- df %>%
  filter(Year %in% c(2010,2020)) %>%
  group_by(Year) %>%
  summarise(across(4:15,.fns = function(x) mean(x)/10))

#clean names
colnames(df_long)[-1] <- colnames(df_long)[-1] %>%
  substr(5,100) %>%
  stringr::str_wrap(12)

#global mean radar
average <- df_long %>% ggradar(font.radar = "Prata",
                               base.size=8,
                               grid.label.size=5,
                               axis.label.size=3,
                    values.radar = c("1","", "10"),
                    legend.position = "bottom")

# Top / Flop 3

top_flop_3 <- df %>%
  filter(Year %in% c(2010,2020)) %>%
  select(Country,Year,Total) %>%
  tidyr::pivot_wider(id_cols = Country, names_from=Year, values_from = Total) %>%
  mutate(change = `2020`-`2010`) %>%
  filter(!is.na(change)) %>%
  arrange(change) %>%
  slice(1:3,(n()-2):n()) %>%
  .$Country

df_long <- df %>%
  filter(Year %in% c(2010,2020), Country %in% top_flop_3) %>%
  mutate(across(4:16,.fns = function(x) x/10)) %>%
  select(-Rank,-Total)

colnames(df_long)[-c(1,2)] <- colnames(df_long)[-c(1,2)] %>%
  substr(5,100) %>%
  stringr::str_wrap(15)



out <- lapply(top_flop_3, function(x) {
  df_long %>% 
  filter(Country==x) %>%
  select(-Country) %>% 
  ggradar(font.radar = "Prata",
          axis.labels = rep("",12),
          values.radar = c("","", ""),
          legend.position = "none",
          group.point.size = 3,
          group.line.width = 1,
          plot.title = x)
})


## Assemble the plot

(average | (out[[1]]+out[[2]]+out[[3]] + 
   out[[4]] + out[[5]] + out[[6]])) +
  plot_annotation(title = "The Fragile State Index",
                  subtitle = paste("The index measures vulnerability of countries based on 12 indicators which take values between 1 and 10",
                                   "where 1 is high stability and 10 is high vulnerability. The global mean is shown on the left, and we see that on",
                                   "average most indicators improved between 2010 and 2020. On the left we see the three countries which improved most",
                                   "(Cuba, Gerogia and Moldova) and the three countries which worsened most (Mali, Syria, Libya).",sep = "\n")) &
  theme(plot.title = element_text(size = 20, family = "Prata"),
        plot.subtitle = element_text(size = 15, family = "Prata"))


## alternative with layout


layout <- "
AAAABBCCDD
AAAABBCCDD
AAAAEEFFGG
AAAAEEFFGG
"

average + out[[1]] + out[[2]] + out[[3]] +
  out[[4]] + out[[5]] +out[[6]] +plot_layout(design = layout)
