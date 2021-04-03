library(dplyr)
library(readxl)
library(ggplot2)
library(ggradar)

sysfonts::font_add_google(name = "Prata", "Prata")
showtext::showtext_auto()


df <- lapply(2006:2020,function(x) {
  read_xlsx(paste0("day03_historical/data/fsi-",x,".xlsx")) %>%
    select(1:16)
}) %>% 
  do.call(rbind,.) %>%
  mutate(Year=format(Year,"%Y"))

df_long <- df %>%
  filter(Year %in% c(2010,2020)) %>%
  group_by(Year) %>%
  summarise(across(4:15,.fns = function(x) mean(x)/10))

colnames(df_long)[-1] <- colnames(df_long)[-1] %>%
  substr(5,100) %>%
  stringr::str_wrap(15)

average <- df_long %>% ggradar(font.radar = "Circular Air",
                    values.radar = c("1 (high\nstability)","", "10"),
                    legend.position = "top")

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
  ggradar(font.radar = "Circular Air",
          axis.labels = rep("",12),
          values.radar = c("","", ""),
          legend.position = "none",
          plot.title = x)
})

library(patchwork)

average +
((out[[1]]+out[[2]]+out[[3]]) /
  (out[[4]] + out[[5]] + out[[6]])) +
  plot_annotation(
    
  )


ggplot()+
  geom_text(aes(x=0,y=0,label=paste("The index measures vulnerability of countries",
                  "based on 12 indicators which take values between 1 and 10",
                  "where 1 is high stability and 10 is high vulnerability.",
                  "We see the global average on the left, and see that on",
                  "average most indicators improved between 2010 and 2020.",
                  "On the left we see the three countries which improved most",
                  "(Cuba, Gerogia and Moldova) and the three countries which",
                  "worsened most (Mali, Syria, Libya).",sep = "\n")),
            hjust = 0, vjust = 1)+
  expand_limits(x=c(0,1),y=c(0,1))+
  labs(title = "The Fragile State Index")
