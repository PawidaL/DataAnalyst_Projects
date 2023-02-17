##Dataset : Diamonds

library(tidyverse)
library(patchwork)
library(ggthemes)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
  
glimpse(diamonds)

##Create column color_scale and clarity_scale in order to categorize the color and clarity of diamonds.
df_diamonds <- diamonds %>% 
mutate(color_scale = case_when(
   color == c("D" , "E" , "F") ~ "Colorless",
   TRUE ~ "Near Colorless"
   )) %>% 
  mutate(
    color_scale = factor(
      color_scale,
      labels = c("Colorless", "Near Colorless" ),
      levels = c("Colorless", "Near Colorless" ),
      ordered = TRUE
      )
    ) %>% 
  mutate(clarity_scale = case_when(
    clarity == "IF" ~ "Internally flawless",
    clarity == c("VVS1", "VVS2") ~ "Very very slightly included",
    clarity == c("VS1", "VS2") ~ "Very slightly included",
    clarity == c("SI1", "SI2") ~ "Slightly included",
    TRUE ~ "Included" 
  )) %>% 
  mutate(
    clarity_scale = factor(
      clarity_scale,
      labels = c("Internally flawless",
                 "Very very slightly included",
                 "Very slightly included",
                 "Slightly included",
                 "Included"),
      levels = c("Internally flawless",
                 "Very very slightly included",
                 "Very slightly included",
                 "Slightly included",
                 "Included"),
      ordered = TRUE
      ))
 
##Price of diamond group by clarity with the sampling size 1000 diamonds
set.seed(26)
df_diamonds %>% 
  sample_n(1000) %>% 
  ggplot( aes(carat, price , color=clarity_scale, alpha=clarity_scale)) +
  geom_point() +
  theme_minimal() +
  theme(legend.justification=c(1,0), 
        legend.position = c(1,0)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Carat",
    y = "Price(USD)",
    caption = "Source: ggplot package") 
    
##Price comparison between colorless and near colorless diamond by very good and premium cut
set.seed(16)
df_diamonds %>% 
  sample_n(5000) %>% 
  filter (cut == c("Very Good", "Premium")) %>% 
  ggplot( aes( price, cut, fill = cut)) +
  geom_bar(stat = "identity", width=0.5) +
  facet_wrap(~color_scale, scale = "free") +
  scale_fill_hue(c = 40) +
  labs(
    title = "Price comparison by color and cut",
    x = "Price") +
  theme( legend.position = "top",
         legend.title = element_blank())
         
##Distribution of each diamonds clarity group 
df_diamonds %>% 
  ggplot( aes(price, group=clarity_scale, fill=clarity_scale))+
  geom_density() +
  facet_wrap(~clarity_scale)+
  scale_fill_brewer( palette = "Spectral") +
  labs ( title = "Price by diamonds clarity")
  
##The distribution of colorless and near colorless of diamonds based on a summary number of minimum, Q1, median, Q3 and maximum.
set.seed(12)
df_diamonds %>% 
 sample_n(1000) %>% 
 filter (clarity_scale == c("Very slightly included",
                             "Slightly included",
                             "Included")) %>% 
 ggplot( aes( color_scale, price, fill = clarity_scale))+
 geom_boxplot() +
 facet_wrap(~color_scale, scale = "free") +
 scale_fill_brewer( palette = "Spectral") +
 theme_minimal()+
 theme(legend.position = "bottom",
       legend.title = element_blank())
      
      
