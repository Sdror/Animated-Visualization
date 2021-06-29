library(tidyverse)
library(gganimate)
library(ggplot2)
library(gifski)
theme_set(theme_classic())

Nikayon <- read.csv("C:\\Users\\Shlomit\\Documents\\מסמכים שלי\\nikayon17_20.csv")

Nikayon  <- subset(Nikayon, !grepl(pattern, city , ignore.case = TRUE))
colnames(Nikayon) <- gsub("city", "city", colnames(Nikayon))
colnames(Nikayon) <- gsub("Value", "value", colnames(Nikayon))
colnames(Nikayon) <- gsub("Year", "year", colnames(Nikayon))


gap <- Nikayon %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = min_rank(-value) * 1,
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",value)) %>%
  filter(rank <=16) %>%
  ungroup()

p <- ggplot(gap, aes(rank, group = city, 
                     fill = as.factor(city), color = as.factor(city))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(city, " ")), vjust = 0.2, hjust = 1,size = 9) +
  geom_text(aes(y=value,label = paste0(" ",round(value,0),"%"), hjust = 0), size = 5) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  
  labs(title="?מי הן הערים המובילות בניקיון", subtitle='{closest_state}', x ="", y="אחוז בני 20 ומעלה המרוצים מהניקיון באזור מגוריהם, בערים הגדולות"    
       ,caption ="מקור: הסקר החברתי, הלשכה המרכזית לסטטיסטיקה, עיבוד ועיצוב: שלומית דרור" ) +
  
  theme(plot.title = element_text(hjust = 0.5, vjust = 5, size = 28, color = "purple", face="bold"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 4, size = 26, color = "black", face="bold"),
        plot.caption = element_text(hjust = 0.95, vjust = -3, size = 22, color = "purple"),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(), # These relate to the axes post-flip
        axis.title.y  = element_text(size = 12),
        axis.text.x  = element_text(hjust = 0.5, size = 10),
        axis.title.x  = element_text(hjust = 0.5, vjust = 0, size = 16),
        plot.margin = margin(1,1.2,1,4.5, "cm")) +
  
  transition_states(year, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')+ 
  exit_fade()

#transition_events(start = 10, end, range = NULL, enter_length = 10, exit_length = NULL)
plot(p)



animate(p, 200, fps = 11, duration = 9, width = 800, height = 600, renderer = gifski_renderer("nikayon1.gif"))
gifski_renderer(file = p(fileext = ".gif"), loop = TRUE,
                width = NULL, height = NULL)
anim_save("animationNikayon2020.gif", animation = (p) )
