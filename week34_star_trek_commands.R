#Rita Raher
# Tidy Tuesday
# http://www.speechinteraction.org/TNG/TeaEarlGreyHotDatasetCodeBook.pdf

# library
library(dplyr)    
library(ggplot2) 
library(tidyverse)
library(showtext)
library(sysfonts)
library(ggthemes) #plot themes
library(extrafont) # fonts
font_import()

# Get the Data
# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!


# load data ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-08-17')
tuesdata <- tidytuesdayR::tt_load(2021, week = 34)

computer <- tuesdata$computer

computer$char <- as.factor(computer$char)
computer$domain <- as.factor(computer$domain)



# clean up data ------------------------------------------------------------

computer <- computer %>% mutate(char=recode(char, 
                         `Riker (O.S.)`="Riker",
                         `Riker'S Voice`="Riker", 
                         `Computer (V.O.)`="Computer",
                         `New Computer Voice`="Computer",
                         `Computer'S Voice`="Computer", 
                         `Computer (Voice)`="Computer",
                         `Com Panel (V.O.)`="Computer",
                         `Computer Voice (V.O.)`="Computer",
                         `Computer (V.O)`="Computer", 
                         `Computer'S Voice`="Computer", 
                         `Computer Voice`="Computer",
                         `Computer Voice (Cont'D)`="Computer",
                         `Data (O.S.)`="Data", 
                         `Data (V.O.)`="Data", 
                         `Geordi (O.S.)`="Geordi",
                         `Geordi (V.O.)`="Geordi", 
                         `Krag (O.S.)`="Krag",
                         `Picard (Cont'D)`="Picard",
                         `Picard (O.S.)`="Picard",
                         `Picard (V.O.)`="Picard",
                         `Satelk (O.C.)`="Satelk",
                         `Worf (O.S.)`="Worf", 
                         `Troi (Cont'D) `="Troi"
                         ))

computer <- computer %>% mutate(domain=recode(domain, 
                                            `Iot`="IoT"
))


# wrangle data ------------------------------------------------------------

df_names <- computer %>%
  filter(char!="Computer") %>%
  group_by(char) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>% 
  slice_head(n = 7)
         
df_names$char

# plot ------------------------------------------------------------

b <- computer %>%
  filter(char %in% df_names$char) %>%
  filter(!is.na(domain)) %>%
  group_by(char, domain) %>%
  summarise(n = n(), na.rm=TRUE) %>%
  arrange(desc(n))%>%
  ggplot(aes(char, n, fill=domain)) +
  geom_bar(stat='identity') +
  coord_flip() +
  labs(title="Types of interactions by character", subtitle = "Week 34 of #TidyTuesday") +
  theme_hc(bgcolor = "darkunica") +
  scale_fill_hc("darkunica") + 
  labs(caption = "(based on data from ... source:speechinteraction.org)") +
  xlab("Character") + ylab("Interactions")

b



computer %>%
  filter(char %in% df_names$char) %>%
  filter(!is.na(domain)) %>%
  group_by(char, domain) %>%
  summarise(n = n(), na.rm=TRUE) %>%
  arrange(desc(n))%>%
  ggplot(aes(char, n, fill=domain)) +
  geom_bar(stat='identity') +
  coord_flip() +
  labs(title="Types of interactions by character", subtitle = "Week 34 of #TidyTuesday") +
  theme_void()+
  theme(
    plot.title = element_text(family="Berlin Sans FB Demi",color="black", size=26),
    plot.caption = element_text(family = "Berlin Sans FB", size = 13),
    plot.margin = unit(c(1,1,0.5,1),"cm"),
    panel.background = element_rect(fill = "#FFF5DE",color = NA),
    plot.background = element_rect(fill = "#FFF5DE",color = NA),
    axis.text.x = element_text(angle = 90, hjust=1,vjust=0.5,size=15, 
                               color="black", family = "Berlin Sans FB"),
    legend.position = 'none'
  )+
  labs(caption = "(based on data from ... source:speechinteraction.org)") +
  xlab("Character") + ylab("Interactions")
  
