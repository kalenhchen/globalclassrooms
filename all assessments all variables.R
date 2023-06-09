### setup

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggsignif)

### data sets

HW_Identifying_Barriers <- read.csv("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/liwc variable extractions (primary 9)/homework/IDingBarriers.csv")
HW_Youth_and_Unions <- read.csv("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/liwc variable extractions (primary 9)/homework/youthUnions.csv")
HW_Womens_March <- read.csv("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/liwc variable extractions (primary 9)/homework/womensMarch.csv")
HW_Free_tuition_and_UBI <- read.csv("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/liwc variable extractions (primary 9)/homework/tuitionUBI.csv")
FINAL_Youth_and_Unions <- read_xlsx("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/liwc variable extractions (primary 9)/final exam/final, youth and unions.xlsx")
FINAL_Free_tuition_and_UBI <- read_xlsx("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/liwc variable extractions (primary 9)/final exam/final, free tuition and UBI.xlsx")

# bar plotting

HW_Identifying_Barriers <- HW_Identifying_Barriers %>% 
  select(Analytic, Clout, Tone, Cognition, Social, Lifestyle, work, Perception, space) %>% 
  mutate(Assessment = "Identifying Barriers HW")
HW_Youth_and_Unions <- HW_Youth_and_Unions %>% 
  select(Analytic, Clout, Tone, Cognition, Social, Lifestyle, work, Perception, space) %>% 
  mutate(Assessment = "Youth and Unions HW")
HW_Womens_March <- HW_Womens_March %>%
  select(Analytic, Clout, Tone, Cognition, Social, Lifestyle, work, Perception, space) %>%
  mutate(Assessment = "Women's March HW")
HW_Free_tuition_and_UBI <- HW_Free_tuition_and_UBI %>% 
  select(Analytic, Clout, Tone, Cognition, Social, Lifestyle, work, Perception, space) %>% 
  mutate(Assessment = "Free tuition and UBI HW")
FINAL_Youth_and_Unions <- FINAL_Youth_and_Unions %>% 
  select(Analytic, Clout, Tone, Cognition, Social, Lifestyle, work, Perception, space) %>% 
  mutate(Assessment = "Youth and Unions related exam responses")
FINAL_Free_tuition_and_UBI <- FINAL_Free_tuition_and_UBI %>% 
  select(Analytic, Clout, Tone, Cognition, Social, Lifestyle, work, Perception, space) %>% 
  mutate(Assessment = "Free tuition and UBI related exam responses")

# combine to one data set
all_scores <- rbind(HW_Identifying_Barriers, HW_Youth_and_Unions, HW_Womens_March, HW_Free_tuition_and_UBI, FINAL_Youth_and_Unions, FINAL_Free_tuition_and_UBI)
all_scores <- all_scores %>% 
  pivot_longer(-Assessment, names_to = "Variable", values_to = "score")

# aggregate scores based on dimensions
library(stringr)
all_scores$Variable <- str_to_title(all_scores$Variable)
all_scores <- all_scores %>% mutate(var2 = case_when(Variable == "Analytic"~"Thinking",
                                                     Variable == "Cognition"~"Thinking",
                                                     Variable == "Clout"~"Social Leadership",
                                                     Variable == "Social"~"Social Leadership",
                                                     Variable == "Lifestyle"~"Work and Lifestyle",
                                                     Variable == "Work"~"Work and Lifestyle",
                                                     Variable == "Perception"~"Experiential",
                                                     Variable == "Space"~"Experiential",
                                                     Variable == "Tone"~"Experiential"
))

all_scores$Variable <- factor(all_scores$Variable, levels =c("Analytic","Cognition","Clout","Social","Lifestyle","Work","Perception","Space","Tone"))

my_colors <- c("#db735c", "#efa86e", "#9a8a76", "#f3c57b", "#7a6752", "#2a91a2")


all_scores %>% 
  group_by(Variable, Assessment) %>% 
  mutate(mean = mean(score,na.rm=T)) %>% 
  mutate(Assessment = factor(Assessment, levels = c("Identifying Barriers HW","Youth and Unions HW","Women's March HW","Free tuition and UBI HW","Youth and Unions related exam responses","Free tuition and UBI related exam responses"))) %>% 
  ggplot(aes(x = Variable,  y = mean, fill = Assessment)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = my_colors) +
  labs(title = "", caption = "Percentage of words in student responses that fall into select LIWC-22 categories.",x = "Linguistic dimension",  y = "Mean score") +
  theme(legend.position = "bottom") +
  ylim(0, 90) +
  geom_signif(y_position = c(90,80), xmin = c(0.5,2.5), 
              xmax = c(2.4,4.4), annotation = c("Thinking","Social Leadership"),
              tip_length = .05) +
  geom_signif(y_position = c(60,75), xmin = c(4.6,6.5), 
              xmax = c(6.4,9.4), annotation = c("Work & Lifestyle","Experiential"),
              tip_length = .05)
