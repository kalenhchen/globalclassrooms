### setup

library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggsignif)

# data sets

HW_Identifying_Barriers <- read.csv("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/liwc variable extractions (primary 9)/homework/IDingBarriers.csv")
HW_Youth_and_Unions <- read.csv("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/liwc variable extractions (primary 9)/homework/youthUnions.csv")
HW_Womens_March <- read.csv("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/liwc variable extractions (primary 9)/homework/womensMarch.csv")
HW_Free_tuition_and_UBI <- read.csv("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/liwc variable extractions (primary 9)/homework/tuitionUBI.csv")
FINAL_Youth_and_Unions <- read_xlsx("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/liwc variable extractions (primary 9)/final exam/final, youth and unions.xlsx")
FINAL_Free_tuition_and_UBI <- read_xlsx("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/liwc variable extractions (primary 9)/final exam/final, free tuition and UBI.xlsx")

# bar plotting

HW_Identifying_Barriers <- HW_Identifying_Barriers %>% 
  select(Analytic) %>% 
  mutate(Assessment = "Identifying Barriers HW (1)")
HW_Youth_and_Unions <- HW_Youth_and_Unions %>% 
  select(Analytic) %>% 
  mutate(Assessment = "Youth and Unions HW (2)")
HW_Womens_March <- HW_Womens_March %>%
  select(Analytic) %>%
  mutate(Assessment = "Women's March HW (3)")
HW_Free_tuition_and_UBI <- HW_Free_tuition_and_UBI %>% 
  select(Analytic) %>% 
  mutate(Assessment = "Free tuition and UBI HW (4)")
FINAL_Youth_and_Unions <- FINAL_Youth_and_Unions %>% 
  select(Analytic) %>% 
  mutate(Assessment = "Youth and Unions related exam responses (5)")
FINAL_Free_tuition_and_UBI <- FINAL_Free_tuition_and_UBI %>% 
  select(Analytic) %>% 
  mutate(Assessment = "Free tuition and UBI related exam responses (6)")

# combine to one data set
all_scores <- rbind(HW_Identifying_Barriers, HW_Youth_and_Unions, HW_Womens_March, HW_Free_tuition_and_UBI, FINAL_Youth_and_Unions, FINAL_Free_tuition_and_UBI)
all_scores <- all_scores %>% 
  pivot_longer(-Assessment, names_to = "Variable", values_to = "score")

my_colors <- c("#db735c", "#efa86e", "#9a8a76", "#f3c57b", "#7a6752", "#2a91a2")


all_scores %>%
  group_by(Variable, Assessment) %>%
  summarise(mean = mean(score, na.rm = TRUE),
            sd = sd(score, na.rm = TRUE)) %>%
  mutate(Assessment = factor(Assessment, levels = c("Identifying Barriers HW (1)", "Youth and Unions HW (2)", "Women's March HW (3)", "Free tuition and UBI HW (4)", "Youth and Unions related exam responses (5)", "Free tuition and UBI related exam responses (6)"))) %>%
  ggplot(aes(x = Assessment, y = mean, fill = Assessment)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = my_colors) +
  labs(title = "", caption = "Percentage of words in student responses indicative of analytic thinking.", x = "Assessment", y = "Mean score") +
  theme(legend.position = "right") +
  scale_x_discrete(labels = c("Identifying Barriers HW (1)" = "(1)", "Youth and Unions HW (2)" = "(2)", "Women's March HW (3)" = "(3)", "Free tuition and UBI HW (4)" = "(4)", "Youth and Unions related exam responses (5)" = "(5)", "Free tuition and UBI related exam responses (6)" = "(6)"))
