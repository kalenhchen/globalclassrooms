library(dplyr)
library(ggplot2)
library(ggsignif)

HW_Identifying_Barriers <- read.csv("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/tone/1. IDing Barriers.csv")
HW_Youth_and_Unions <- read.csv("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/tone/2. Youth and Unions.csv")
HW_Womens_March <- read.csv("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/tone/3. Women's March.csv")
HW_Free_Tuition_and_UBI <- read.csv("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/tone/4. Free Tuition and UBI.csv")
FINAL_Youth_and_Unions <- read.csv("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/tone/5.2 Final, Youth and Unions.csv")
FINAL_Free_Tuition_and_UBI <- read.csv("C:/Users/kalen/OneDrive - University of Toronto/global classrooms/data analysis/tone/5.4 Final, Free Tuition and UBI.csv")

data_list <- list(
  "HW_Identifying_Barriers" = HW_Identifying_Barriers,
  "HW_Youth_and_Unions" = HW_Youth_and_Unions,
  "HW_Womens_March" = HW_Womens_March,
  "HW_Free_Tuition_and_UBI" = HW_Free_Tuition_and_UBI,
  "FINAL_Youth_and_Unions" = FINAL_Youth_and_Unions,
  "FINAL_Free_Tuition_and_UBI" = FINAL_Free_Tuition_and_UBI
)

# order of data frames
desired_order <- c("HW_Identifying_Barriers", "HW_Youth_and_Unions", "HW_Womens_March", "HW_Free_Tuition_and_UBI", "FINAL_Youth_and_Unions", "FINAL_Free_Tuition_and_UBI")

# Reorder the data frames in the desired order
data_list_reordered <- data_list[desired_order]

# Create an empty list to store the mean values
mean_scores <- list()

# Iterate over each data frame and calculate the mean of tone_pos
for (name in names(data_list_reordered)) {
  mean_scores[[name]] <- data_list_reordered[[name]] %>%
    summarise(mean_tone_pos = mean(tone_pos)) %>%
    mutate(Assessment = name)
}

# Combine the mean scores into a single data frame
all_mean_scores <- bind_rows(mean_scores)

# Plot the mean scores
ggplot(all_mean_scores, aes(x = Assessment, y = mean_tone_pos, fill = Assessment)) +
  geom_bar(stat = "identity") +
  labs(title = "Positive tone across assessments",
       caption = "Writing indicative of positive tone within student responses.",
       x = "Assessment", y = "Score, shown in percent") +
  theme(legend.position = c(0.8, 0.8)) +
  ylim (0, 5)
