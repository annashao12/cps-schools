library(tidyverse)
library(readr)
library(ggrepel)

# loading data sets
hs <- read_csv("data/school_progress_2024.csv", show_col_types = F) %>%
  filter(Primary_Category == "HS")

pop <- read_csv("data/school_profile_2024.csv", show_col_types = F) %>%
  filter(Primary_Category == "HS")

compiled <- merge(hs, pop, by = "School_ID", all.x = TRUE)

# summarizing data sets
student_count_total <- compiled %>%
  select(c(School_Type, Student_Count_Total)) %>%
  summarize(total_students = sum(Student_Count_Total),
            .by = School_Type)

type <- hs %>%
  summarize(num_schools = n(), .by = School_Type) %>%
  arrange(num_schools) %>%
  mutate(School_Type = factor(School_Type, levels = unique(School_Type), ordered = T))

# tidying data for scatterplot
df <- merge(type, student_count_total, by = "School_Type", all.x = TRUE) %>%
  arrange(num_schools) %>%
  mutate(School_Type = factor(School_Type, levels = unique(School_Type), ordered = T))

colnames(df) <- c("V1", "V2", "V3")

# scatterplot
ggplot(df,
       aes(x = V2,
           y = V3)) +
  geom_point() +
  geom_label_repel(aes(label = V1),
                   min.segment.length = 0.1) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Number of Schools",
       y = "Total Number of Students",
       title = "Number of Schools and Students by School Type",
       subtitle = "There are far more neighborhood and charter schools and students compared to other\nschool types") +
  theme(plot.title = element_text(color='black',face='bold'),
        axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'),
        text = element_text(size=16))
