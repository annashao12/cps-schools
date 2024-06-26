library(tidyverse)
library(readr)
library(ggrepel)

# importing data
hs <- read_csv("data/school_progress_2024.csv", show_col_types = F) %>%
  filter(Primary_Category == "HS")

pop <- read_csv("data/school_profile_2024.csv", show_col_types = F) %>%
  filter(Primary_Category == "HS")

compiled <- merge(hs, pop, by = "School_ID", all.x = TRUE)

# defining outlier function
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | 
           x > quantile(x, 0.75) + 1.5 * IQR(x))
}

# tidying data for boxplot
pop_per_school <- compiled %>%
  select(c(Short_Name.x, School_Type, Student_Count_Total)) 

rownames(pop_per_school) <- paste0(pop_per_school$Short_Name.x, ": ", pop_per_school$Student_Count_Total) 

for_boxplot <- pop_per_school %>% 
  tibble::rownames_to_column(var = "outlier") %>% 
  group_by(School_Type) %>% 
  mutate(is_outlier = ifelse(is_outlier(Student_Count_Total), Student_Count_Total, as.numeric(NA)))

for_boxplot$outlier[which(is.na(for_boxplot$is_outlier))] <- as.numeric(NA)
for_boxplot[63, 1] <- NA

# creating the boxplot
ggplot(for_boxplot, 
       aes(y = Student_Count_Total, x = fct_reorder(School_Type, Student_Count_Total))) +
  geom_boxplot(fill = 'lightcoral',
               outlier.shape = 10,
               outlier.size = 3.5) + 
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 50, alpha = 0.6) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "School Type",
       y = "Number of Students",
       title = "Distribution of Student Population Size by School Type",
       caption = "The outlier 'PATHWAYS - BRIGHTON PARK HS: 405,' a citywide-option, is not labeled",
       subtitle = "Schools of several types have large student populations\nbut overall, selective enrollment schools tend to have more students") +
  geom_label_repel(aes(label = outlier),
                   na.rm = TRUE,
                   force = 1.5,
                   nudge_y = 2) +
  theme(legend.position = "none",
        plot.title = element_text(color='black',face='bold'),
        axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'),
        axis.text.x = element_text(angle = 30, hjust = 1))
