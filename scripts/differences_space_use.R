library(tidyverse)
library(readr)
library(readxl)

# importing data
hs <- read_csv("data/school_progress_2024.csv", show_col_types = F) %>%
  filter(Primary_Category == "HS")

space_use_orig <- read_excel("data/space_use.xlsx",  
                             sheet = "Data") %>%
  mutate(School_ID = `School ID`)

# tidying data
selected_schools <- merge(hs, space_use_orig, by = "School_ID", all.x = TRUE) %>%
  filter(School_Type %in% c("Career academy", "Military academy", "Selective enrollment", "Neighborhood", "Small", "Magnet")) %>%
  select(c(School_Type, `Space Use Status`)) %>%
  na.omit() #one NA from Neighborhood

space_compiled <- selected_schools %>%
  group_by(School_Type, `Space Use Status`) %>%
  summarize(n = n()) %>%
  mutate(`Space Use Status` = factor(`Space Use Status`, 
                                     levels = c("Underutilized", "Efficient", "Overcrowded"), 
                                     ordered = T),
         School_Type = factor(School_Type,
                              levels = c("Military academy", 
                                         "Small", 
                                         "Neighborhood", 
                                         "Career academy", 
                                         "Magnet",
                                         "Selective enrollment"),
                              ordered = T)) %>% 
  group_by(School_Type) %>%
  mutate(perc = n/sum(n))

# creating the proportion bar chart
ggplot(space_compiled, aes(x = School_Type, y = perc, fill = `Space Use Status`)) + 
  geom_col(position = position_stack(), color = "black") +
  scale_fill_manual(values = c("lightcoral", "steelblue", "maroon4")) +
  geom_text(aes(label = n), 
            position = position_stack(vjust =0.5), 
            size = 7,
            color = "white") +
  theme_bw() +
  labs(x = "School Type",
       y = "Proportion",
       title = "Space Use Status by School Type",
       caption = "Several school types were omitted either because data was unavailable or there were fewer than 10 samples.\nThe label on each portion of the bars indicate the number of schools in that category.",
       subtitle = "Proportion of underutilized selective enrollment schools is smallest compared to other school types") +
  theme(legend.position = "top",
        plot.title = element_text(color='black',face='bold'),
        axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))
