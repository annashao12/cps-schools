library(tidyverse)
library(readr)
library(cowplot)

# loading and tidying data
hs <- read_csv("data/school_progress_2024.csv", show_col_types = F) %>%
  filter(Primary_Category == "HS")

type <- hs %>%
  summarize(num_schools = n(), .by = School_Type) %>%
  arrange(num_schools) %>%
  mutate(School_Type = factor(School_Type, levels = unique(School_Type), ordered = T))

# bar chart for number of each school type
n_type <- ggplot(type,
                 aes(x = num_schools,
                     y = School_Type)) +
  geom_bar(stat = "identity", fill = 'lightcoral') +
  geom_text(aes(label = num_schools), hjust = -0.2) +
  labs(title = "Type of Schools in CPS",
       subtitle = "Charter schools are most common, \nmore than neighborhood schools",
       x = "Number of Schools",
       y = "School Type") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(color='black',face='bold'),
        axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold')) +
  xlim(0, max(type$num_schools) * 1.05)

# preparing data for student population by school type bar chart
pop <- read_csv("data/school_profile_2024.csv", show_col_types = F) %>%
  filter(Primary_Category == "HS")

compiled <- merge(hs, pop, by = "School_ID", all.x = TRUE)

student_count_total <- compiled %>%
  select(c(School_Type, Student_Count_Total)) %>%
  summarize(total_students = sum(Student_Count_Total),
            .by = School_Type)

df <- merge(type, student_count_total, by = "School_Type", all.x = TRUE) %>%
  arrange(num_schools) %>%
  mutate(School_Type = factor(School_Type, levels = unique(School_Type), ordered = T))

# bar chart for student population
pop_total <- ggplot(df,
                    aes(x = total_students,
                        y = School_Type)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_students), hjust = -0.2) +
  labs(title = "Total Number of Students in CPS by School Type",
       subtitle = "Most school types have fewer than 10,000 total students\n ",
       x = "Number of Students",
       y = NULL) +
  theme_bw() +
  scale_y_discrete(position = "right") +
  theme(legend.position = "none",
        plot.title = element_text(color='black',face='bold'),
        axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold')) +
  xlim(0, max(df$total_students) * 1.1)

# plotting charts side-by-side
plot_grid(n_type, pop_total, align = "h", ncol = 2, rel_widths = c(3/7, 4/7))

# tidying for double bar chart
colnames(df) <- c("V1", "V2", "V3")

sf <- max(df$V2)/max(df$V3)

DF_long <- df %>%
  arrange(V2) %>%
  mutate(V3 = V3*sf) %>%
  pivot_longer(names_to = "y_new", values_to = "val", V2:V3) %>%
  mutate(V1 = factor(V1, levels = unique(V1), ordered = T))

# plotting double bar chart
ggplot(DF_long, aes(x=V1)) +
  geom_bar(aes(y = val, fill = y_new, group = y_new),
           stat="identity", position=position_dodge(),
           color = 'black', alpha = 0.9)  +
  scale_fill_manual(values = c("steelblue", "lightcoral"),
                    labels = c('Schools', 'Students')) +
  scale_y_continuous(name = "Number of Schools",labels = scales::comma,
                     sec.axis = sec_axis(~./sf, name="Total Number of Students", labels = scales::comma))+
  labs(fill = 'Count',
       title = 'Number of Schools and Students by School Type',
       subtitle = 'Neighborhood schools have the most number of students\nbut there are more charter schools',
       x = 'Type of School')+
  theme_bw()+
  theme(legend.position = 'top',
        plot.title = element_text(color='black',face='bold'),
        axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'),
        axis.text.x = element_text(angle = 30, hjust = 1))
