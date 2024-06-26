library(tidyverse)
library(readr)
library(ggrepel)
library(readxl)
library(gghighlight)
library(cowplot)

# importing data
hs <- read_csv("data/school_progress_2024.csv", show_col_types = F) %>%
  filter(Primary_Category == "HS")

grad_rate <- read_excel("data/dropout_graduation.xlsx", 
                        sheet = "School 4 Year Cohort Rates (2)") %>%
  mutate(School_ID = `School ID`)

compiled <- merge(hs, grad_rate, by = "School_ID", all.x = TRUE) %>%
  select(c(Short_Name, School_Type, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`)) 

box <- compiled %>%
  select(c(Short_Name, School_Type, `2023`)) %>%
  na.omit() %>%
  filter(!(School_Type %in% c("Special Education", "Small", "Contract", "Citywide-Option")))

# defining outlier
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | 
           x > quantile(x, 0.75) + 1.5 * IQR(x))
}

# preparing list for boxplot
rownames(box) <- paste0(box$Short_Name, ": ", box$`2023`) 

for_boxplot <- box %>% 
  tibble::rownames_to_column(var = "outlier") %>% 
  group_by(School_Type) %>% 
  mutate(is_outlier = ifelse(is_outlier(`2023`), `2023`, as.numeric(NA)))

for_boxplot$outlier[which(is.na(for_boxplot$is_outlier))] <- as.numeric(NA)
no_outliers <- for_boxplot %>%
  filter(is.na(outlier))

# boxplot with outliers
ggplot(for_boxplot, 
       aes(y = `2023`, x = fct_reorder(School_Type, `2023`))) +
  geom_boxplot(fill = 'lightcoral',
               outlier.shape = 10,
               outlier.size = 3.5) + 
  geom_dotplot(binaxis='y', stackdir='center', binwidth = 1, alpha = 0.6) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "School Type",
       y = "Graduation Rate (%)",
       title = "Distribution of Graduation Rate by School Type",
       caption = "School types with limited data were not included",
       subtitle = "Most schools have a graduation rate above 75%") +
  geom_text_repel(aes(label = outlier),
                   na.rm = TRUE,
                   force = 1.5,
                   nudge_y = -3,
                   nudge_x = 1) +
  theme(legend.position = "none",
        plot.title = element_text(color='black',face='bold'),
        axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'),
        axis.text.x = element_text(angle = 30, hjust = 1))

# boxplot without outliers
ggplot(for_boxplot, 
       aes(y = `2023`, x = fct_reorder(School_Type, `2023`))) +
  geom_boxplot(fill = 'lightcoral',
               outliers = FALSE) + 
  geom_dotplot(data = no_outliers, binaxis='y', stackdir='center', binwidth = 1, alpha = 0.6) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "School Type",
       y = "Graduation Rate (%)",
       title = "Graduation Rate by School Type (Outliers Removed)",
       caption = "Schools with outlier graduation rates and school types with limited data were exluded",
       subtitle = "Neighborhood schools have the most variable graduation rate") +
  theme(legend.position = "none",
        plot.title = element_text(color='black',face='bold'),
        axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'),
        axis.text.x = element_text(angle = 30, hjust = 1))

### line plot
# preparing list for line plots
line_overall <- compiled %>%
  select(!c(Short_Name)) %>%
  na.omit() %>%
  filter(!(School_Type %in% c("Special Education", "Small", "Contract", "Citywide-Option"))) %>%
  summarize_at(vars(`2017`:`2023`), median) %>%
  pivot_longer(cols = everything(),
               names_to = "Year",
               values_to = "Rate") %>%
  mutate(School_Type = "Overall District")

line <- compiled %>%
  select(!c(Short_Name)) %>%
  na.omit() %>%
  filter(!(School_Type %in% c("Special Education", "Small", "Contract", "Citywide-Option"))) %>%
  group_by(School_Type) %>%
  summarize_at(vars(`2017`:`2023`), median) %>%
  pivot_longer(cols = !c(School_Type), names_to = 'Year', values_to= 'Rate')

line_all <- full_join(line, line_overall)

# line plot highlighting each school type
a <- ggplot(data = line_all, aes(x = Year, 
                                 y = Rate, 
                                 color = fct_reorder(School_Type, Rate, median, .desc = T), 
                                 group = School_Type)) +
  geom_line(aes()) +
  geom_point() +
  theme_bw() +
  labs(x = "Year",
       y = NULL,
       title = "Graduation Rate by School Type Over the Years",
       subtitle = "Graduation rate has been fluctuating for the past 3 years among several\nschool types, but selective enrollment school rates have remained more steady",
       caption = "Each data point represents the median rate",
       color = "School Type") +
  scale_y_continuous(position = "right", limits = c(69.9,100))+
  theme(legend.position = "bottom",
        plot.title = element_text(color='black',face='bold'),
        axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))+
  gghighlight(School_Type != "Overall District",
              use_direct_label = F)

# line plot highlighting overall district
b <- ggplot(data = line_all, aes(x = Year, 
                                 y = Rate, 
                                 color = fct_reorder(School_Type, Rate, median, .desc = T), 
                                 group = School_Type)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "Year",
       y = "Graduation Rate (%)",
       title = "District Graduation Rate",
       subtitle = "The overall district graduation rate has\ngenerally been rising slowly") +
  ylim(69.9, 100)+
  theme(legend.position = "none",
        plot.title = element_text(color='black',face='bold'),
        axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))+
  gghighlight(School_Type == "Overall District",
              use_direct_label = F)

# plotting line graphs next to each other
plot_grid(b, a, align = "h", ncol = 2, rel_widths = c(2/5, 3/5))

