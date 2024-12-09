library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(caret)
library(reshape2)

all_data <- read.csv("data.csv",
                     check.names = FALSE,
                     stringsAsFactors = FALSE)

# cleaning up the column names so that they snake_case
colnames(all_data) <- tolower(gsub(" ", "_", colnames(all_data)))
colnames(all_data) <- gsub("[?]", "", colnames(all_data))

# remove net_income_flag since it has only one value: "1"
all_data <- all_data %>% 
  select(-net_income_flag) 

all_data <- all_data %>% 
  mutate(bankrupt = as.factor(bankrupt))

# Show class imbalance
class_counts <- table(all_data$bankrupt)
class_percentages <- prop.table(class_counts) * 100

print(class_counts)
print(class_percentages)

barplot <- barplot(class_counts,
        main = "Class Imbalance",
        xlab = "Class",
        ylab = "Count",
        col = c("skyblue", "salmon"),
        cex.names = 2,
        cex.axis = 2,
        cex.lab = 2,
        names.arg = c("Not Bankrupt", "Bankrupt"))

text(x = barplot, 
     y = class_counts / 2,
     label = paste0(round(class_percentages, 1), "%"), 
     cex = 2, col = "black")



# DENSITY PLOTS

ggplot(all_data, aes(x = `tax_rate_(a)`, fill = bankrupt)) +
  geom_density(alpha = 0.5) +
  labs(
    x = "Tax Rate",
    y = "Density",
    fill = "Target (Bankrupt Status)"
  ) +
  scale_fill_manual(values = c("skyblue", "salmon"),
                    labels = c("Non-Bankrupt", "Bankrupt")) +
  theme_minimal() +
  theme(text=element_text(size=20), #change font size of all text
        axis.text=element_text(size=12), #change font size of axis text
        axis.title=element_text(size=18), #change font size of axis titles
        plot.title=element_text(size=18), #change font size of plot title
        legend.text=element_text(size=11), #change font size of legend text
        legend.title=element_text(size=13)) #change font size of legend title   

ggplot(all_data, aes(x = `total_asset_turnover`, fill = bankrupt)) +
  geom_density(alpha = 0.5) +
  labs(
    x = "Total Asset Turnover",
    y = "Density",
    fill = "Target (Bankrupt Status)"
  ) +
  scale_fill_manual(values = c("skyblue", "salmon"),
                    labels = c("Non-Bankrupt", "Bankrupt")) +
  theme_minimal() +
  theme(text=element_text(size=20), #change font size of all text
        axis.text=element_text(size=10), #change font size of axis text
        axis.title=element_text(size=18), #change font size of axis titles
        plot.title=element_text(size=18), #change font size of plot title
        legend.text=element_text(size=11), #change font size of legend text
        legend.title=element_text(size=13)) #change font size of legend title   