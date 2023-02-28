# Load libraries
library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library("ggalt")
library("lubridate")
library("viridis")
library("plotly")
library("scales")

# Load dataset
library_df <- read.csv("./2017-2023-10-Checkouts-SPL-Data.csv")

# Create dataframe for author
leigh_bardugo_all_df <- library_df %>% 
  filter(Creator %in% c("Leigh Bardugo", "Bardugo, Leigh")) %>% 
  filter(!CheckoutYear == 2023)

# Rename books to match each other/consolidate all formats
leigh_bardugo_all_df$Creator <- tolower(leigh_bardugo_all_df$Creator)
leigh_bardugo_all_df$Title <- tolower(leigh_bardugo_all_df$Title)

leigh_bardugo_all_df$Title[str_detect(leigh_bardugo_all_df$Title, "shadow and bone")] <- "Shadow and Bone"

leigh_bardugo_all_df$Title[str_detect(leigh_bardugo_all_df$Title, "siege and storm")] <- "Siege and Storm"

leigh_bardugo_all_df$Title[str_detect(leigh_bardugo_all_df$Title, "ruin and rising")] <- "Ruin and Rising"

leigh_bardugo_all_df$Title[str_detect(leigh_bardugo_all_df$Title, "six of crows")] <- "Six of Crows"

leigh_bardugo_all_df$Title[str_detect(leigh_bardugo_all_df$Title, "crooked kingdom")] <- "Crooked Kingdom"

leigh_bardugo_all_df$Title[str_detect(leigh_bardugo_all_df$Title, "king of scars")] <- "King of Scars"

leigh_bardugo_all_df$Title[str_detect(leigh_bardugo_all_df$Title, "rule of wolves")] <- "Rule of Wolves"

grishaverse_df <- leigh_bardugo_all_df %>% 
  filter(Title %in% c("Shadow and Bone", "Siege and Storm", "Ruin and Rising",
                      "Six of Crows", "Crooked Kingdom", "King of Scars", "Rule of Wolves"))

# Find total checkouts per Grishaverse book from January to December of 2021
checkouts_per_months <- grishaverse_df %>% 
  select(Title, CheckoutYear, CheckoutMonth, Checkouts) %>% 
  filter(CheckoutYear == "2021") %>% 
  group_by(Title, CheckoutMonth) %>% 
  summarize(total_checkouts_per_month = sum(Checkouts, na.rm = TRUE)) %>% 
  arrange(desc(total_checkouts_per_month))

# Make interactive bar chart
chart1_plot <- ggplot(data = checkouts_per_months, mapping = aes(x = reorder(CheckoutMonth, +CheckoutMonth), 
                                                                 y = total_checkouts_per_month,
                                                                 fill = Title)) +
  scale_fill_manual(values = c("#240925", "#3E1138", "#56194f", "#802b5b", "#aa405b", "#d46059", "#fea775"), 
                    labels=c("Crooked Kingdom", "King of Scars", "Ruin and Rising", "Rule of Wolves",
                             "Shadow and Bone", "Siege and Storm", "Six of Crows")) +
  labs(title = "Monthly Checkouts for Leigh Bardugo's Grishaverse Novels in 2021", 
       x = "Month",
       y = "Number of Checkouts", 
       color = "Title") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black")) +
  
  geom_col()

ggplotly(chart1_plot, toltip = "text")
