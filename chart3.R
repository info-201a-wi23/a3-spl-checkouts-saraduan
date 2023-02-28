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
library_df <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv")

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

# Find total checkouts per Grishaverse book over the years 2017-2022
grishaverse_checkouts_per_year <- grishaverse_df %>% 
  select(Title, CheckoutYear, Checkouts, Creator) %>% 
  group_by(Title, CheckoutYear) %>% 
  summarize(total_checkouts_per_year = sum(Checkouts, na.rm = TRUE))

# Find total of all checkouts
all_checkouts <- grishaverse_checkouts_per_year %>% 
  summarize(total = sum(total_checkouts_per_year))

# Create pie chart
piechart <- ggplot(all_checkouts, aes(x = "", y = total, fill = Title)) +
  geom_col() +
  scale_fill_manual(values = c("#240925", "#3E1138", "#56194f", "#802b5b", "#aa405b", "#d46059", "#fea775")) +
  labs(title = "Total Grishaverse Checkouts per Book from 2017-2022", fill = "Title") +
  geom_text(aes(label = total),
            color = "white",
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void() 

piechart
