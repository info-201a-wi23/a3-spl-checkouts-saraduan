

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

# Find total checkouts per Grishaverse book over the years 2017-2022
grishaverse_checkouts_per_year <- grishaverse_df %>% 
  select(Title, CheckoutYear, Checkouts, Creator) %>% 
  group_by(Title, CheckoutYear) %>% 
  summarize(total_checkouts_per_year = sum(Checkouts, na.rm = TRUE))

# Find total of all checkouts
all_checkouts <- grishaverse_checkouts_per_year %>% 
  summarize(total = sum(total_checkouts_per_year))

# Find total checkouts for Dregs series
dregs_checkouts_per_year <- grishaverse_checkouts_per_year %>% 
  filter(Title %in% c("Six of Crows", "Crooked Kingdom")) %>% 
  group_by(CheckoutYear) %>% 
  summarize(total = sum(total_checkouts_per_year))

dregs <- "Dregs Series"
dregs_checkouts_per_year$Title <- dregs

# Find total checkouts for Grisha trilogy
grisha_checkouts_per_year <- grishaverse_checkouts_per_year %>% 
  filter(Title %in% c("Shadow and Bone", "Siege and Storm", "Ruin and Rising")) %>% 
  group_by(CheckoutYear) %>% 
  summarize(total = sum(total_checkouts_per_year))

grisha <- "Grisha Trilogy"
grisha_checkouts_per_year$Title <- grisha

# Find total checkouts for King of Scars duology
king_of_scars_checkouts_per_year <- grishaverse_checkouts_per_year %>% 
  filter(Title %in% c("King of Scars", "Rule of Wolves")) %>% 
  group_by(CheckoutYear) %>% 
  summarize(total = sum(total_checkouts_per_year))

king_of_scars <- "King of Scars Duology"
king_of_scars_checkouts_per_year$Title <- king_of_scars

# Combine dataframes
all_series <- rbind(dregs_checkouts_per_year, grisha_checkouts_per_year)
all_series <- rbind(all_series, king_of_scars_checkouts_per_year)

# Make line plot
ggplot(data = all_series) +
  geom_line(mapping = aes(x = CheckoutYear, y = total, color = Title, lwd = 2)) +
  scale_color_manual(values = c("#260b2c", "#aa405b", "#fea775")) +
  guides(lwd = FALSE) +
  labs(title = "Total Checkouts for each Grishaverse Series", x = "Year",
       y = "Number of Checkouts", color = "Series Title") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black"))
