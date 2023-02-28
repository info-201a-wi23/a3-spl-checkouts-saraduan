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

# Create summary list
summary_info <- list()

# Find total of all checkouts
all_checkouts <- grishaverse_checkouts_per_year %>% 
  summarize(total = sum(total_checkouts_per_year))

# Find most popular material type
summary_info$grishaverse_material_type <- grishaverse_df %>% 
  select(Title, CheckoutYear, Checkouts, MaterialType) %>% 
  summarize(max_type = max(MaterialType, na.rm = TRUE)) %>% 
  pull(max_type)
summary_info$grishaverse_material_type <- tolower(summary_info$grishaverse_material_type)

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

# Find most popular series
summary_info$highest_series <- all_series %>% 
  filter(total == max(total)) %>% 
  pull(Title)

# Find Grisha Trilogy checkouts
summary_info$grisha_trilogy_checkouts <- all_series %>% 
  filter(total == max(total)) %>% 
  pull(total)

# Find highest number of checkouts
summary_info$highest_checkouts <- all_checkouts %>% 
  filter(total == max(total)) %>% 
  pull(Title)

# Find lowest number of checkouts
summary_info$lowest_checkouts <- all_checkouts %>% 
  filter(total == min(total)) %>% 
  pull(Title)

# Find Shadow and Bone total checkouts
summary_info$sab_checkouts <- all_checkouts %>% 
  filter(Title == "Shadow and Bone") %>% 
  pull(total)

# Find Siege and Storm total checkouts
summary_info$sas_checkouts <- all_checkouts %>% 
  filter(Title == "Siege and Storm") %>% 
  pull(total)

# Find Ruin and Rising total checkouts
summary_info$rar_checkouts <- all_checkouts %>% 
  filter(Title == "Ruin and Rising") %>% 
  pull(total)

# Find Six of Crows total checkouts
summary_info$soc_checkouts <- all_checkouts %>% 
  filter(Title == "Six of Crows") %>% 
  pull(total)

# Find Crooked Kingdom total checkouts
summary_info$ck_checkouts <- all_checkouts %>% 
  filter(Title == "Crooked Kingdom") %>% 
  pull(total)

# Find King of Scars total checkouts
summary_info$kos_checkouts <- all_checkouts %>% 
  filter(Title == "King of Scars") %>% 
  pull(total)

# Find Rule of Wolves total checkouts
summary_info$row_checkouts <- all_checkouts %>% 
  filter(Title == "Rule of Wolves") %>% 
  pull(total)

# Find May 2021 number of checkouts
summary_info$total_may_checkouts <- grishaverse_df %>% 
  filter(CheckoutMonth == "5") %>% 
  filter(CheckoutYear == "2021") %>% 
  group_by(CheckoutMonth) %>% 
  summarize(may_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(may_checkouts)

# Find April 2021 number of checkouts
summary_info$total_april_checkouts <- grishaverse_df %>% 
  filter(CheckoutMonth == "4") %>% 
  filter(CheckoutYear == "2021") %>% 
  group_by(CheckoutMonth) %>% 
  summarize(april_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(april_checkouts)

# Find October 2021 number of checkouts
summary_info$total_oct_checkouts <- grishaverse_df %>% 
  filter(CheckoutMonth == "10") %>% 
  filter(CheckoutYear == "2021") %>% 
  group_by(CheckoutMonth) %>% 
  summarize(oct_checkouts = sum(Checkouts, na.rm = TRUE)) %>% 
  pull(oct_checkouts)

# Find difference in checkouts between May and April
summary_info$may_april_difference <- summary_info$total_may_checkouts - summary_info$total_april_checkouts

# Total checkouts
all_checkouts <- grishaverse_checkouts_per_year %>% 
  summarize(total = sum(total_checkouts_per_year))

summary_info$total_checkouts <- all_checkouts %>%
  summarize(all = sum(total)) %>% 
  pull(all)

# Find average checkouts per year from 2017-2022
summary_info$avg_per_year <- grishaverse_checkouts_per_year %>% 
  group_by(CheckoutYear) %>% 
  summarize(avg_checkouts = mean(total_checkouts_per_year)) %>% 
  summarize(total_avg = sum(avg_checkouts, na.rm = TRUE)) %>% 
  pull(total_avg)

# Find average per 7 books
summary_info$avg_per_novel <- summary_info$avg_per_year/7

# Find difference between highest checkout book and lowest checkout book
summary_info$high_low_difference <- summary_info$sab_checkouts - summary_info$row_checkouts
