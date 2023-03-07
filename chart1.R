library("tidyverse")
library("ggplot2")
library("dplyr")

# load 2022-2023 checkouts dataset
checkouts <- read.csv("/Users/sarajduan/Desktop/2022-2023-All-Checkouts-SPL-Data.csv",stringsAsFactors = FALSE)

checkouts <- checkouts %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
checkouts$date <- as.Date(spl_df$date, format = "%Y-%m-%d")


# material types
hardcover_books <- checkouts %>%
  filter(MaterialType %in% c("BOOK")) %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts))  
hardcover <- "Hardcover Books"
hardcover_books$MaterialType <- hardcover

audio_books <- checkouts %>%
  filter(MaterialType %in% c("AUDIOBOOK")) %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts)) 
audio <- "Audio Books"
audio_books$MaterialType <- audio

e_books <- checkouts %>%
  filter(MaterialType %in% c("EBOOK")) %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts))  
elec <- "Electronic Books"
e_books$MaterialType <- elec

sound_discs <- checkouts %>% 
  filter(MaterialType %in% c("SOUNDDISC")) %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts))  
sound <- "Sound Discs"
sound_discs$MaterialType <- sound


# plot
all_materials <- rbind(hardcover_books, audio_books)
all_materials <- rbind(all_materials, e_books)
all_materials <- rbind(all_materials, sound_discs)

ggplot(data = all_materials) +
  geom_line(aes(
    x = date, 
    y = total_checkouts, 
    color = MaterialType
  )) +
  geom_point(aes(
    x = date,
    y = total_checkouts,
    color = MaterialType
  )) +
  scale_color_manual(values = c("#84d2f6", "#59a5d8", "#386fa4", "#133c55")) +
  guides(lwd = FALSE) +
  scale_y_continuous(labels = label_number_si()) +
  labs(
    title = "Total Checkouts of Each Material Per Month (2022-2023)", 
    x = "Month",
    y = "Number of Checkouts", 
    color = "Material Type"
  ) +
  theme(axis.line = element_line(color = "black"))
