# A3: chart 3
# Sara Duan
# INFO 201 AB

library("tidyverse")
library("ggplot2")
library("dplyr")
library("scales")

# load 2017-2023 checkouts dataset
checkouts <- read.csv("/Users/sarajduan/Desktop/2017-2023-10-Checkouts-SPL-Data.csv",stringsAsFactors = FALSE)


# genres
thriller_checkouts <- checkouts %>%
  group_by(Subjects) %>%
  filter(grepl('Thriller', Subjects)) %>%
  summarise(total_checkouts = sum(Checkouts))
total_thriller_checkouts <- sum(thriller_checkouts$total_checkouts)

mystery_checkouts <- checkouts %>%
  group_by(Subjects) %>%
  filter(grepl('Mystery', Subjects)) %>%
  summarise(total_checkouts = sum(Checkouts))
total_mystery_checkouts <- sum(mystery_checkouts$total_checkouts)

romance_checkouts <- checkouts %>%
  group_by(Subjects) %>%
  filter(grepl('Romance', Subjects)) %>%
  summarise(total_checkouts = sum(Checkouts))
total_romance_checkouts <- sum(romance_checkouts$total_checkouts)

fantasy_checkouts <- checkouts %>%
  group_by(Subjects) %>%
  filter(grepl('Fantasy', Subjects)) %>%
  summarise(total_checkouts = sum(Checkouts))
total_fantasy_checkouts <- sum(fantasy_checkouts$total_checkouts)

drama_checkouts <- checkouts %>%
  group_by(Subjects) %>%
  filter(grepl('Drama', Subjects)) %>%
  summarise(total_checkouts = sum(Checkouts))
total_drama_checkouts <- sum(drama_checkouts$total_checkouts)

science_checkouts <- checkouts %>%
  group_by(Subjects) %>%
  filter(grepl('Science', Subjects)) %>%
  summarise(total_checkouts = sum(Checkouts))
total_science_checkouts <- sum(science_checkouts$total_checkouts)


# plot
genre_checkouts <- data.frame(
  genre=c("Thriller", "Mystery", "Romance", 
          "Fantasy", "Drama", "Science"),
  total_checkouts=c(
    total_thriller_checkouts, total_mystery_checkouts, 
    total_romance_checkouts, total_fantasy_checkouts, 
    total_drama_checkouts, total_science_checkouts))

ggplot(data = genre_checkouts) +
  geom_bar(aes(
    x = genre,
    y = total_checkouts,
    fill = genre),
    stat = "identity"
  ) +
  scale_fill_manual(values = c("#91e5f6", "#84d2f6", "#59a5d8", "#386fa4", "#133c55", "#051923")) +
  scale_y_continuous(labels = label_number_si()) +
  labs(
    title = "Total Checkouts for Common Genres (2017-2023)", 
    x = "Genre", 
    y = "Number of Checkouts"
  ) +
  coord_flip() +
  theme(axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5))
