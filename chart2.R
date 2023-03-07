# A3: chart 2
# Sara Duan
# INFO 201 AB

library("tidyverse")
library("ggplot2")
library("dplyr")
library("scales")

# load 2022-2023 checkouts dataset
checkouts <- read.csv("/Users/sarajduan/Desktop/2017-2023-10-Checkouts-SPL-Data.csv",stringsAsFactors = FALSE)


# checkout types
checkout_types <- checkouts %>%
  filter(UsageClass %in% c("Physical", "Digital")) %>%
  group_by(CheckoutYear, UsageClass) %>%
  summarise(total_checkouts = sum(Checkouts))


# plot
ggplot(data = checkout_types) + 
  geom_bar(aes(
    x = CheckoutYear, 
    y = total_checkouts, 
    fill = UsageClass),
    stat = "identity"
  ) +
  scale_fill_manual(values = c("#59a5d8", "#386fa4")) +
  scale_y_continuous(labels = label_number_si()) +
  labs(
    title = "Total Physical vs. Digital Checkouts Per Year (2017-2023)", 
    x="Year", 
    y="Number of Checkouts", 
    fill="Book Type"
  ) +
  theme(axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5))
