## PMRF Feb 2020 Report: 
## Depth distribution plots

## Author: Michaela A. Kratofil
## Updated: 19 Oct 2020

## ========================================================================== ##

## load packages
library(tidyverse)
library(lubridate)
library(ggplot2)

## read in GIS-processed effort data 
eff <- read.csv("GIS output/Effort_thruJul2020_GIS_2020AUG30.csv", header = T)

## subset february effort data 
sub <- eff %>%
  filter(Year_ == 2020)
sub$Date_ <- mdy(sub$Date_)
sub$month <- month(sub$Date_)
feb <- filter(sub, month == 2)

## create depth bins
summary(feb$depth)
hm <- filter(feb, depth >= 0)
feb.w <- filter(feb, depth < 0)
summary(feb.w$depth)

hist(feb.w$depth)
feb.w$DEPTH <- round(feb.w$depth*-1, 0)
hist(feb.w$DEPTH)

ggplot(feb.w, aes(x = DEPTH)) +
  geom_histogram(binwidth = 100)

feb.w$depth_bin <- as.character(NA)
feb.w <- feb.w %>%
  mutate(
  depth_bin = ifelse(between(DEPTH, 1, 100), "1-100", depth_bin),
  depth_bin = ifelse(between(DEPTH, 101, 200), "101-200", depth_bin),
  depth_bin = ifelse(between(DEPTH, 201, 300), "201-300", depth_bin),
  depth_bin = ifelse(between(DEPTH, 301, 400), "301-400", depth_bin),
  depth_bin = ifelse(between(DEPTH, 401, 500), "401-500", depth_bin),
  depth_bin = ifelse(between(DEPTH, 501, 600), "501-600", depth_bin),
  depth_bin = ifelse(between(DEPTH, 601, 700), "601-700", depth_bin),
  depth_bin = ifelse(between(DEPTH, 701, 800), "701-800", depth_bin),
  depth_bin = ifelse(between(DEPTH, 801, 900), "801-900", depth_bin),
  depth_bin = ifelse(between(DEPTH, 901, 1000), "901-1000", depth_bin),
  depth_bin = ifelse(between(DEPTH, 1001, 1100), "1001-1100", depth_bin),
  depth_bin = ifelse(between(DEPTH, 1101, 1200), "1101-1200", depth_bin),
  depth_bin = ifelse(between(DEPTH, 1201, 1300), "1201-1300", depth_bin),
  depth_bin = ifelse(between(DEPTH, 1301, 1400), "1301-1400", depth_bin),
  depth_bin = ifelse(between(DEPTH, 1401, 1500), "1401-1500", depth_bin),
  depth_bin = ifelse(between(DEPTH, 1501, 1600), "1501-1600", depth_bin),
  depth_bin = ifelse(between(DEPTH, 1601, 1700), "1601-1700", depth_bin),
  depth_bin = ifelse(between(DEPTH, 1701, 1800), "1701-1800", depth_bin),
  depth_bin = ifelse(between(DEPTH, 1801, 1900), "1801-1900", depth_bin),
  depth_bin = ifelse(between(DEPTH, 1901, 2000), "1901-2000", depth_bin),
  depth_bin = ifelse(between(DEPTH, 2001, 2100), "2001-2100", depth_bin),
  depth_bin = ifelse(between(DEPTH, 2101, 2200), "2101-2200", depth_bin),
  )

feb.w %>%
  count(depth_bin) %>%
  mutate(perc = n / nrow(feb.w)) -> d.perc

feb.w$depth_bin <- factor(feb.w$depth_bin,
                          levels = c(
                            "1-100",
                            "101-200",
                            "201-300",
                            "301-400",
                            "401-500",
                            "501-600",
                            "601-700",
                            "701-800",
                            "801-900",
                            "901-1000",
                            "1001-1100",
                            "1101-1200",
                            "1201-1300",
                            "1301-1400",
                            "1401-1500",
                            "1501-1600",
                            "1601-1700",
                            "1701-1800",
                            "1801-1900",
                            "1901-2000",
                            "2001-2100",
                            "2101-2200"
                          ))
 
plot_theme <- function() {
  theme_bw() + # or classic, but prefers to have a rectangle/box around the entire plot
    theme(axis.text = element_text(color = 'black', size = 12),
          axis.title = element_text(color = 'black', face = 'bold', size = 15))# if have a continuous y-axis, make start at 0 (ggplot includes gap)
}

ggplot(feb.w, aes(depth_bin)) +
  geom_bar(aes(y = (..count..)/sum(..count..)),
           color = "black", fill = "lightgrey",
           width = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, .38),
                     expand = c(0,0),
                     breaks = seq(0, .38, by = .05)) +
  plot_theme() +
  ylab("Percentage") +
  xlab("Depth (m)") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = -0.0001),
    axis.title.x = element_text(vjust = -0.5),
    axis.title.y = element_text(vjust = 2),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black", size = 1.25)
  )
ggsave("PMRF Feb 2020 Report/Plots/KauaiFeb2020Effort_DepthDistributionPlotv2.jpg",
       width = 8.5, height = 6, units = "in", dpi = 300)

