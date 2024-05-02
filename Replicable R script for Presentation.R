library(tidyverse)
library(knitr)
library(cowplot)
library(patchwork)
library(ggplot2)
library(readxl)
library(dplyr)

#Loading Data
setwd("C:/Users/emory/Documents/Statistics Level 2")
df = read_excel("Copy of cities.xlsx")

#Filtered data to only include MENA region
Middle_E_NAfrica <- df %>% 
  filter(REGION == "Middle East and North Africa")

#CREATION OF 1ST VISUALIZATION:
plot1 <- ggplot(Middle_E_NAfrica, aes(x = CITY, y = DEATHEVENTS)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", size = 1) +  # Add black borders
  geom_text(aes(label = DEATHEVENTS), vjust = -0.5, color = "black", size = 3) +  # Add count labels within bars
  labs(x = "City", y = "Number of Deadly Social Disorder Events", 
       title = "Deadly Social Disorder Events in MENA by City") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # Make x-axis text bold
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        plot.background = element_rect(fill = "white", color = "black"),
        panel.border = element_rect(color = "black", fill = NA),
        axis.ticks = element_line(color = "black", size = 0.5))

print(plot1)


#CREATION OF 2ND VISUALIZATION: LINE PLOTS COMPRING THE RELATIONSHIP BETWEEN THE
#NUMBER OF SOCIAL DISORDER EVENTS IN MENA AND LATIN AMERICA AND THE NUMBER OF DEADLY
#EVENTS


#Created Mena lineplot showing relationship between DEATHEVENTS and NEVENTS
plot1 <- ggplot(Middle_E_NAfrica, aes(x = NEVENTS, y = DEATHEVENTS)) +
  geom_line(color = "purple") +
  geom_point() +
  scale_y_continuous(breaks = seq(0, max(Middle_E_NAfrica$DEATHEVENTS), by = 50)) +
  scale_x_continuous(breaks = seq(0, max(Middle_E_NAfrica$NEVENTS), by = 100)) +
  labs(x = "Number of events", y = "Number of Deadly events") +  
  annotate("text", x = Inf, y = Inf, label = "MENA", size = 6, hjust = 1.6, vjust = 1) +
  theme(axis.title.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 15), face = "bold"),
        panel.border = element_rect(color = "black", fill = NA))

#Created Latin America dataset
Latin_America <- df %>%
  filter(REGION == "Latin America")

#Created Latin America lineplot showing relationship between DEATHEVENTS and NEVENTS
plot4 <- ggplot(Latin_America, aes(x = NEVENTS, y = DEATHEVENTS)) +
  geom_line(color = "red") +
  geom_point() +
  scale_y_continuous(breaks = NULL, limits = c(0, 350), minor_breaks = seq(0, 350, by = 50)) +
  scale_x_continuous(breaks = seq(0, max(Latin_America$NEVENTS), by = 50)) +
  labs(x = "Number of events", y = NULL) +  
  theme(panel.grid.major = element_line(color = "white", linetype = "solid"),
        panel.grid.minor = element_line(color = "white", linetype = "solid"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 15), face ="bold"), 
        axis.title.y = element_blank(),
        panel.border = element_rect(color = "black", fill = NA)) +
  annotate("text", x = Inf, y = Inf, label = "Latin America", size = 6, hjust = 1.1, vjust = 1)

# Combine plots side by side 
combined_plots <- plot1 + plot4 +
  plot_layout(ncol = 2) +  
  theme(
    plot.margin = margin(40, 40, 40, 40),  
    axis.title = element_text(margin = margin(t = 20))
  )

print(combined_plots)








