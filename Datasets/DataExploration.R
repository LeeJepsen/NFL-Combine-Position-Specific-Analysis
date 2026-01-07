## Lee Jepsen 
## NFL Project Combine and Pro Bowl analysis
## Statistical Inference
## Data Viz

## Clearing Environment
rm(list = ls())

## install packages
install.packages("dplyr")
install.packages("plyr")
install.packages("tidyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("writexl")
install.packages("reshape2")

## Load Packages 
library("dplyr")
library("plyr")
library("tidyr")
library("readr")
library("readxl")
library("stringr")
library("ggplot2")
library("writexl")
library("gridExtra")
library("reshape2")
library("tibble")


##Import MergedDataset
##includes all merged and cleaned Pro Bowl and NFL Combine files from 2000-2023
MergedData <- read_excel("C:/Users/YourName/Desktop/MergedDataset.xlsx")


## Defining and Mapping Positions by skill set
## Not including Kickers, Punters, Long Snappers,and FUllBacks lack of supporting data
PositionGroups <- list(OL = c("OL", "OG", "OT", "C"), 
                       TE = "TE", 
                       RB = "RB", 
                       WR = "WR", 
                       QB = "QB", 
                       DL = c("DL", "DT"), 
                       DE = c("DE", "EDGE"), 
                       LB = c("OLB", "ILB", "LB"), 
                       DB = c("DB", "S", "CB"))


## Creating Column for Position Group
MergedData <- MergedData %>%
  mutate(PositionGroup = case_when(Position %in% PositionGroups$OL ~ "OL",
                                   Position %in% PositionGroups$TE ~ "TE",
                                   Position %in% PositionGroups$RB ~ "RB",
                                   Position %in% PositionGroups$WR ~ "WR",
                                   Position %in% PositionGroups$QB ~ "QB",
                                   Position %in% PositionGroups$DL ~ "DL",
                                   Position %in% PositionGroups$DE ~ "DE",
                                   Position %in% PositionGroups$LB ~ "LB",
                                   Position %in% PositionGroups$DB ~ "DB",
                                   TRUE ~ "Other"
  ))

## Eliminating Players classified as "Other"
## Other ~ Undrafted, Drafted before 2000, Not invited to Combine
MergedData <- MergedData %>%
  filter(PositionGroup != "Other")

## function to calculate averages to loop through all position groups
calculate_position_averages <- function(position_group, data) {
  subset_data <- subset(data, PositionGroup == position_group)
  
  ## Overall average for position
  overall_avg <- subset_data %>%
    dplyr::summarise(across(c(Weight, 
                              Forty, 
                              VerticalJump, 
                              BenchPress, 
                              BroadJump, 
                              ThreeCone, 
                              Shuttle), 
                            mean, na.rm = TRUE)) %>%
    mutate(Group = "Overall")
  
  ## Pro Bowl average for position
  probowl_avg <- subset_data %>%
    filter(ProBowl == 1) %>%
    dplyr::summarise(across(c(Weight, 
                              Forty, 
                              VerticalJump, 
                              BenchPress, 
                              BroadJump, 
                              ThreeCone, 
                              Shuttle), 
                            mean, na.rm = TRUE)) %>%
    mutate(Group = "Pro Bowl")
  
  ## Combine and reshape the data
  combined <- bind_rows(overall_avg, probowl_avg) %>%
    pivot_longer(cols = -Group, names_to = "Metric", values_to = "Average") %>%
    mutate(PositionGroup = position_group) 
  
  return(combined)
}

## List of position groups
position_groups <- c("QB", "RB", "TE", "WR", "OL", "DL", "LB", "DE", "DB")

## Apply function to each metric and position group
all_positions_comparison <- lapply(position_groups, 
                                   calculate_position_averages, 
                                   data = MergedData) %>%
  bind_rows()



## Creating Table of averages for All players 
## Function to calculate means for all positions
calculate_means <- function(data) {
  data %>%
    group_by(PositionGroup) %>%
    dplyr::summarise(across(c(Weight, 
                              Forty, VerticalJump, 
                              BenchPress,
                              BroadJump, 
                              ThreeCone, 
                              Shuttle),
                            mean, na.rm = TRUE)) %>%
    rename_with(~paste0("Mean_", .), -PositionGroup) 
}

## Calculate means for all players
all_players_means <- calculate_means(MergedData)
all_players_means


## Creating Table of averages for Pro Bowl players
## Function to calculate means for all Pro Bowl players
calculate_pro_bowl_means <- function(data) {
  pro_bowl_data <- data %>% filter(ProBowl == 1)
  pro_bowl_means <- pro_bowl_data %>%
    group_by(PositionGroup) %>%
    dplyr::summarise(across(c(Weight, 
                              Forty, 
                              VerticalJump, 
                              BenchPress, 
                              BroadJump, 
                              ThreeCone, 
                              Shuttle),
                            mean, na.rm = TRUE)) %>%
    rename_with(~paste0("Mean_", .), -PositionGroup)
  return(pro_bowl_means)
}

## Calculate means for Pro Bowl players
pro_bowl_means <- calculate_pro_bowl_means(MergedData)
pro_bowl_means


## Export NFL combine average and Pro Bowl average tables to excel
#write_xlsx(all_players_means, "all_players_means.xlsx")
#write_xlsx(pro_bowl_means, "pro_bowl_means.xlsx")


## Data Viz exploration Part 1
## Repeat Density plots of each Metrics for each Position
## Forty yard dash plots
## Function to create density plot including Pro Bowl players
create_density_plot <- function(data, position_group) {
  non_probowl_data <- data %>%
    filter(PositionGroup == position_group & ProBowl == 0)
  probowl_data <- data %>%
    filter(PositionGroup == position_group & ProBowl == 1)
  ## Density Plot
  ggplot() +
    geom_density(data = non_probowl_data, aes(x = Forty, fill = "Non-Pro Bowl"),
                 alpha = 0.5, adjust = 1) +
    geom_density(data = probowl_data, aes(x = Forty, fill = "Pro Bowl"),
                 alpha = 0.5, adjust = 1) +
    labs(title = paste(position_group, " - 40 Yard Dash Times"),
         x = "Seconds", y = "Density") +
    scale_fill_manual(values = c("Non-Pro Bowl" = "gray", "Pro Bowl" = "red"), 
                      labels = c("Non-Pro Bowl", "Pro Bowl")) +
    theme_minimal() +
    theme(legend.position = "top")
}

## List of position groups
position_groups <- c("QB", "RB", "TE", "WR", "OL", "DL", "LB", "DE", "DB")

## Generate a list of plots
density_plots <- lapply(position_groups, function(pos_group) 
  create_density_plot(MergedData, pos_group))

## Arrange the plots into a grid format
library(gridExtra)
do.call(grid.arrange, c(density_plots, ncol = 3))



## Weight Plots
## Function to create density plot including Pro Bowl players
create_density_plot <- function(data, position_group) {
  non_probowl_data <- data %>%
    filter(PositionGroup == position_group & ProBowl == 0)
  probowl_data <- data %>%
    filter(PositionGroup == position_group & ProBowl == 1)
  ## Density Plot
  ggplot() +
    geom_density(data = non_probowl_data, aes(x = Weight, fill = "Non-Pro Bowl"),
                 alpha = 0.5, adjust = 1) +
    geom_density(data = probowl_data, aes(x = Weight, fill = "Pro Bowl"),
                 alpha = 0.5, adjust = 1) +
    labs(title = paste(position_group, " - Weight"),
         x = "Pounds", y = "Density") +
    scale_fill_manual(values = c("Non-Pro Bowl" = "gray", "Pro Bowl" = "orange"), 
                      labels = c("Non-Pro Bowl", "Pro Bowl")) +
    theme_minimal() +
    theme(legend.position = "top")
}

## List of position groups
position_groups <- c("QB", "RB", "TE", "WR", "OL", "DL", "LB", "DE", "DB")

## Generate a list of plots
density_plots <- lapply(position_groups, function(pos_group) 
  create_density_plot(MergedData, pos_group))

# Arrange the plots into a grid format
library(gridExtra)
do.call(grid.arrange, c(density_plots, ncol = 3))



## Vertical Jump Plots
## Function to create density plot including Pro Bowl players
create_density_plot <- function(data, position_group) {
  non_probowl_data <- data %>%
    filter(PositionGroup == position_group & ProBowl == 0)
  probowl_data <- data %>%
    filter(PositionGroup == position_group & ProBowl == 1)
  ## Density Plot
  ggplot() +
    geom_density(data = non_probowl_data, aes(x = VerticalJump, fill = "Non-Pro Bowl"),
                 alpha = 0.5, adjust = 1) +
    geom_density(data = probowl_data, aes(x = VerticalJump, fill = "Pro Bowl"),
                 alpha = 0.5, adjust = 1) +
    labs(title = paste(position_group, " - VerticalJump"),
         x = "Inches", y = "Density") +
    scale_fill_manual(values = c("Non-Pro Bowl" = "gray", "Pro Bowl" = "yellow"), 
                      labels = c("Non-Pro Bowl", "Pro Bowl")) +
    theme_minimal() +
    theme(legend.position = "top")
}

## List of position groups
position_groups <- c("QB", "RB", "TE", "WR", "OL", "DL", "LB", "DE", "DB")

## Generate a list of plots
density_plots <- lapply(position_groups, function(pos_group) 
  create_density_plot(MergedData, pos_group))

## Arrange the plots into a grid format
library(gridExtra)
do.call(grid.arrange, c(density_plots, ncol = 3))



## Bench Press Plots
## Function to create density plot including Pro Bowl players
create_density_plot <- function(data, position_group) {
  non_probowl_data <- data %>%
    filter(PositionGroup == position_group & ProBowl == 0)
  probowl_data <- data %>%
    filter(PositionGroup == position_group & ProBowl == 1)
  ## Density Plot
  ggplot() +
    geom_density(data = non_probowl_data, aes(x = BenchPress, fill = "Non-Pro Bowl"),
                 alpha = 0.5, adjust = 1) +
    geom_density(data = probowl_data, aes(x = BenchPress, fill = "Pro Bowl"),
                 alpha = 0.5, adjust = 1) +
    labs(title = paste(position_group, " - BenchPress"),
         x = "Reps of 225lbs", y = "Density") +
    scale_fill_manual(values = c("Non-Pro Bowl" = "gray", "Pro Bowl" = "green"), 
                      labels = c("Non-Pro Bowl", "Pro Bowl")) +
    theme_minimal() +
    theme(legend.position = "top")
}

## List of position groups
position_groups <- c("QB", "RB", "TE", "WR", "OL", "DL", "LB", "DE", "DB")

## Generate a list of plots
density_plots <- lapply(position_groups, function(pos_group) 
  create_density_plot(MergedData, pos_group))

## Arrange the plots into a grid format
library(gridExtra)
do.call(grid.arrange, c(density_plots, ncol = 3))



## Broad Jump Plots
## Function to create density plot including Pro Bowl players
create_density_plot <- function(data, position_group) {
  non_probowl_data <- data %>%
    filter(PositionGroup == position_group & ProBowl == 0)
  probowl_data <- data %>%
    filter(PositionGroup == position_group & ProBowl == 1)
  ## Density Plot
  ggplot() +
    geom_density(data = non_probowl_data, aes(x = BroadJump, fill = "Non-Pro Bowl"),
                 alpha = 0.5, adjust = 1) +
    geom_density(data = probowl_data, aes(x = BroadJump, fill = "Pro Bowl"),
                 alpha = 0.5, adjust = 1) +
    labs(title = paste(position_group, " - BroadJump"),
         x = "Inches", y = "Density") +
    scale_fill_manual(values = c("Non-Pro Bowl" = "gray", "Pro Bowl" = "cyan2"), 
                      labels = c("Non-Pro Bowl", "Pro Bowl")) +
    theme_minimal() +
    theme(legend.position = "top")
}

## List of position groups
position_groups <- c("QB", "RB", "TE", "WR", "OL", "DL", "LB", "DE", "DB")

## Generate a list of plots
density_plots <- lapply(position_groups, function(pos_group) 
  create_density_plot(MergedData, pos_group))

## Arrange the plots into a grid format
library(gridExtra)
do.call(grid.arrange, c(density_plots, ncol = 3))



## Three Cone Plots
## Function to create density plot including Pro Bowl players
create_density_plot <- function(data, position_group) {
  non_probowl_data <- data %>%
    filter(PositionGroup == position_group & ProBowl == 0)
  probowl_data <- data %>%
    filter(PositionGroup == position_group & ProBowl == 1)
  ## Density Plot
  ggplot() +
    geom_density(data = non_probowl_data, aes(x = ThreeCone, fill = "Non-Pro Bowl"),
                 alpha = 0.5, adjust = 1) +
    geom_density(data = probowl_data, aes(x = ThreeCone, fill = "Pro Bowl"),
                 alpha = 0.5, adjust = 1) +
    labs(title = paste(position_group, " - ThreeCone"),
         x = "Seconds", y = "Density") +
    scale_fill_manual(values = c("Non-Pro Bowl" = "gray", "Pro Bowl" = "blue"), 
                      labels = c("Non-Pro Bowl", "Pro Bowl")) +
    theme_minimal() +
    theme(legend.position = "top")
}

## List of position groups
position_groups <- c("QB", "RB", "TE", "WR", "OL", "DL", "LB", "DE", "DB")

## Generate a list of plots
density_plots <- lapply(position_groups, function(pos_group) 
  create_density_plot(MergedData, pos_group))

## Arrange the plots into a grid format
library(gridExtra)
do.call(grid.arrange, c(density_plots, ncol = 3))



## Shuttle Plots
## Function to create density plot including Pro Bowl players
create_density_plot <- function(data, position_group) {
  non_probowl_data <- data %>%
    filter(PositionGroup == position_group & ProBowl == 0)
  probowl_data <- data %>%
    filter(PositionGroup == position_group & ProBowl == 1)
  ## Density Plot
  ggplot() +
    geom_density(data = non_probowl_data, aes(x = Shuttle, fill = "Non-Pro Bowl"),
                 alpha = 0.5, adjust = 1) +
    geom_density(data = probowl_data, aes(x = Shuttle, fill = "Pro Bowl"),
                 alpha = 0.5, adjust = 1) +
    labs(title = paste(position_group, " - Shuttle"),
         x = "Seconds", y = "Density") +
    scale_fill_manual(values = c("Non-Pro Bowl" = "gray", "Pro Bowl" = "pink"), 
                      labels = c("Non-Pro Bowl", "Pro Bowl")) +
    theme_minimal() +
    theme(legend.position = "top")
}

## List of position groups
position_groups <- c("QB", "RB", "TE", "WR", "OL", "DL", "LB", "DE", "DB")

## Generate a list of plots
density_plots <- lapply(position_groups, function(pos_group) 
  create_density_plot(MergedData, pos_group))

## Arrange the plots into a grid format
library(gridExtra)
do.call(grid.arrange, c(density_plots, ncol = 3))




## Data Viz exploration part 2
## Density overlay plots for each metric, and separating by Offense and Defense

## Define offensive and defensive positions
offensive_positions <- c("QB", "RB", "WR", "TE", "OL")
defensive_positions <- c("DL", "DE", "LB", "DB")


## Defining Density data metrics
density_data <- MergedData %>%
  pivot_longer(cols = c(Weight, Forty, 
                        VerticalJump, 
                        BenchPress, 
                        BroadJump, 
                        ThreeCone, 
                        Shuttle),
               names_to = "Metric", 
               values_to = "Value")

## Separate data for offense and defense positions
density_data_offense <- density_data %>% filter(PositionGroup %in% offensive_positions)
density_data_defense <- density_data %>% filter(PositionGroup %in% defensive_positions)

## Create density plot for offensive positions
offense_plot <- ggplot(density_data_offense, aes(x = Value, fill = PositionGroup)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Metric, scales = "free", ncol = 2) +
  labs(
    title = "Density Plot of Combine Metrics by Offensive Positions",
    x = "Metric Value",
    y = "Density",
    fill = "Position Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

## Create density plot for defensive positions
defense_plot <- ggplot(density_data_defense, aes(x = Value, fill = PositionGroup)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Metric, scales = "free", ncol = 2) +
  labs(
    title = "Density Plot of Combine Metrics by Defensive Positions",
    x = "Metric Value",
    y = "Density",
    fill = "Position Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

## Load Density Plots
print(offense_plot)
print(defense_plot)




## Creating Same density Plots but for Pro Bowl athletes
## Filter data for Pro Bowlers only
density_data_pro_bowl <- MergedData %>%
  filter(ProBowl == 1) %>%
  pivot_longer(cols = c(Weight, Forty, 
                        VerticalJump, 
                        BenchPress, 
                        BroadJump, 
                        ThreeCone, 
                        Shuttle),
               names_to = "Metric", 
               values_to = "Value")

## Separate Pro Bowl data into offense and defense
density_data_pro_bowl_offense <- density_data_pro_bowl %>% 
  filter(PositionGroup %in% offensive_positions)

density_data_pro_bowl_defense <- density_data_pro_bowl %>% 
  filter(PositionGroup %in% defensive_positions)

## Create density plot for Pro Bowl athletes - offensive positions
pro_bowl_offense_plot <- ggplot(density_data_pro_bowl_offense, 
                                aes(x = Value, fill = PositionGroup)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Metric, scales = "free", ncol = 2) +
  labs(
    title = "Density Plot of Combine Metrics by Pro Bowl Offensive Positions",
    x = "Metric Value",
    y = "Density",
    fill = "Position Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

## Create density plot for Pro Bowl athletes - defensive positions
pro_bowl_defense_plot <- ggplot(density_data_pro_bowl_defense, aes(x = Value, fill = PositionGroup)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ Metric, scales = "free", ncol = 2) +
  labs(
    title = "Density Plot of Combine Metrics by Pro Bowl Defensive Positions",
    x = "Metric Value",
    y = "Density",
    fill = "Position Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

## Print the plots
print(pro_bowl_offense_plot)
print(pro_bowl_defense_plot)




## Data Viz Exploration Part 3
## Grid of Box plots for each position, comparing Combine metric averages.
## Function to create box plots for each position group including Pro Bowl athletes
create_ordered_boxplot <- function(data, metric, pro_bowl_only = FALSE) {
  ## Subset to Pro Bowl players if specified
  if (pro_bowl_only) {
    data <- data %>% filter(ProBowl == 1)
  }
  
  ## Calculate averages for the metric by position
  averages <- data %>%
    group_by(PositionGroup) %>%
    dplyr::summarize(Average = mean(.data[[metric]], na.rm = TRUE)) %>%
    arrange(Average)
  
  ## Reorder positions based on performance
  data <- data %>%
    mutate(PositionGroup = factor(PositionGroup, levels = averages$PositionGroup))
  
  ggplot(data, aes(x = .data[[metric]], y = PositionGroup, fill = PositionGroup)) +
    geom_boxplot(alpha = 0.5) +
    labs(
      title = paste(metric, "by Position",
                    ifelse(pro_bowl_only, "(Pro Bowl Players)", "")),
      x = metric,
      y = "Position"
    ) +
    scale_fill_brewer(palette = "Set3") +
    theme_minimal() +
    theme(legend.position = "none") +
    coord_flip()  # Flip coordinates for better readability
}

## List of metrics to plot
metrics <- c("Weight", 
             "Forty", 
             "VerticalJump", 
             "BenchPress", 
             "BroadJump", 
             "ThreeCone", 
             "Shuttle")

## Generate and arrange plots for all players
all_players_boxplots <- lapply(metrics, function(metric)
  create_ordered_boxplot(MergedData, metric, pro_bowl_only = FALSE))

## Generate and arrange plots for Pro Bowl players
pro_bowl_boxplots <- lapply(metrics, function(metric)
  create_ordered_boxplot(MergedData, metric, pro_bowl_only = TRUE))

## Arrange plots into grids
grid.arrange(grobs = all_players_boxplots, ncol = 3, top = "All Players")
grid.arrange(grobs = pro_bowl_boxplots, ncol = 3, top = "Pro Bowl Players")




## Statistical Inference
## ANOVA testing by position groups
## Compare Means of Pro Bowl and non Pro Bowl players
## subset players into Position groups
OL <- subset(MergedData, PositionGroup == "OL")
TE <- subset(MergedData, MergedData$PositionGroup == "TE")
FB <- subset(MergedData, MergedData$PositionGroup == "FB")
RB <- subset(MergedData, MergedData$PositionGroup == "RB")
WR <- subset(MergedData, MergedData$PositionGroup == "WR")
QB <- subset(MergedData, MergedData$PositionGroup == "QB")
DL <- subset(MergedData, PositionGroup == "DL")
DE <- subset(MergedData, PositionGroup == "DE")
LB <- subset(MergedData, PositionGroup == "LB")
DB <- subset(MergedData, PositionGroup == "DB")


## RB comparison
## Convert ProBowl to factor
RB$ProBowl <- as.factor(RB$ProBowl)

## Get list of all metrics columns
metrics <- colnames(RB)[4:10] 

## Function to extract ANOVA results
extract_anova_results <- function(metric) {
  anova_model_RB <- aov(get(metric) ~ ProBowl, data = RB)
  summary_info <- summary(anova_model_RB)[[1]]
  ## Extracting columns
  data.frame(
    Metric = metric,
    F_Value = summary_info[1, "F value"],
    DF1 = summary_info[1, "Df"],
    DF2 = summary_info[2, "Df"],
    P_Value = summary_info[1, "Pr(>F)"]
  )
}

## Apply function to each metric and compile results into a data frame
anova_results_RB <- do.call(rbind, lapply(metrics, extract_anova_results))

## Display the results
anova_results_RB




## QB comparison
##Convert ProBowl to factor
QB$ProBowl <- as.factor(QB$ProBowl)

## Get list of all metrics columns
metrics <- colnames(QB)[4:10] 

## Function to extract ANOVA results
extract_anova_results <- function(metric) {
  anova_model_QB <- aov(get(metric) ~ ProBowl, data = QB)
  summary_info <- summary(anova_model_QB)[[1]]
  ## Extracting columns
  data.frame(
    Metric = metric,
    F_Value = summary_info[1, "F value"],
    DF1 = summary_info[1, "Df"],
    DF2 = summary_info[2, "Df"],
    P_Value = summary_info[1, "Pr(>F)"]
  )
}

## Apply function to each metric and compile results into a data frame
anova_results_QB <- do.call(rbind, lapply(metrics, extract_anova_results))




## OL comparison
## Convert ProBowl to factor
OL$ProBowl <- as.factor(OL$ProBowl)

## Get list of all metrics columns
metrics <- colnames(OL)[4:10] 

## Function to extract ANOVA results
extract_anova_results <- function(metric) {
  anova_model_OL <- aov(get(metric) ~ ProBowl, data = OL)
  summary_info <- summary(anova_model_OL)[[1]]
  ## Extracting columns
  data.frame(
    Metric = metric,
    F_Value = summary_info[1, "F value"],
    DF1 = summary_info[1, "Df"],
    DF2 = summary_info[2, "Df"],
    P_Value = summary_info[1, "Pr(>F)"]
  )
}

## Apply function to each metric and compile results into a data frame
anova_results_OL <- do.call(rbind, lapply(metrics, extract_anova_results))




## TE comparison
## Convert ProBowl to factor
TE$ProBowl <- as.factor(TE$ProBowl)

## Get list of all metrics columns
metrics <- colnames(TE)[4:10] 

## Function to extract ANOVA results
extract_anova_results <- function(metric) {
  anova_model_TE <- aov(get(metric) ~ ProBowl, data = TE)
  summary_info <- summary(anova_model_TE)[[1]]
  ## Extracting columns
  data.frame(
    Metric = metric,
    F_Value = summary_info[1, "F value"],
    DF1 = summary_info[1, "Df"],
    DF2 = summary_info[2, "Df"],
    P_Value = summary_info[1, "Pr(>F)"]
  )
}

## Apply function to each metric and compile results into a data frame
anova_results_TE <- do.call(rbind, lapply(metrics, extract_anova_results))




## WR comparison
## Convert ProBowl to factor
WR$ProBowl <- as.factor(WR$ProBowl)

## Get list of all metrics columns
metrics <- colnames(WR)[4:10] 

## Function to extract ANOVA results
extract_anova_results <- function(metric) {
  anova_model_WR <- aov(get(metric) ~ ProBowl, data = WR)
  summary_info <- summary(anova_model_WR)[[1]]
  ## Extracting columns
  data.frame(
    Metric = metric,
    F_Value = summary_info[1, "F value"],
    DF1 = summary_info[1, "Df"],
    DF2 = summary_info[2, "Df"],
    P_Value = summary_info[1, "Pr(>F)"]
  )
}

## Apply function to each metric and compile results into a data frame
anova_results_WR <- do.call(rbind, lapply(metrics, extract_anova_results))




## DL comparison
## Convert ProBowl to factor
DL$ProBowl <- as.factor(DL$ProBowl)

## Get list of all metrics columns
metrics <- colnames(DL)[4:10] 

## Function to extract ANOVA results
extract_anova_results <- function(metric) {
  anova_model_DL <- aov(get(metric) ~ ProBowl, data = DL)
  summary_info <- summary(anova_model_DL)[[1]]
  ## Extracting columns
  data.frame(
    Metric = metric,
    F_Value = summary_info[1, "F value"],
    DF1 = summary_info[1, "Df"],
    DF2 = summary_info[2, "Df"],
    P_Value = summary_info[1, "Pr(>F)"]
  )
}

## Apply function to each metric and compile results into a data frame
anova_results_DL <- do.call(rbind, lapply(metrics, extract_anova_results))




## DE comparison
## Convert ProBowl to factor
DE$ProBowl <- as.factor(DE$ProBowl)

## Get list of all metrics columns
metrics <- colnames(DE)[4:10] 

## Function to extract ANOVA results
extract_anova_results <- function(metric) {
  anova_model_DE <- aov(get(metric) ~ ProBowl, data = DE)
  summary_info <- summary(anova_model_DE)[[1]]
  ## Extracting columns
  data.frame(
    Metric = metric,
    F_Value = summary_info[1, "F value"],
    DF1 = summary_info[1, "Df"],
    DF2 = summary_info[2, "Df"],
    P_Value = summary_info[1, "Pr(>F)"]
  )
}

## Apply function to each metric and compile results into a data frame
anova_results_DE <- do.call(rbind, lapply(metrics, extract_anova_results))




## LB comparison
## Convert ProBowl to factor
LB$ProBowl <- as.factor(LB$ProBowl)

## Get list of all metrics columns
metrics <- colnames(LB)[4:10] 

## Function to extract ANOVA results
extract_anova_results <- function(metric) {
  anova_model_LB <- aov(get(metric) ~ ProBowl, data = LB)
  summary_info <- summary(anova_model_LB)[[1]]
  ## Extracting columns
  data.frame(
    Metric = metric,
    F_Value = summary_info[1, "F value"],
    DF1 = summary_info[1, "Df"],
    DF2 = summary_info[2, "Df"],
    P_Value = summary_info[1, "Pr(>F)"]
  )
}

## Apply function to each metric and compile results into a data frame
anova_results_LB <- do.call(rbind, lapply(metrics, extract_anova_results))




## DB comparison
## Convert ProBowl to factor
DB$ProBowl <- as.factor(DB$ProBowl)

## Get list of all metrics columns
metrics <- colnames(DB)[4:10] 

## Function to extract ANOVA results
extract_anova_results <- function(metric) {
  anova_model_DB <- aov(get(metric) ~ ProBowl, data = DB)
  summary_info <- summary(anova_model_DB)[[1]]
  ## Extracting columns
  data.frame(
    Metric = metric,
    F_Value = summary_info[1, "F value"],
    DF1 = summary_info[1, "Df"],
    DF2 = summary_info[2, "Df"],
    P_Value = summary_info[1, "Pr(>F)"]
  )
}

## Apply function to each metric and compile results into a data frame
anova_results_DB <- do.call(rbind, lapply(metrics, extract_anova_results))




## Creating condensed table to export to excel
## Combine results for all positions
all_anova_results <- rbind(
  cbind(anova_results_RB, Position = "RB"),
  cbind(anova_results_QB, Position = "QB"),
  cbind(anova_results_OL, Position = "OL"),
  cbind(anova_results_TE, Position = "TE"),
  cbind(anova_results_WR, Position = "WR"),
  cbind(anova_results_DL, Position = "DL"),
  cbind(anova_results_DE, Position = "DE"),
  cbind(anova_results_LB, Position = "LB"),
  cbind(anova_results_DB, Position = "DB")
)

## Add significance column based on p-value
all_anova_results <- all_anova_results %>%
  mutate(Significant = ifelse(P_Value < 0.05, "Yes", "No"))

## check data
print(all_anova_results)

## Group data by position and summarize significant and non-significant metrics
summary_table <- all_anova_results %>%
  group_by(Position) %>%
  dplyr::summarize(
    Significant_Count = sum(Significant == "Yes"),
    Non_Significant_Count = sum(Significant == "No"),
    Total_Metrics = n(),
    P_Values = paste(round(P_Value, 3), collapse = ", ")
  ) %>%
  ungroup()

## Add a summary row
summary_row <- summary_table %>%
  dplyr::summarize(
    Position = "Summary",
    Significant_Count = sum(Significant_Count),
    Non_Significant_Count = sum(Non_Significant_Count),
    Total_Metrics = sum(Total_Metrics),
    P_Values = "N/A" 
  )

## Combine summary table and summary row
final_summary_table <- bind_rows(summary_table, summary_row)

## Print final summary table
print(final_summary_table)

## Export as Excel file
#write_xlsx(final_summary_table, "anova_summary.xlsx")



## ANOVA p-value Data Viz
## Creating a Heatmap of p-values
## Transform data into a wide format
heatmap_data <- dcast(all_anova_results, Metric ~ Position, value.var = "P_Value")

## Replace NA with 1 (non-significant for missing data)
heatmap_data[is.na(heatmap_data)] <- 1

## Make Heatmap into long format
heatmap_long <- melt(heatmap_data, id.vars = "Metric")

## Create significant and not significant on Heatmap
heatmap_long$Significance <- ifelse(heatmap_long$value < 0.05, "Significant", "Not Significant")

ggplot(heatmap_long, aes(x = variable, y = Metric, fill = Significance)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("Significant" = "lightgreen", "Not Significant" = "tomato3"),
    name = "Significance"
  ) +
  labs(
    title = "Significance Heatmap of ANOVA Results (α = 0.05)",
    x = "Position Group",
    y = "Metric"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right"
  )






