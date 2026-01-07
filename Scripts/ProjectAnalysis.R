## Lee Jepsen 
## NFL Project Combine and Pro Bowl analysis
## Descriptive Statistics
## Applied Probability
## Logistic Regression


## Clearing Environment
rm(list = ls())

## install packages
install.packages("dplyr")
install.packages("plyr")
install.packages("readr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("VennDiagram")
install.packages("pscl")



## Load Packages
library("readxl")
library("dplyr")
library("plyr")
library("readr")
library("stringr")
library("ggplot2")
library("tidyr")
library("VennDiagram")
library("gridExtra")
library("broom")
library("car")
library("pscl")


##Import MergedDataset
##includes all merged and cleaned Pro Bowl and NFL Combine files from 2000-2023
MergedData <- read_excel("C:/Users/YourName/Desktop//MergedDataset.xlsx")

## Defining and Mapping Positions by skill set
## Not including Kickers, Punters, and Long Snappers, lack of supporting data
PositionGroups <- list(OL = c("OL", "OG", "OT", "C"), 
                       TE = "TE", 
                       FB = "FB", 
                       RB = "RB", 
                       WR = "WR", 
                       QB = "QB", 
                       DL = c("DL", "DT"), 
                       DE = c("DE", "EDGE"), 
                       LB = c("OLB", "ILB", "LB"), 
                       DB = c("DB", "S", "CB") 
)


## Creating Column for Position Group
MergedData <- MergedData %>%
  mutate(PositionGroup = case_when(Position %in% PositionGroups$OL ~ "OL",
                                   Position %in% PositionGroups$TE ~ "TE",
                                   Position %in% PositionGroups$FB ~ "FB",
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
### Other ~ Undrafted, Drafted before 2000, Not invited to Combine
MergedData <- MergedData %>%
  filter(PositionGroup != "Other")



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

## Descriptive Statistics for each Position group 
## Evaluating Performance metrics - calculating mean values for Positions
## Mean performances for each position in NFL Combine
MeanPerformance <- MergedData %>%
  group_by(PositionGroup) %>%
  dplyr::summarise(across(where(is.numeric),mean, na.rm = TRUE), .groups = "drop")

## Mean performance for Pro Bowl player combine metrics 
ProBowlPerformance <- MergedData %>%
  filter(ProBowl == 1) %>%
  group_by(PositionGroup) %>%
  dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")


## Merging MeanPerformance(all players) and ProBowlPerformance(Pro Bowl Players) 
ComparisonTable <- MeanPerformance %>%
  left_join(ProBowlPerformance, 
            by = "PositionGroup", 
            suffix = c("_All", "_ProBowl"))




## Conditional Probability of Position making the Pro Bowl
## Grouping data by position and calculating proportion of ProBowlers in each group
#issues with summarise function, have to specify from dplyr package to work
ConditionalProBowlProbabilities <- MergedData %>%
  group_by(PositionGroup) %>%
  dplyr::summarise(TotalPlayers = length(PositionGroup),             
                   ProBowlers = sum(ProBowl, na.rm = TRUE),         
                   ProBowlProbability = ProBowlers / TotalPlayers    
  ) %>%
  arrange(desc(ProBowlProbability))                   


## Bar Plot the probability of each position making the pro bowl
ggplot(ConditionalProBowlProbabilities, 
       aes(x = reorder(PositionGroup, -ProBowlProbability), 
           y = ProBowlProbability, fill = PositionGroup)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(ProBowlProbability, accuracy = 0.1)), 
            vjust = -0.5, size = 3) +  # Add percentages to the bars
  labs(title = "Pro Bowl Probability by Position",
       x = "Position Group", y = "Pro Bowl Probability (%)") +
  theme_minimal()




## Prepping data for Logistic Regression
## Converting ProBowl column to factor
MergedData$ProBowl <- factor(MergedData$ProBowl, levels = c(0,1))

## Converting Combine metrics to Numeric value
MergedData$Weight       <- as.numeric(MergedData$Weight)
MergedData$Forty        <- as.numeric(MergedData$Forty)
MergedData$VerticalJump <- as.numeric(MergedData$VerticalJump)
MergedData$BenchPress   <- as.numeric(MergedData$BenchPress)
MergedData$BroadJump    <- as.numeric(MergedData$BroadJump)
MergedData$ThreeCone    <- as.numeric(MergedData$ThreeCone)
MergedData$Shuttle      <- as.numeric(MergedData$Shuttle)


## Creating a function to loop through Logistic Regression for each player
## Instead of doing each position separately
## Function to perform logistic regression and extract desired metrics
logistic_regression_with_metrics <- function(data, position_group) {
  ## Fit logistic regression model
  model <- glm(ProBowl ~ 
                 Weight + 
                 Forty + 
                 VerticalJump +
                 BenchPress + 
                 BroadJump +
                 ThreeCone + 
                 Shuttle, 
               data = data, 
               family = binomial)
  
  ## Extract coefficients, p-values, and odds ratios
  summary_df <- as.data.frame(coef(summary(model)))
  colnames(summary_df) <- c("Estimate", "StdError", "ZValue", "PValue")
  summary_df$OddsRatio <- exp(summary_df$Estimate)
  
  ## Calculate McFadden's R2
  mcfadden_r2 <- pR2(model)["McFadden"]
  
  ## Add R² as an attribute or column to the summary
  summary_df$McFaddenR2 <- mcfadden_r2
  
  ## Print results for this position
  cat("\nLogistic Regression Results for", position_group, "\n")
  print(summary_df)
  
  ## Return results as a list
  return(list(Summary = summary_df, Model = model, McFaddenR2 = mcfadden_r2))
}

## List of position groups and data
position_data <- list(OL = OL,
                      QB = QB,
                      TE = TE, 
                      RB = RB, 
                      WR = WR, 
                      DL = DL, 
                      DE = DE,
                      LB = LB, 
                      DB = DB)

## Run logistic regression for all positions
results <- lapply(names(position_data), function(pos) {
  logistic_regression_with_metrics(position_data[[pos]], pos)
})

## Create DF for each position
RB_result <- logistic_regression_with_metrics(RB, "RB")
QB_result <- logistic_regression_with_metrics(QB, "QB")
OL_result <- logistic_regression_with_metrics(OL, "OL")
TE_result <- logistic_regression_with_metrics(TE, "TE")
WR_result <- logistic_regression_with_metrics(WR, "WR")
DL_result <- logistic_regression_with_metrics(DL, "DL")
DE_result <- logistic_regression_with_metrics(DE, "DE")
LB_result <- logistic_regression_with_metrics(LB, "LB")
DB_result <- logistic_regression_with_metrics(DB, "DB")

## Adding significance column 
add_significance <- function(result) {
  df <- result$Summary
  
  ## Define significance levels
  df$Significance <- ifelse(df$PValue < 0.001, "***",
                            ifelse(df$PValue < 0.01, "**",
                                   ifelse(df$PValue < 0.05, "*", "")))
  
  ## Return the updated result
  result$Summary <- df
  return(result)
}

## Apply this function to each result
RB_result <- add_significance(RB_result)
QB_result <- add_significance(QB_result)
OL_result <- add_significance(OL_result)
TE_result <- add_significance(TE_result)
WR_result <- add_significance(WR_result)
DL_result <- add_significance(DL_result)
DE_result <- add_significance(DE_result)
LB_result <- add_significance(LB_result)
DB_result <- add_significance(DB_result)


## Creating excel sheet with results by page
convert_summary <- function(result) {
  df <- result$Summary
  df$Metric <- row.names(df)
  row.names(df) <- NULL
  return(df)
}

## converting row names to a metric to show up on table
RB_df <- convert_summary(RB_result)
QB_df <- convert_summary(QB_result)
OL_df <- convert_summary(OL_result)
TE_df <- convert_summary(TE_result)
WR_df <- convert_summary(WR_result)
DL_df <- convert_summary(DL_result)
DE_df <- convert_summary(DE_result)
LB_df <- convert_summary(LB_result)
DB_df <- convert_summary(DB_result)

## results as pages
all_results <- list(
  RB = RB_df,
  QB = QB_df,
  OL = OL_df,
  TE = TE_df,
  WR = WR_df,
  DL = DL_df,
  DE = DE_df,
  LB = LB_df,
  DB = DB_df
)


## export 
#write_xlsx(all_results, "LogRegAll_Position_Results.xlsx")