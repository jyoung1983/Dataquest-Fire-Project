library(readr)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)

# Import Data ----
forest_data <- read_csv("forestfires.csv")

# Manual Graphs Month and Day ----
# *Data by Month ----
forest_data_month <- forest_data %>%
    group_by(month) %>%
    summarize(total_fires = n()) %>%
    ungroup() %>%
    mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")))

ggplot(forest_data_month, aes(x = month, y = total_fires))+
    geom_bar(stat = "identity")

# *Data by Day ----
forest_data_day <- forest_data %>%
    group_by(day) %>%
    summarize(total_fires = n()) %>%
    ungroup() %>%
    mutate(day = factor(day, levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun")))

ggplot(forest_data_day, aes(x = day, y = total_fires))+
    geom_bar(stat = "identity")

# Compare Month or Day to each variable ----

# write function to create box plots
create_boxplot <- function(x, y) {
    ggplot(data = forest_data) + 
        aes_string(x = x, y = y) +
        geom_boxplot() +
        theme(panel.background = element_rect(fill = "white"))
}

# create vectors of variable names for both month and day
x_var_month <- names(forest_data)[3]
y_var_month <- names(forest_data)[5:12]
x_var_day <- names(forest_data)[4]
y_var_day <- names(forest_data)[5:12]

# apply the function to create the plots

# By month
map2(x_var_month, y_var_month, create_boxplot)

# By day
map2(x_var_day, y_var_day, create_boxplot)

# Compare each variable to area variable ----

# write function to create scatter plots
create_scatterplot <- function(x, y) {
    ggplot(data = forest_data) + 
        aes_string(x = x, y = y) +
        geom_point(alpha = 0.3) +
        theme(panel.background = element_rect(fill = "white"))
}

# create vectors of variable names; x will be variables FFMC:rain, and y will be area
x_vars <- names(forest_data)[5:12]
y_var <- names(forest_data)[13]

# apply create_scatterplot function
map2(x_vars, y_var, create_scatterplot)

# Filtered Subsets of forest_data ----
# Use different subsets of forest_data to get a better idea of how variables relate to area variable

# * No Large Outliers ----
# Removes large area outliers (outliers defined using interquartile range method)
forest_data_no_large_area <- forest_data %>%
    filter(area <= quantile(forest_data$area, 0.75)+IQR(forest_data$area)*1.5)

# Function and variable for scatterplot that excludes large outliers
create_scatterplot_no_large_outliers <- function(x, y) {
    ggplot(data = forest_data_no_large_area) + 
        aes_string(x = x, y = y) +
        geom_point(alpha = 0.3) +
        theme(panel.background = element_rect(fill = "white"))
}

scatter_no_large_outliers <- map2(x_vars, y_var, create_scatterplot_no_large_outliers)
scatter_no_large_outliers

# * No Zero Area Observations ----
# Removes 0 area observations
forest_data_no_zero_area <- forest_data %>%
    filter(area != 0)

# Function and variable for scatterplot that removes observations with 0 area
create_scatterplot_zero_area <- function(x, y) {
    ggplot(data = forest_data_no_zero_area) + 
        aes_string(x = x, y = y) +
        geom_point(alpha = 0.3) +
        theme(panel.background = element_rect(fill = "white"))
}

scatter_zero_area <- map2(x_vars, y_var, create_scatterplot_zero_area)
scatter_zero_area

# * No Outliers (large or small) ----
# Only shows non-outlier data (removes both small and large outliers)
forest_data_no_outliers <- forest_data %>%
    filter(area <= quantile(forest_data$area, 0.75)+IQR(forest_data$area)*1.5 & 
               area >= quantile(forest_data$area, 0.25)-IQR(forest_data$area)*1.5)

# Function and variable for scatterplot that only includes values in the interquartile range (i.e. removes all outliers)
create_scatterplot_no_outliers <- function(x, y) {
    ggplot(data = forest_data_no_outliers) + 
        aes_string(x = x, y = y) +
        geom_point(alpha = 0.3) +
        theme(panel.background = element_rect(fill = "white"))
}

scatter_no_outliers <- map2(x_vars, y_var, create_scatterplot_no_outliers)

# * No Outliers or Zero Area Observations ----
# Only shows non-outlier data (removes both small and large outliers)
forest_data_no_outliers_no_zero <- forest_data %>%
    filter(area != 0) %>%
    filter(area <= quantile(forest_data$area, 0.75)+IQR(forest_data$area)*1.5 & 
               area >= quantile(forest_data$area, 0.25)-IQR(forest_data$area)*1.5)

# Function and variable for scatterplot that only includes values in the interquartile range (i.e. removes all outliers)
create_scatterplot_no_outliers_no_zero <- function(x, y) {
    ggplot(data = forest_data_no_outliers_no_zero) + 
        aes_string(x = x, y = y) +
        geom_point(alpha = 0.3) +
        theme(panel.background = element_rect(fill = "white"))
}

scatter_no_outliers_no_zero <- map2(x_vars, y_var, create_scatterplot_no_outliers_no_zero)
scatter_no_outliers_no_zero
