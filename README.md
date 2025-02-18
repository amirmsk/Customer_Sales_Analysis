
---

# Customer and Sales Analysis

This repository contains an analysis of customer behaviors and sales data using R. there is no data included.

## Project Overview

The aim of this project is to analyze customer behaviors and sales data to identify trends, patterns, and insights that can help improve business strategies.

## Data Preparation

1. **Loading Packages**: Essential libraries like `readxl`, `dplyr`, `ggplot2`, `lubridate`, and `forecast` are used.
2. **Importing Data**: Data is imported from an Excel file.
3. **Data Transformation**: Date columns are transformed, and outliers are removed.
4. **Cumulative Revenue Calculation**: Calculates cumulative revenue for each user.

## Analysis

1. **Cohort Analysis**: Groups customers by their initial join month and calculates average lifetime revenue.
2. **Forecasting**: Uses ARIMA to forecast future orders.
3. **Discount Analysis**: Compares total basket purchases between different discount types.
4. **Purchase Frequency**: Analyzes purchase frequency and intervals between purchases.
5. **Clustering**: Groups customers based on purchase behavior and total basket size.

## Visualizations

1. **Heatmaps**: Shows average lifetime revenue by cohort.
2. **Histograms**: Displays purchase frequency and intervals between purchases.
3. **Scatter Plots**: Plots mean intervals vs. total basket.
4. **Time Series Plots**: Visualizes total amount minus discount over time.
5. **Cluster Behavior Change**: Shows changes in behavior over time for different clusters.

## Conclusion

This analysis provides valuable insights into customer behaviors and sales patterns, helping to make data-driven decisions to enhance business strategies.

## How to Run

1. Ensure you have R and the necessary packages installed.
2. Clone the repository.
3. Place the data file in the specified path.
4. Run `task_script.R` to execute the analysis.

---
