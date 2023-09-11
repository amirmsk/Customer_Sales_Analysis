#1-1


#load the packages
library(readxl) 
library(writexl)
library(dplyr)
library(ggplot2)
library(lubridate)


#import the data
data <- read_excel("C:\\Users\\user\\Documents\\projects\\task_SnapFood\\data.xlsx")


#preapare the data
#data transformation
data$created_at<-  as.Date(data$created_at, "%m/%d/%Y")
data$discount_type<- as.factor(data$discount_type)


#remove the outlier
data <- data %>% filter(created_at > "2022-1-1")
summary(data)


#create the initial_join
first_month_per_user <- data %>%
  group_by(user_id) %>%
  summarize(initial_join = format(min(created_at), "%Y-%m"))
data <- merge(data, first_month_per_user, by = "user_id")


#cumulative revenue
cumulative_revenue <- data %>%
  group_by(user_id, initial_join, created_at) %>%
  summarize(cumulative_rev = sum(basket - discount_cost)) %>%
  ungroup() %>%
  group_by(user_id,initial_join) %>%
  mutate(cumulative_rev = cumsum(cumulative_rev))


# Merge the cumulative revenue back to the original data frame
data <- data %>%
  left_join(cumulative_revenue, by = c("user_id", "initial_join", "created_at"))


#clear the columns
#data <- subset(data, select = -c(cumulative_rev.x, cumulative_rev.y))


# Create a month-year column for 'created_at'
data$month_year <- format(data$created_at, "%Y-%m")


# Group data by cohort (first_month) and month_year to get the average lifetime revenue
cohort_data <- data %>%
  group_by(initial_join, month_year) %>%
  summarize(lifetime_revenue = mean(cumulative_rev, na.rm = TRUE))


# Calculate the difference in months between the cohort month and the order month
cohort_data$month_diff <- interval(ymd(paste0(cohort_data$initial_join, "-01")), 
                                   ymd(paste0(cohort_data$month_year, "-01"))) / months(1)


#Plotting the heatmap
ggplot(cohort_data, aes(x = month_diff, y = initial_join, fill = lifetime_revenue)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "average life time revenue",
       x = "Months Since First Purchase",
       y = "initial join",
       fill = "Average Lifetime Revenue") +
  theme_minimal()

library(forecast)


# Convert 'created_at' column to datetime if it's not already
data$created_at <- as.Date(data$created_at)

# Extract month and year from 'created_at'
data$created_at <- format(data$created_at, "%Y-%m-%d")


# Filter for users added in each mont
added_users_by_month<- data %>%
  group_by(created_at) %>%
  distinct(user_id) %>%
  ungroup()


# Prepare data for users' behavior after their user ID was created
created_at <- added_users_by_month %>%
  inner_join(added_users_by_month, by = c("user_id", "created_at")) %>%
  group_by(user_id) %>%
  summarize(creation_date = min(created_at)) %>% 
  ungroup()

#lead
lead_variable <- created_at %>%
  mutate(creation_date = as.Date(creation_date), 
         lead_variable = creation_date %m+% months(1))

# murge the lead_variable to added_user
added_users_by_month <- added_users_by_month %>%
  left_join(lead_variable, by = c("user_id"))

#change the data format in the added_user dataset
added_users_by_month$created_at<- as.Date(added_users_by_month$created_at)
added_users_by_month$created_at<- format(added_users_by_month$created_at, format = "%y-%m")
added_users_by_month$lead_variable<- as.Date(added_users_by_month$lead_variable)
added_users_by_month$lead_variable<- format(added_users_by_month$lead_variable, format = "%y-%m")
added_users_by_month$creation_date <- as.Date(added_users_by_month$creation_date)
added_users_by_month$creation_date <- format(added_users_by_month$creation_date, format ="%y-%m" )


#subset the unwanted column
#added_users_by_month <- subset(added_users_by_month, select = -month_year)

# Calculate the future behavior for the lead month
lead_future_behaviour <- added_users_by_month %>%
  filter( created_at == lead_variable) %>%
  group_by(user_id, lead_variable) %>%
  summarize(total_orders_lead_month = n()) %>%
  ungroup()


# Convert lead_variable to a Date format
lead_future_behaviour$lead_variable <- as.Date(lead_future_behaviour$lead_variable, format = "%y-%m")

# Create a time series object
ts_data <- ts(lead_future_behaviour$total_orders_lead_month, frequency = 12)

# Fit an ARIMA model
arima_model <- auto.arima(ts_data)

# Forecast for lead_variable of August 2022 (20-8)
forecast_result <- forecast(arima_model, h = 1)

# Extract the forecasted value
forecasted_value <- forecast_result$mean[1]

# Print the forecasted value
cat("Forecasted total_orders of july customers in August 2022 (20-8):", forecasted_value, "\n")


#visualize the forcast 
plot(forecast_result, main = "Forecasted Total Orders for August 2022")

#gauge the accuracy
accuracy(forecast_result)
#                        ME     RMSE      MAE       MPE     MAPE      MASE       ACF1
#Training set -3.760455e-12 1.714096 1.102488 -43.42696 65.70636 0.7494583 0.01989789

#2-1


# Arrange data by user_id and created_at
data <- data %>%
  arrange(user_id, created_at)

# Identify users with first purchase discount type 1
first_purchase_discount_1 <- data %>%
  group_by(user_id) %>%
  filter(row_number() == 1 & discount_type == 1) %>%
  select(user_id)

# Identify users with first purchase discount type NA
first_purchase_discount_na <- data %>%
  group_by(user_id) %>%
  filter(row_number() == 1 & is.na(discount_type)) %>%
  select(user_id)

# Calculate total basket purchase for each user
data <- data %>%
  group_by(user_id) %>%
  mutate(total_basket_purchase = sum(basket)) %>%
  ungroup()

# Print user IDs with their corresponding total basket purchase
first_purchase_discount_1_with_total <- data %>%
  filter(user_id %in% first_purchase_discount_1$user_id) %>%
  select(user_id, total_basket_purchase)
print(first_purchase_discount_1_with_total)

first_purchase_discount_na_with_total <- data %>%
  filter(user_id %in% first_purchase_discount_na$user_id) %>%
  select(user_id, total_basket_purchase)
print(first_purchase_discount_na_with_total)


# Calculate mean total basket for each group
mean_total_basket_discount_1 <- first_purchase_discount_1_with_total %>%
  summarise(mean_total_basket = mean(total_basket_purchase))

mean_total_basket_discount_na <- first_purchase_discount_na_with_total %>%
  summarise(mean_total_basket = mean(total_basket_purchase))

# Print mean total basket for each group
cat("Mean Total Basket for users with first purchase discount type 1:\n")
print(mean_total_basket_discount_1)

cat("Mean Total Basket for users with first purchase discount type NA:\n")
print(mean_total_basket_discount_na)

# Perform t-test
t_test_result <- t.test(
  first_purchase_discount_1_with_total$total_basket_purchase,
  first_purchase_discount_na_with_total$total_basket_purchase
)

# Print t-test results
cat("T-test Results:\n")
print(t_test_result)

# Assuming you have the 'data' dataframe with the specified columns

# Group the data by user_id
grouped_data <- data %>%
  group_by(user_id) %>%
  
  # Mutate the discount type on the first order for each user
  mutate(first_order_discount_type = ifelse(row_number() == 1, discount_type, NA)) %>%
  
  # Mutate the total number of orders each user makes
  mutate(total_orders_per_user = n()) %>%
  
  # Mutate the total basket amount for each user
  mutate(total_basket_per_user = sum(basket)) %>%
  
  # Remove duplicate rows (if needed)
  distinct()

filtered_data <- grouped_data %>%
  group_by(user_id) %>%
  filter(discount_type == 1 & created_at == min(created_at))

# Print the modified data
print(filtered_data)


# Assuming you have the 'filtered_users' dataframe with the specified columns

# Define the discount cost ranges for the groups
group_ranges <- c(0, 10000, 20000, 30000, 40000)

# Add a new column for the discount groups
grouped_users <- filtered_data %>%
  mutate(discount_group = cut(discount_cost, breaks = group_ranges, labels = c("Group 1", "Group 2", "Group 3", "Group 4")))

# Print the grouped users
print(grouped_users)

library(stats)

# Assuming you have the 'grouped_users' dataframe with the specified columns

# Perform ANOVA for total_basket_per_user
anova_basket <- aov(total_basket_per_user ~ discount_group, data = grouped_users)
summary(anova_basket)

tukey_orders <- TukeyHSD(anova_basket)
print(tukey_orders)

# Perform ANOVA for total_basket_per_user
anova_basket <- aov(total_basket_per_user ~ discount_group, data = grouped_users)

# Perform Tukey's HSD test for total_basket_per_user
tukey_basket <- TukeyHSD(anova_basket)
print(tukey_basket)


mean_total_basket_by_group <- grouped_users %>%
  group_by(discount_group) %>%
  summarize(mean_total_basket = mean(total_basket_per_user))

# Print the mean total basket for each discount group
print(mean_total_basket_by_group)


# Calculate mean total orders for each discount group
mean_total_orders_by_group <- grouped_users %>%
  group_by(discount_group) %>%
  summarize(mean_total_orders = mean(total_orders_per_user))

# Print the mean total orders for each discount group
print(mean_total_orders_by_group)


# Calculate total basket amount for each discount group
total_basket_by_group <- grouped_users %>%
  group_by(discount_group) %>%
  summarize(total_basket_amount = sum(total_basket_per_user))

# Print the total basket amount for each discount group
print(total_basket_by_group)


#3-1

# Convert 'created_at' column to Date
data$created_at <- as.Date(data$created_at, format = "%m/%d/%y")


# Calculate the number of occurrences for each user_id
user_occurrences <- data %>%
  group_by(user_id) %>%
  summarise(occurrences = n()) %>%
  ungroup()


# Calculate the mean occurrence of purchases for each customer
mean_occurrence <- mean(user_occurrences$occurrences)
max_occurrence <- max(user_occurrences$occurrences)


# Print the mean occurrence
cat("Mean Occurrence of Purchases:", mean_occurrence, "\n")


# Sort user_occurrences dataframe in descending order
user_occurrences_sorted <- user_occurrences %>%
  arrange(desc(occurrences))


# Select the top customers with the highest purchase frequency
top_customers <- head(user_occurrences_sorted, n = 10)  # Change 'n' to the desired number of top customers
# Print the top customers
print(top_customers)


# Create a histogram of purchase frequencies
purchase_frequency_hist <- ggplot(user_occurrences, aes(x = occurrences)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = "Purchase Frequency Distribution",
    x = "Purchase Frequency",
    y = "Number of Customers"
  ) +
  theme_minimal()
# Display the histogram
print(purchase_frequency_hist)


# Convert 'created_at' column to Date
data$created_at <- as.Date(data$created_at, format = "%m/%d/%y")


# Calculate time intervals between consecutive purchases for each user
data <- data %>% arrange(created_at) %>%
  arrange(user_id, created_at) %>%
  group_by(user_id) %>%
  mutate(interval_days = as.numeric(difftime(created_at, lag(created_at), units = "days")))


# Calculate the mean interval, total basket, and total purchase count for each user
mean_interval_per_user <- data %>%
  group_by(user_id) %>%
  summarise(mean_interval = mean(interval_days, na.rm = TRUE),
            total_basket = sum(basket, na.rm = TRUE),
            total_purchases = n())

# Print the mean interval for each user
print(mean_interval_per_user)


# Create a histogram of mean intervals
interval_histogram <- ggplot(mean_interval_per_user, aes(x = mean_interval)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Mean Intervals between Purchases",
    x = "Mean Interval between Purchases (days)",
    y = "Number of Users"
  ) +
  theme_minimal()

# Display the histogram
print(interval_histogram)


# Calculate the mean interval of purchases and total basket for each user
mean_interval_per_user <- data %>%
  group_by(user_id) %>%
  summarise(mean_interval = mean(interval_days, na.rm = TRUE),
            total_basket = sum(basket, na.rm = TRUE))

# Create a scatter plot of mean interval vs. total basket with a modern theme
interval_basket_plot <- ggplot(mean_interval_per_user, aes(x = mean_interval, y = total_basket)) +
  geom_point(color = "#86C5DA", alpha = 0.7) +
  labs(
    title = "Mean Interval vs. Total Basket",
    x = "Mean Interval between Purchases (days)",
    y = "Total Basket"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, color = "#2E3E4E"),
    axis.title = element_text(size = 14, color = "#2E3E4E"),
    axis.text = element_text(size = 12, color = "#2E3E4E"),
    axis.line = element_line(color = "#2E3E4E"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#F5F5F5"),
    panel.background = element_rect(fill = "#F5F5F5")
  )

# Display the scatter plot
print(interval_basket_plot)


repeat_customers <- data %>%
  group_by(user_id) %>%
  filter(n() > 1) %>%
  distinct(user_id) %>%
  n_distinct()
cat("RPR =",x = 4444/9985)
#RPR = 0.4450676


# calculating the PF 
cat("PF= " ,mean(mean_interval_per_user$mean_interval, na.rm = TRUE))
# PF=  2.910065


# calculating the TBP
mean_interval_per_user$mean_interval[mean_interval_per_user$mean_interval == "NaN"] = 0


# Select the columns for clustering
cluster_data <- mean_interval_per_user[, c("mean_interval", "total_basket")]

# Perform k-means clustering
num_clusters <- 4  # You can choose the number of clusters you want
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(cluster_data, centers = num_clusters)

# Add cluster labels to the original dataframe
mean_interval_per_user$cluster <- as.factor(kmeans_result$cluster)

# Visualize the clusters using scatter plots
library(ggplot2)
cluster_plot <- ggplot(mean_interval_per_user, aes(x = mean_interval, y = total_basket, color = cluster)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(
    title = "Clustering of Users based on Mean Interval and Total Basket",
    x = "Mean Interval between Purchases (days)",
    y = "Total Basket",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, color = "#2E3E4E"),
    axis.title = element_text(size = 14, color = "#2E3E4E"),
    axis.text = element_text(size = 12, color = "#2E3E4E"),
    axis.line = element_line(color = "#2E3E4E"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#F5F5F5"),
    panel.background = element_rect(fill = "#F5F5F5"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Display the cluster plot
print(cluster_plot)


# ...

# Perform k-means clustering
num_clusters <- 4  # You can choose the number of clusters you want
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(cluster_data, centers = num_clusters)

# Print the cluster centers
cat("Cluster Centers:\n")
print(kmeans_result$centers)

# Print the cluster assignments for each user
cluster_assignments <- kmeans_result$cluster
cluster_assignment_table <- table(cluster_assignments)
cat("\nCluster Assignments:\n")
print(cluster_assignment_table)


#2

# Convert 'created_at' column to Date
data$created_at <- as.Date(data$created_at, format = "%m/%d/%y")

# Calculate the month-year for each record
data$month_year <- format(data$created_at, "%Y-%m")

# Calculate the total basket amount minus the total discount for each month
monthly_totals <- data %>%
  group_by(month_year) %>%
  summarise(total_amount_minus_discount = sum(basket - discount_cost))

# Create a time series object
ts_data <- ts(monthly_totals$total_amount_minus_discount, frequency = 12)


library(ggplot2)

# Create a time series plot using ggplot
ts_plot <- ggplot(monthly_totals, aes(x = as.Date(paste0(month_year, "-01")), y = total_amount_minus_discount)) +
  geom_line() +
  labs(
    title = "Time Series of Total Amount Minus Discount",
    x = "Month",
    y = "Total Amount Minus Discount"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")  # Add this line to display month labels

# Display the time series plot
print(ts_plot)


monthly_metrics <- data %>% arrange(created_at) %>%
  mutate(month_year = format(created_at, "%Y-%m")) %>%
  group_by(month_year) %>%
  summarise(mean_interval = mean(interval_days, na.rm = TRUE),
            avg_basket_size = mean(basket, na.rm = TRUE),
            total_purchase_count = n())

# Print the calculated metrics for each month
print(monthly_metrics)

# Join the cluster column to the data dataframe
data <- left_join(data, mean_interval_per_user[, c("user_id", "cluster")], by = "user_id")

# Calculate Monthly Metrics for Each Cluster
cluster_monthly_metrics <- data %>%
  group_by(cluster, created_at) %>%
  summarise(mean_interval = mean(interval_days, na.rm = TRUE),
            total_basket = mean(total_basket_purchase, na.rm = TRUE)) 

# Create a Line Chart to Visualize Cluster Behavior Change
cluster_behaviour_change <- ggplot(cluster_monthly_metrics, aes(x = created_at)) +
  geom_line(aes(y = mean_interval, color = cluster), size = 1.5) +
  geom_line(aes(y = total_basket, color = cluster), size = 1.5, linetype = "dashed") +
  labs(
    title = "Cluster Behavior Change Over Time",
    x = "Month",
    y = "total_basket",
    color = "Cluster"
  ) +
  scale_color_discrete(name = "Cluster") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Display the Line Chart
print(cluster_behaviour_change)


#total_purchase_cluster
tpt <- data %>% group_by(data$cluster) %>% summarise(sum(basket))

#calculationg the users growth
data$join_month <- format(as.Date(data$initial_join), "%m")

# Count the number of unique users who joined in each month
monthly_user_counts <- aggregate(user_id ~ join_month, data = data, FUN = function(x) length(unique(x)))

print(monthly_user_counts)

ggplot(monthly_user_counts, aes(x = as.Date(paste(initial_join, "01", sep = "-")), y = user_id)) +
  geom_line() +
  labs(x = "Month", y = "Number of Users", title = "Monthly User Counts") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal()







