library(readr)
TIFA_1999_2007_2 <- read_csv("TIFA 1999-2007-2.csv")
install.packages("dplyr")
library(dplyr)

# Filter out rows with values above 96 in the "Hours Driving" column
TIFA_1999_2007 <- TIFA_1999_2007_2 %>%
  filter(`Hours Driving` < 96)
# View the new table
View(TIFA_1999_2007)

# # Select the highest value in the "Hours Driving" column
# highest_hours_value <- TIFA_1999_2007 %>%
#   slice_max(order_by = `Hours Driving`, n = 1)
# 
# # View the highest value
# View(highest_hours_value)
library(ggplot2)

# Calculate the percentage frequencies
percentage_frequencies <- TIFA_1999_2007 %>%
  group_by(`Hours Driving`) %>%
  summarize(percentage = n() / nrow(TIFA_1999_2007) * 100)
# Convert 'Hours Driving' to a factor to ensure all values appear on the x-axis
percentage_frequencies$`Hours Driving` <- factor(percentage_frequencies$`Hours Driving`)
# Create a histogram
ggplot(percentage_frequencies, aes(x = `Hours Driving`, y = percentage)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(title = "Percentage of Accidents in Hours Driving",
       x = "Hours Driving",
       y = "Percentage") +
  theme_minimal()


# Create a table of counts
hours_driving_counts <- table(TIFA_1999_2007$`Hours Driving`)
# Convert the table to a data frame with adjusted column names
hours_driving_df <- data.frame(`Hours Driving` = as.numeric(names(hours_driving_counts)),
                               `Count of Cases` = as.numeric(hours_driving_counts))
# Set column names explicitly
colnames(hours_driving_df) <- c("Hours Driving", "Count of Cases")
#Change from data frame into Line-Point Graph: Distribution of Hours Driving 
ggplot(hours_driving_df, aes(x = factor(`Hours Driving`), y = `Count of Cases`, group = 1)) +
  geom_line(color = "green") +
  geom_point(color = "blue") +
  labs(title = "Distribution of Hours Driving",
       x = "Hours Driving",
       y = "Count of Cases") +
  theme_minimal()

#Create Pie Chart: Distribution of Light Condition when Hours Driving is 1
# Filter the data where "Hours Driving" equals "1" and "light condition" is not equal to "9"
subset_data <- TIFA_1999_2007 %>%
  filter(`Hours Driving` == 1, `light condition` != 9)
# Convert "light condition" to factor
subset_data$`light condition` <- factor(subset_data$`light condition`)
# Calculate the percentage frequencies for "light condition" within the subset
light_condition_percentages <- subset_data %>%
  group_by(`light condition`) %>%
  summarize(percentage = n() / nrow(subset_data) * 100)
light_condition_percentages
#Plot Distribution of Light Condition when Hours Driving is 1
ggplot(light_condition_percentages, aes(x = "", y = percentage, fill = `light condition`)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Light Condition when Hours Driving is 1",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = scales::hue_pal()(length(unique(light_condition_percentages$`light condition`)))) +
  theme_minimal()


# Create plot: Distribution of Atmospheric Condition when Hours Driving is 1
# Filter the data where "Hours Driving" equals "1" and "atmospheric condition" is not equal to "9"
subset_data_atmospheric <- TIFA_1999_2007 %>%
  filter(`Hours Driving` == 1, `atmospheric condition` != 9)
# Convert "atmospheric condition" to factor
subset_data_atmospheric$`atmospheric condition` <- factor(subset_data_atmospheric$`atmospheric condition`)
# Calculate the percentage frequencies for "atmospheric condition" within the subset
atmospheric_condition_percentages <- subset_data_atmospheric %>%
  group_by(`atmospheric condition`) %>%
  mutate(percentage = n() / nrow(subset_data_atmospheric) * 100)
# Create a box plot
ggplot(atmospheric_condition_percentages, aes(x = `atmospheric condition`, y = percentage, fill = `atmospheric condition`)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Distribution of Atmospheric Condition when Hours Driving is 1",
       x = "Atmospheric Condition",
       y = "Percentage") +
  scale_fill_manual(values = scales::hue_pal()(length(unique(atmospheric_condition_percentages$`atmospheric condition`)))) +
  theme_minimal()



#Create plot: Distribution of Accident Time when Hours Driving is 1
# Filter the data where "Hours Driving" equals "1"
subset_data_hour <- TIFA_1999_2007 %>%
  filter(`Hours Driving` == 1, hour <= 24)
# Calculate the percentage frequencies for "hour" within the subset
hour_percentages <- subset_data_hour %>%
  group_by(hour) %>%
  summarize(percentage = n() / nrow(subset_data_hour) * 100)
ggplot(hour_percentages, aes(x = hour, y = percentage, fill = percentage)) +
  geom_bar(stat = "identity", color = "black") +  # Adding black borders to the bars for better visibility
  labs(title = "Distribution of Accident Time when Hours Driving is 1",
       x = "Accident Time",
       y = "Percentage") +
  scale_fill_gradient(low = "palegreen", high = "seagreen4") +  # Adjust the colors as needed
  theme_minimal()


# Calculate mean and standard deviation for Hours Driving
mean_hours <- mean(TIFA_1999_2007$`Hours Driving`)
sd_hours <- sd(TIFA_1999_2007$`Hours Driving`)
# Print the results
cat("Mean Hours Driving:", mean_hours, "\n") #3.49018
cat("Standard Deviation Hours Driving:", sd_hours, "\n") #2.635365


#Create plot: Number of Cases in 1st Hour of Driving Each Year
TIFA_1999_2007$year <- as.factor(TIFA_1999_2007$year)
# Filter the data where "Hours Driving" equals "1"
subset_data_year <- TIFA_1999_2007 %>%
  filter(`Hours Driving` == 1)
# Calculate the count of cases for each year
cases_by_year <- subset_data_year %>%
  group_by(year) %>%
  summarize(count_of_cases = n())
# Create a bar plot
ggplot(cases_by_year, aes(x = year, y = count_of_cases, fill = year)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Number of Cases in 1st Hour of Driving Each Year",
       x = "Year",
       y = "Number of Cases") +
  scale_fill_manual(values = scales::hue_pal()(length(unique(cases_by_year$year)))) +
  theme_minimal()
# Perform ANOVA test
anova_result <- aov(count_of_cases ~ year, data = cases_by_year)

# Extract the p-value from the ANOVA test result
p_value_anova <- summary(anova_result)[["Pr(>F)"]][1]

# Print the p-value
cat("P-value (ANOVA):", p_value_anova, "\n")



#Create plot: Percentage of Cases in 1st Hour of Driving Each Year
TIFA_1999_2007$year <- as.factor(TIFA_1999_2007$year)
# Filter the data where "Hours Driving" equals "1"
subset_data_year <- TIFA_1999_2007 %>%
  filter(`Hours Driving` == 1)
# Calculate the count of cases for each year
cases_by_year <- subset_data_year %>%
  group_by(year) %>%
  summarize(count_of_cases = n())
# Calculate the total number of cases
total_cases <- sum(cases_by_year$count_of_cases)
# Calculate the percentage of cases for each year
cases_by_year <- cases_by_year %>%
  mutate(percentage_of_cases = (count_of_cases / total_cases) * 100)
# Create a line graph with percentage labels
ggplot(cases_by_year, aes(x = year, y = percentage_of_cases, color = year)) +
  geom_line() +
  geom_point() +  # Add points for each year
  geom_text(aes(label = sprintf("%.1f%%", percentage_of_cases), vjust = -0.5), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Percentage of Cases in 1st Hour of Driving Each Year",
       x = "Year",
       y = "Percentage of Cases") +
  scale_color_manual(values = scales::hue_pal()(length(unique(cases_by_year$year)))) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma_format(scale = 1),
                     limits = c(0, 15),  # Adjust the range as needed
                     breaks = seq(0, 20, by = 2))


                    
# Perform one-sample t-test
t_test_result <- t.test(TIFA_1999_2007$`Hours Driving`, mu = 4)

# Extract the p-value from the test result
p_value <- t_test_result$p.value

# Print the p-value
cat("P-value:", p_value, "\n")


















