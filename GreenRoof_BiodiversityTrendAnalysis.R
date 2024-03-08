######R script that includes data preparation, linear regression analysis for both green roof 
###### publications and biodiversity studies, and plotting the trends with ggplot2. 
#######This script merges all the analysis steps discussed earlier into one comprehensive R code.

###This script starts by loading the necessary libraries and defining the dataset. 
###It then prepares the data for plotting, creates a trend plot using ggplot2, 
###and performs linear regression analyses for both the green roof publications overall and 
###the subset of studies focusing on biodiversity. The summary() function is called for 
###each model to display the regression analysis results, including the slope coefficient 
###and its statistical significance.

###Dependencies-=Ensure you have the ggplot2 and reshape2 packages installed in your R environment before running this script. If not, you can install them using install.packages("ggplot2") and install.packages("reshape2").

# Load necessary libraries
library(ggplot2)
library(reshape2)

# Define the dataset
years <- 2003:2023
green_roof_publications <- c(19, 31, 28, 35, 48, 50, 69, 115, 105, 167, 199, 231, 285, 334, 363, 359, 446, 471, 552, 548, 496)
green_roof_biodiversity_studies <- c(1, 2, 1, 1, 0, 3, 1, 7, 10, 8, 13, 13, 25, 21, 24, 37, 27, 22, 39, 51, 44)

# Prepare data for plotting
data <- data.frame(years, green_roof_publications, green_roof_biodiversity_studies)
data_long <- melt(data, id.vars = "years", variable.name = "Category", value.name = "Publications")

# Plotting trends
ggplot(data_long, aes(x = years, y = Publications, color = Category)) +
  geom_line(aes(group = Category), size = 1) +
  geom_point() +
  scale_x_continuous(breaks = years, labels = as.character(years)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Green Roof Publications vs. Green Roof Biodiversity Studies (2003-2023)",
       x = "Year", y = "Record Count", color = "Category")

# Linear regression analysis for biodiversity studies
model_biodiversity <- lm(green_roof_biodiversity_studies ~ years)
summary(model_biodiversity)

# Linear regression analysis for all green roof publications
model_publications <- lm(green_roof_publications ~ years)
summary(model_publications)

# Narrowing down the dataset to the years 2019 to 2023 for biodiversity studies
years_recent <- 2019:2023
green_roof_biodiversity_studies_recent <- tail(green_roof_biodiversity_studies, 5)

# Preparing the data for linear regression for the recent period
data_recent <- data.frame(years = years_recent, green_roof_biodiversity_studies_recent)
X_recent <- model.matrix(~ years, data_recent)

# Fitting the linear regression model for the recent period of biodiversity studies
model_biodiversity_recent <- lm(green_roof_biodiversity_studies_recent ~ years, data = data_recent)
summary(model_biodiversity_recent)

######################################

## attempting to plot the trends of the full period over the recent period but 
## thats wrong just for fun


# Data preparation for the full period
data_full <- data.frame(years = years, green_roof_biodiversity_studies)
data_full_long <- melt(data_full, id.vars = "years", variable.name = "Category", value.name = "Publications")

# Plotting trend for the full period (2003-2023)
ggplot(data_full_long, aes(x = years, y = Publications, color = Category)) +
  geom_line(aes(group = Category), size = 1) +
  geom_point() +
  scale_x_continuous(breaks = years, labels = as.character(years)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Green Roof Biodiversity Studies Trend (2003-2023)",
       x = "Year", y = "Record Count", color = "Category")

# Data preparation for the recent period (2019-2023)
data_recent <- data.frame(years = years_recent, green_roof_biodiversity_studies = green_roof_biodiversity_studies_recent)
data_recent_long <- melt(data_recent, id.vars = "years", variable.name = "Category", value.name = "Publications")

# Plotting trend for the recent period (2019-2023)
ggplot(data_recent_long, aes(x = years, y = Publications, color = Category)) +
  geom_line(aes(group = Category), size = 1) +
  geom_point() +
  scale_x_continuous(breaks = years_recent, labels = as.character(years_recent)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Green Roof Biodiversity Studies Trend (2019-2023)",
       x = "Year", y = "Record Count", color = "Category")

