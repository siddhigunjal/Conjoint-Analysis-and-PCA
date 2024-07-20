# Conjoint-Analysis-and-PCA
#Question 1:
# Loading the necessary packages for performing conjoint analysis
library(conjoint)   # Package for conducting conjoint analysis
library(readxl)     # Package for reading Excel files
library(dplyr)      # Package for data manipulation
library(data.table) # Package for efficient data handling
library(ggplot2)    # Package for data visualization
set.seed(40416375)  # Setting a seed for reproducibility


# Setting attributes and their corresponding levels in a list
my_attribute.level <- list(
  envfriend = c("0%", "30%", "50%"),                 # Describes the level of environmental friendliness
  deltime = c("14 days", "21 days", "30 days"),      # Describes the delivery time options
  servleve = c("5yw", "5yw&fm", "5ywmins&up"),    # Indicates different service levels
  price = c("1000 GBP", "1200 GBP", "1500 GBP"),     # Represents price levels in GBP
  Quality_of_Materialmate = c("Market average", "higher than market average"),          # Indicates the quality of material
  markprof = c("Not very proficient and poor communication", "Very proficient and have good communication skills")            # Describes marketing proficiency
)


# Generating the fractional factorial frame
my_experiment <- expand.grid(my_attribute.level)  # Creating all possible combinations of attribute levels
my_factframe <- caFactorialDesign(data = my_experiment, type = "fractional", cards = 25, seed = 40416375)  # Creating the fractional factorial design with 25 cards
my_factframe  # Displaying the fractional factorial frame

# Checking for correlation within the fractional factorial frame
print(cor(caEncodedDesign(my_factframe)))  # Calculating and printing the correlation matrix for my_factframe

# Generating different fractional factorial frames with varying numbers of cards
my_factframe1 <- caFactorialDesign(data = my_experiment, type = "fractional", cards = 20, seed = 40416375)
print(cor(caEncodedDesign(my_factframe1)))

my_factframe2 <- caFactorialDesign(data = my_experiment, type = "fractional", cards = 18, seed = 40416375)
print(cor(caEncodedDesign(my_factframe2)))

my_factframe3 <- caFactorialDesign(data = my_experiment, type = "fractional", cards = 13, seed = 40416375)
print(cor(caEncodedDesign(my_factframe3)))

#---------------------------------------------------------------
# Conjoint Analysis

# Reading the preference dataset
my_pref <- read_xlsx("C:/Users/DELL/OneDrive/Desktop/Marketing/Assignment 2/Conjoint Prefernces.xlsx")  # Reading preference data from an Excel file
my_pref <- my_pref[, -1]  # Removing the first column (assuming it's an index or ID column)

# Reading the product profiles dataset
my_frame <- read.csv("C:/Users/DELL/OneDrive/Desktop/Marketing/Assignment 2/Product Profiles.csv")  # Reading product profiles data from a CSV file
my_frame <- my_frame[, -c(1, 8:10)]  # Removing columns 1, 8, 9, and 10 (assuming they are not needed for analysis)


# Transforming attribute levels in the frame
# Adjusting attribute levels to more understandable labels

my_frame <- my_frame %>%
  dplyr::mutate(
    Environmental.friendliness = case_when(
      Environmental.friendliness == 1 ~ "0%",  # If environmental friendliness is rated as 1, set it to "0%"
      Environmental.friendliness == 2 ~ "30%",  # If environmental friendliness is rated as 2, set it to "30%"
      Environmental.friendliness == 3 ~ "50%"),  # If environmental friendliness is rated as 3, set it to "50%"
    
    Delivery.time = case_when(
      Delivery.time == 1 ~ "14 days",  # If delivery time is rated as 1, set it to "14 days"
      Delivery.time == 2 ~ "21 days",  # If delivery time is rated as 2, set it to "21 days"
      Delivery.time == 3 ~ "30 days"),  # If delivery time is rated as 3, set it to "30 days"
    
    Service.Level = case_when(
      Service.Level == 1 ~ "5yw",  # If service level is rated as 1, set it to "5yw"
      Service.Level == 2 ~ "5yw&fm",  # If service level is rated as 2, set it to "5yw&fm"
      Service.Level == 3 ~ "5ywmins&up"),  # If service level is rated as 3, set it to "5ywmins&up"
    
    Price = case_when(
      Price == 1 ~ "1000 GBP",  # If price is rated as 1, set it to "1000 GBP"
      Price == 2 ~ "1200 GBP",  # If price is rated as 2, set it to "1200 GBP"
      Price == 3 ~ "1500 GBP"),  # If price is rated as 3, set it to "1500 GBP"
    
    Quality.of.material = case_when(
      Quality.of.material == 1 ~ "Market average",  # If quality of material is rated as 1, set it to "Market average"
      Quality.of.material == 2 ~ "higher than market average"),  # If quality of material is rated as 2, set it to "higher than market average"
    
    Marketing.Proficiency = case_when(
      Marketing.Proficiency == 1 ~ "Not very proficient and poor communication",  # If marketing proficiency is rated as 1, set it to "Not very proficient and poor communication"
      Marketing.Proficiency == 2 ~ "Very proficient and have good communication skills")  # If marketing proficiency is rated as 2, set it to "Very proficient and have good communication skills"
  )


# Creating a dataframe for attribute levels
# Unlisting attribute levels and creating a dataframe
my_attribute_vec <- data.frame(unlist(my_attribute.level, use.names = FALSE))

# Adding column names to the dataframe
colnames(my_attribute_vec) <- c("levels")

# Initialize an empty dataframe to store part-worth utilities
my_part_worths <- NULL  
my_part_worths


# Loop over each column in the preference data
for (i in 1:ncol(my_pref)) {
  temp <- caPartUtilities(my_pref[, i], my_frame, my_attribute_vec)  # Calculate part-worth utilities for the current respondent
  
  # Baseline Cases: Environmental_Friendliness - "0%", Delivery_Time - "14 days", Service_Lvl - "5 yw", Price - "1000 GBP", Qul_of_Mat - "MA", Mkt_Prof - "NP&Poorcc"
  Base.Env <- temp[, "0%"]
  Base.Del <- temp[, "14 days"]
  Base.Sev <- temp[, "5yw"]
  Base.GBP <- temp[, "1000 GBP"]
  Base.Qul <- temp[, "Market average"]
  Base.Mkt <- temp[, "Not very proficient and poor communication"]
  
  temp[, "intercept"] <- temp[, "intercept"] - Base.Env - Base.Del - Base.Sev - 
    Base.GBP - Base.Qul - Base.Mkt  # Adjusting intercept for baseline cases
  
  # Adjusting part-worths for each attribute level based on baseline cases
  # Environmental_Friendliness
  L1 <- length(my_attribute.level$envfriend) + 1
  for (j in 2:L1) {
    temp[, j] <- temp[, j] - Base.Env
  }
  # Delivery_Time
  L2 <- length(my_attribute.level$deltime) + L1
  for (k in (L1 + 1):L2) {
    temp[, k] <- temp[, k] - Base.Del
  }
  # Service_Lvl
  L3 <- length(my_attribute.level$servleve) + L2
  for (l in (L2 + 1):L3) {
    temp[, l] <- temp[, l] - Base.Sev
  }
  # Price
  L4 <- length(my_attribute.level$price) + L3
  for (m in (L3 + 1):L4) {
    temp[, m] <- temp[, m] - Base.GBP
  }
  # Quality_of_Material
  L5 <- length(my_attribute.level$Quality_of_Materialmate) + L4
  for (n in (L4 + 1):L5) {
    temp[, n] <- temp[, n] - Base.Qul
  }
  # Marketing_Proficiency
  L6 <- length(my_attribute.level$markprof) + L5
  for (o in (L5 + 1):L6) {
    temp[, o] <- temp[, o] - Base.Mkt
  }
  
  my_part_worths <- rbind(my_part_worths, temp)  # Append part-worths to the main dataframe
}

rownames(my_part_worths) <- colnames(my_pref)  # Assign column names of preference data as row names of part-worths dataframe


# Customer Segmentation using Hierarchical Clustering
# Setting seed for reproducibility
set.seed(40416375)

# Performing Hierarchical Clustering
my_hc <- hclust(dist(my_part_worths), method = "single")

# Elbow Plot: Determining Optimal Number of Clusters
x <- 1:5
y <- sort(my_hc$height, decreasing = TRUE)[1:5]
# Adjusting margins
par(mar = c(5, 5, 2, 2))  # Change the margin values as needed
# Plotting the elbow plot
plot(x, y, type = "o", col = "darkgreen", xlab = "Number of Clusters", ylab = "Height", main = "Hierarchical Cluster Elbow Plot")
axis(1, at = seq(0, 10, by = 1))

# Plotting the Dendrogram of Hierarchical Clusters
plot(my_hc)
rect.hclust(my_hc, k = 4, border = 10:60)

# Cutting the Dendrogram to Obtain Clusters
my_hc_out <- cutree(my_hc, k = 4)

# Adding Cluster Labels to Part-worths Dataframe
my_part_worths <- cbind(my_part_worths, Cluster = my_hc_out)

# Writing Part-worths Dataframe to a CSV File
write.csv(my_part_worths, file = "C:/Users/DELL/OneDrive/Desktop/Marketing/Assignment 2/worths.csv", row.names = TRUE)


#---------------------------------------------------------------
# Performing Principal Component Analysis (PCA)

# Setting seed for reproducibility
set.seed(40416375)

# Reading the customer perception data
my_perception_data <- read.csv(file.choose("C:/Users/DELL/OneDrive/Desktop/Marketing/Assignment 2/PCA Data.csv"))

# Applying PCA
my_pca <- prcomp(my_perception_data[, 2:length(my_perception_data)], retx = TRUE, scale = TRUE)

# Creating perceptual mapping attributes
my_attribute <- as.data.table(colnames(my_perception_data[, 2:length(my_perception_data)]))
setnames(my_attribute, 1, "Attribute")

# Calculating factor loadings for Factor 1 and Factor 2
my_factor1 <- my_pca$rotation[, 1] * my_pca$sdev[1]
my_factor2 <- my_pca$rotation[, 2] * my_pca$sdev[2]

# Creating a dataframe for perceptual mapping attributes with factor loadings
my_path <- rep(1, nrow(my_attribute))
my_pca_factors <- subset(cbind(my_attribute, my_factor1, my_factor2, my_path), select = c(Attribute, my_factor1, my_factor2, my_path))
my_pca_origin <- cbind(my_attribute, my_factor1 = rep(0, nrow(my_attribute)), my_factor2 = rep(0, nrow(my_attribute)), my_path = rep(0, nrow(my_attribute)))
my_pca_attributes <- rbind(my_pca_factors, my_pca_origin)
my_pca_attributes


# Writing perceptual mapping attributes to a CSV file
write.csv(my_pca_attributes, file = "C:/Users/DELL/OneDrive/Desktop/Marketing/Assignment 2/pca_attributes.csv", row.names = FALSE)

# Perceptual Mapping
my_score1 <- (my_pca$x[, 1] / apply(abs(my_pca$x), 2, max)[1])
my_score1
my_score2 <- (my_pca$x[, 2] / apply(abs(my_pca$x), 2, max)[2])
my_score2

# Creating a dataframe for perceptual mapping scores
my_pca_scores <- subset(cbind(my_perception_data, my_score1, my_score2), select = c(Model, my_score1, my_score2))
my_pca_scores 

# Writing perceptual mapping scores to a CSV file
write.csv(my_pca_scores, file = "C:/Users/DELL/OneDrive/Desktop/Marketing/Assignment 2/pcascores.csv", row.names = FALSE)


# Feature Importance

# Calculating attribute importance based on the absolute sum of loadings for the first two principal components
#Loading Factor
my_pca$rotation
my_attribute_importance <- data.frame(Attribute = colnames(my_perception_data[, 2:length(my_perception_data)]), Importance = rowSums(abs(my_pca$rotation[, 1:2])))
my_attribute_importance <- my_attribute_importance[order(-my_attribute_importance$Importance), ]  # Ordering by importance
my_attribute_importance

# Plotting feature importance
ggplot(my_attribute_importance, aes(x = reorder(Attribute, Importance), y = Importance)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Features Importance in PCA", x = "Feature", y = "Total Loading Value")

#Singular Values
my_pca$sdev

# Calculating Proportion of Variance Explained (PVE)
my_PVE <- (my_pca$sdev^2) / sum(my_pca$sdev^2)
my_PVE


# Calculating cumulative proportion of variance explained
my_cumulative_PVE <- cumsum(my_PVE)
my_cumulative_PVE

# Scree-plot data
my_data_scree <- data.frame(PCA = 1:length(my_PVE), PVE.Explained = my_PVE, Cumulative_PVE.Explained = my_cumulative_PVE)
my_data_scree

# Plotting the Scree-plot
ggplot(my_data_scree, aes(x = PCA, y = PVE.Explained)) +
  geom_point(color = "pink", size = 3) + 
  geom_line(color = "pink", size = 1.25) +
  labs(title = "Proportion of Variance Explained",
       x = "Principal Component",
       y = "Proportion of Variance Explained")


