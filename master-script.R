# -----------------------------------------------------------------------------

# Predicting Heart Disease Using Clinical Variables

# -----------------------------------------------------------------------------
# Section 1: Data Setup and Preparation
# -----------------------------------------------------------------------------

# 1.1: Install the Required Packages

# tidyverse is a collection of packages commonly used for data analysis,
# manipulation, and visualization (e.g., ggplot2, ddplyr, tidyr)

install.packages("tidyverse")

# ggcorrplot visualizes correlation matrices using ggplot2
install.packages("ggcorrplot")

# factoextra is used to make PCA figures
install.packages("factoextra")

# vegan is used in multivariate data analysis (PCA)
install.packages("vegan")

# car (companion to applied regression) helps with hypothesis testing
# and checking for multi-collinearity
install.packages("car")


# 1.2: Load and Prepare Data

# Load the packages
library(tidyverse)
library(ggcorrplot)
library(factoextra)
library(vegan)
library(car)

# Add the dataset to your working directory, then read and save it within an
# object called "heart_original"
heart_original <- read_csv("heart.csv")

# View the dataset to make sure it loaded correctly
View(heart_original)

# Rename columns for better usability
heart_original <- rename(heart_original,
                         chest_pain_type = "cp",
                         max_HR = "thalach",
                         heart_disease = "target",
                         resting_BP = "trestbps",
                         fasting_blood_sugar = "fbs",
                         resting_ECG = "restecg",
                         exercise_induced_angina = "exang",
                         major_vessels_count = "ca",
                         cholesterol = "chol")

# Convert all 0 values in "thal" column to NA as
# these are NULL in the original dataset
heart_original$thal[heart_original$thal == 0] <- NA

# Remove all the NA rows in the dataset
heart_original <- na.omit(heart_original)

# Create object called "heart_modified" which will be a copy of the object
# "heart_original"
# Modifications will not be made to "heart_original", therefore having a copy
# ensures that we will always have the original dataset
heart_modified <- heart_original

# -----------------------------------------------------------------------------
# Section 2: Exploratory Data Analysis
# -----------------------------------------------------------------------------

# 2.1: Box Plots for Outlier Detection

# Create box plots to visually check for outliers in the dataset
heart_modified %>%
  select(age, resting_BP, max_HR, cholesterol, oldpeak) %>%
  boxplot(xlab = "Variables",
          names = c("age", "resting BP", "max HR", "cholesterol", "old peak"),
          cex.lab = 1.4,
          cex.axis = 1.2)


# 2.2 Correlation Matrix Visualization

# Create the correlation matrix for the data set and the PCA
cor_matrix <- round(cor(heart_original), 1)

# View the correlation matrix
cor_matrix

# EXPLAIN WHAT IS HAPPENING HEREEEEEE???????
ggcorrplot(cor_matrix, lab = TRUE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# 2.3: Principle Component Analysis (PCA)

# Create an object called "pca_heart" to hold PCA results
pca_heart <- prcomp(heart_original,
                    scale = TRUE, center = TRUE)

# View the PCA results
pca_heart

# Display summary statistics for PCA
summary(pca_heart)

# View the loadings
pca_heart$rotation


# 2.4: Scree Plot

# Create a line plot of the principle component scores using rule 1
screeplot(pca_heart,
          npcs = length(pca_heart$sdev),
          type = "line",
          cex.lab = 1.2,
          cex.main = 1.5)
abline(1, 0, col = "red")


# 2.5: Bi-Plots

# Create a bi-plot of the principle component scores with the eigenvectors
fviz_pca_var(pca_heart,
             axes = c(1, 2),
             col.var = "contrib",
             gradient.cols = c("lightblue4", "orange", "red"),
             repel = TRUE, ) +
  labs(col = "Contribution")

# Create a bi-plot of the principle component scores with only eigenvalues shown
fviz_pca_ind(pca_heart,
             col.ind = "cos2",
             gradient.cols = c("lightblue4", "orange", "red"),
             repel = TRUE,
             label = "") +
  labs(col = "Contribution")

# -----------------------------------------------------------------------------
# Section 3: Data Preprocessing
# -----------------------------------------------------------------------------

# 3.1: Convert Categorical Variables into Factors

# Convert the "heart_disease" variable in the dataset into a factor with
# updated labels "Absent" and "Present" in place of 0 and 1
heart_modified$heart_disease <-
  factor(heart_modified$heart_disease,
         levels = c(0, 1), labels = c("Absent", "Present"))

# Convert the "chest_pain_type" variable in the dataset into a factor with
# descriptive labels representing the types of chest pain, replacing the
# numeric values 0-3
heart_modified$chest_pain_type <-
  factor(heart_modified$chest_pain_type,
         levels = c(0, 1, 2, 3), labels = c("Typical angina",
                                            "Atypical angina",
                                            "Non-anginal pain",
                                            "Asymptomatic"))

# Convert the remaining variables into factors, retaining labels consistent
# with the original dataset
heart_modified$fasting_blood_sugar <-
  factor(heart_modified$fasting_blood_sugar,
         levels = c(0, 1), labels = c(">120", "<120"))

heart_modified$resting_ECG <-
  factor(heart_modified$resting_ECG,
         levels = c(0, 1, 2), labels = c("normal",
                                         "ST wave abnormality",
                                         "left ventricular hypertrophy"))

heart_modified$exercise_induced_angina <-
  factor(heart_modified$exercise_induced_angina,
         levels = c(0, 1), labels = c("no", "yes"))

heart_modified$slope <-
  factor(heart_modified$slope,
         levels = c(0, 1, 2), labels = c("upsloping", "flat", "downsloping"))

heart_modified$thal <-
  factor(heart_modified$thal,
         levels = c(1, 2, 3), labels = c("fixed defect",
                                         "normal",
                                         "reversible defect"))

heart_modified$major_vessels_count <-
  as.factor(heart_modified$major_vessels_count)

heart_modified$sex <-
  factor(heart_modified$sex,
         levels = c(0, 1), labels = c("female", "male"))

# -----------------------------------------------------------------------------




# -----------------------------------------------------------------------------
# Investigation 1: Exploring the Relationship between Chest Pain and
# Heart Disease
# -----------------------------------------------------------------------------

# 1.1: Data Subset and Summarization

# Subset and summarize the data to obtain counts for occurences and
# non-occurrences of heart disease across all four types of chest pain
heart_cp <- heart_modified %>%
  select(chest_pain_type, heart_disease) %>%
  group_by(chest_pain_type, heart_disease) %>%
  summarise(tally = n())

# View the "heart_cp" object
View(heart_cp)


# 1.2: Clustered Bar Chart

# Create a clustered bar chart from the subsetted data
ggplot(heart_cp, aes(x = chest_pain_type, y = tally, fill = heart_disease)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("lightblue3", "red4")) +
  labs(title = "Heart Disease Frequency per Chest Pain Type",
       fill = "Heart disease") +
  xlab("Type of Chest Pain") +
  ylab("Frequency") +
  theme_classic() +
  theme(text = element_text(size = 13),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))


# 1.3: Chi-Squared Test of Independence

# Perform a chi-squared test of independence on the clustered bar chart results

# Create a contingency table that shows the frequency distribution of chest
# pain type and presence/absence of heart disease
table_heart <- table(heart_modified$chest_pain_type,
                     heart_modified$heart_disease)

# View the table
print(table_heart)

# Perform the chi-squared test
my_test <- chisq.test(table_heart)

# View the results to find the p-value, degrees of freedom and X^2 values
my_test

# -----------------------------------------------------------------------------
# Investigation 2: Predicting Heart Disease Based on Maximum Heart Rate
# -----------------------------------------------------------------------------

# 2.1: Data Preparation for Logistic Regression

# Subset the data to include only "max_HR" and "heart_disease" variables
heart_logistic <- heart_original %>%
  select(max_HR, heart_disease) %>%
  group_by(max_HR, heart_disease)

# View the modified dataset
View(heart_logistic)


# 2.2: Logistic Regression Figure

# Create a logistic regression figure to visualize the relationship between
# max heart rate and heart disease
ggplot(data = heart_logistic, aes(x = max_HR, y = heart_disease)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "glm",
              method.args = list(family = "quasibinomial"), color = "blue1") +
  ylab("Heart Disease") +
  xlab("Max Heart Rate") +
  labs(title = "Probability of Heart Disease based on Max Heart Rate") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


# 2.3: Logistic Regression Analysis

# Perform logistic regression on the relationship between max heart rate and
# heart disease

# Fit a logistic regression model
model_glm <- glm(heart_disease ~ max_HR,
                 data = heart_original,
                 family = binomial)

# View the results of the logistic regression
summary(model_glm)


# 2.4: Diagnostic Plot to Check Assumptions

# Make sure to view all 4 diagnostic plots before proceeding on
# with the analysis
plot(model_glm)

# -----------------------------------------------------------------------------
# Investigation 3: Predicting Heart Disease Using All Variables
# -----------------------------------------------------------------------------

# 3.1: Logistic Regression Analysis

# Perform logistic regression using all variables in the dataset to predict
# the presence/absence of heart disease

# Fit a logistic regression model
model_glm_all <- glm(heart_disease ~ .,
                     data = heart_modified,
                     family = binomial)

# View the results
summary(model_glm_all)

# Create an analysis of deviance table (Type III tests)
Anova(model_glm_all, type = 3)


# 3.2: Diagnostic Plot to Check Assumptions

# Ensure to view all 4 diagnostic plots before proceeding
plot(model_glm_all)

# Check for multi-collinearity (VIF function measures multi-collinearity)
# Adjusted GVIF values > 10 indicates multi-collinearity issues
vif(model_glm_all)


# 3.3: Predicted Probability Analysis

# Create a new data frame to store the predicted probabilities
# and heart disease status

# Create a new data frame called "predicted_data"
predicated_data <-
  data.frame(probability_of_heart_disease = model_glm_all$fitted.values,
             heart_disease_status = heart_modified$heart_disease)

# Order the data frame from lowest to highest probability
predicated_data <-
  predicated_data[order(predicated_data$probability_of_heart_disease,
                        decreasing = FALSE), ]

# Create a new column for ranking based on sorted order
predicated_data$rank <- 1:1018

# View the new data frame to verify columns
View(predicated_data)


# 3.4: Scatter Plot Visualization

# Create a scatter plot to visualize the predicted probabilities
# based on ranking

#Select the "predicted_data" data frame for this figure
ggplot(data = predicated_data,
       aes(x = rank, y = probability_of_heart_disease)) +
  geom_point(aes(color = heart_disease_status), stroke = 1.4) +
  xlab("Index (patients ranked based on their predicted probabilites)") +
  ylab("Predicted Probability of Heart Disease") +
  scale_color_manual(name = "Heart Disease",
                     values = c("lightblue3", "darkred"),
                     labels = c("Absent", "Present")) +
  theme_classic() +
  theme(text = element_text(size = 13),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))

# -----------------------------------------------------------------------------
# Investigation 4: Exploring the Interaction between Heart Disease
# and Sex on Maximum Heart Rate
# -----------------------------------------------------------------------------

# 4.1: Linear Model for Interaction Analysis

# Perform a linear model exploring the interaction between heart disease
# and sex on max heart rate
model_lm <- lm(max_HR ~ heart_disease * sex, data = heart_modified)

# View the results
summary(model_lm)


# 4.2: Diagnostic Plot to Check Assumptions

# Ensure to sure to view all 4 diagnostic plots before proceeding
plot(model_lm)

# Check for multi-collinearity
# Adjusted GVIF values > 10 indicates multi-collinearity issues
vif(model_lm, type = "predictor")


# 4.3: Data Preparation for Interaction Figure

# Subset the data for creating the figure to visualize the interaction between
# heart disease and sex on max heart rate
interaction_data <- heart_modified %>%
  group_by(heart_disease, sex) %>%
  summarise(mean_max_HR = mean(max_HR))


# 4.4: Interaction Visualization

# Create a figure to visually explore the interaction between heart disease
# and sex on max heart rate
ggplot(interaction_data, aes(x = heart_disease, y = mean_max_HR, color = sex,
                             group = sex, shape = sex, linetype = sex)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_color_manual(values = c("blue1", "darkred")) +
  scale_shape_manual(values = c(16, 17)) +
  scale_linetype_manual(values = c("solid", "solid")) +
  labs(x = "Heart Disease",
       y = "Max Heart Rate",
       color = "Sex", shape = "Sex", linetype = "Sex") +
  scale_x_discrete(expand = c(0, 0.3)) +
  theme_classic() +
  theme(text = element_text(size = 13),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

# -----------------------------------------------------------------------------

# This concludes the series of investigations on heart disease analysis.
# Thank you for exploring the data and relationships!

# -----------------------------------------------------------------------------