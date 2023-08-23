#__________________________________________________________________________________________________#

#Heart Disease Project Master Script

#_________________________________________________________________________________________________#

#Install the packages below if not done so already
install.packages("tidyverse") #Tidyverse is a collection of common packages used for
                            #data analysis/manipulation, visualization (ggplot2, ddplyr, tidyr, etc)

install.packages("ggcorrplot") #Visualizes correlation matrices using ggplot2

install.packages("factoextra") #Helps make the PCA figures

install.packages("vegan") #Helps with multivariate data analysis (PCA)

install.packages("car") #Helps with hypothesis testing and checking for multi-collinearity

#Load the packages 
library(tidyverse) 
library(ggcorrplot)
library(factoextra)
library(vegan)
library(car)

#Add the data set to your working directory and then read and store it in an object called "heart_original"
heart_original <- read_csv("heart.csv")

#View the data set to make sure it loaded onto R correctly
View(heart_original)

#Rename some of the columns using the rename command to make it easier to work with
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


#Convert all the 0 values in the "thal" column to NA as they are NULL in the original data set
heart_original$thal[heart_original$thal == 0] <- NA

#Remove all of the NA rows in the dataset
heart_original <- na.omit(heart_original)


#Create another object called "heart_modified" which will be a copy of the object "heart_original"
#This is important because we will not make any modifications to "heart_original" so that we will always have the
#original data set available
#We will use "heart_modified" to make any changes to the data such as changing some variables into factors/discrete data
heart_modified <- heart_original
#__________________________________________________________________________________________________#


#________________________________________________________________________________________________#
#Creating box plots to visually check for outliers in our data set
#The %>% symbol represents a pipe operator which allows us to chain commands together

heart_modified %>% 
select(age, resting_BP, max_HR, cholesterol, oldpeak) %>% #Select all the continuous variables
boxplot(xlab = "Variables",
        names = c("age", "resting BP", "max HR", "cholesterol", "old peak"), #Renaming the variables 
        cex.lab = 1.4, #Changes the size of the x-axis label
        cex.axis = 1.2) #Changes the size of the axis ticks and tick labels 
#________________________________________________________________________________________________#


#________________________________________________________________________________________________#
#Saad's code for creating the correlation matrix for the data set and the PCA 
#Place the data set into the "cor" command and round by 1 decimal point.
#Place it all into another object called "cor_matrix"
cor_matrix = round(cor(heart_original), 1)

cor_matrix #Run this line of code to view the correlation matrix  

#To visualize the correlation matrix, use the ggcorrplot command and insert the 
#cor_matrix object as the argument
ggcorrplot(cor_matrix, lab = TRUE) +
  theme(axis.text.x = element_text(angle = 90 , vjust = 0.5)) #Change the x axis labels position so 
                                                                #they are rotated 90 degrees and centered
#________________________________________________________________________________________________#


#________________________________________________________________________________________________#
#PCA code

#Create an object called "pca" which will hold the results from the PCA 
pca.heart = prcomp(heart_original, #"prcomp" function conducts the PCA on the argument (the heart data set) 
            scale = TRUE, center = TRUE) #scale = TRUE scales the data so that it has no mean and unit variance before doing the PCA

#View the PCA results
pca.heart

#The summary function displays the standard deviation, proportion of variance and
#cumulative proportion of variance explained by each principle component
summary(pca.heart)

#View the loadings
pca.heart$rotation


#Create a line plot of the principle component scores using rule 1 
screeplot(pca.heart, 
        npcs = length(pca.heart$sdev), #The number of principal components to include on the plot 
        type = "line", #Make a line plot
        cex.lab = 1.2, #Change font size for labels
        cex.main = 1.5) #Change font size for title 

abline(1,0, col = "red") #Draws a straight with an intercept of 1 and a slope of 0


#Create a bi-plot of the principle component scores with the eigenvectors
fviz_pca_var(pca.heart,
            axes = c(1,2), #Use PC1 and PC2
            col.var = "contrib", # Color by contributions to the PC
            gradient.cols = c("lightblue4", "orange", "red"),
            repel = T,) + # Avoid text overlapping
labs(col = "Contribution") #Title of the legend


#Create a bi-plot of the principle component scores with only eigenvalues shown
fviz_pca_ind(pca.heart, 
            col.ind = "cos2", # Color by quality of representation
            gradient.cols = c("lightblue4", "orange", "red"),
            repel = T, # Avoid text overlapping
            label = "" ) + # Remove the labels for each data point
labs(col = "Contribution") #Title of the legend
#__________________________________________________________________________________________________#


#_________________________________________________________________________________________________#
#Emily's code for turning the categorical variables into factors and also subsetting the data

#Convert the "heart_disease" variable in the  data set into a factor that
#will have new the labels Absent and Present instead of 0 and 1
heart_modified$heart_disease <- factor(heart_modified$heart_disease, levels = c(0,1), labels = c("Absent", "Present"))


#Convert the "chest_pain_type" variable in the  data set into a factor that
#will have new the labels that represent that name of the type of chest pain 
#instead of 0-3
heart_modified$chest_pain_type <- factor(heart_modified$chest_pain_type, levels = c(0,1,2,3), labels = c("Typical angina","Atypical angina","Non-anginal pain","Asymptomatic"))


#Convert the "fasting_blood_sugar" variable into a factor with the  labels that match the original data set
heart_modified$fasting_blood_sugar <- factor(heart_modified$fasting_blood_sugar, levels = c(0,1), labels = c(">120", "<120"))


#Convert the "resting_ECG" variable into a factor with the  labels that match the original data set
heart_modified$resting_ECG <- factor(heart_modified$resting_ECG, levels = c(0,1,2), labels = c("normal", "ST wave abnormality", "left ventricular hypertrophy"))


#Convert the "exercise_induced_angina" variable into a factor with the  labels that match the original data set
heart_modified$exercise_induced_angina <- factor(heart_modified$exercise_induced_angina, levels = c(0,1), labels = c("no", "yes"))


#Convert the "slope" variable into a factor with the  labels that match the original data set
heart_modified$slope <- factor(heart_modified$slope, levels = c(0,1,2), labels = c("upsloping", "flat", "downsloping"))


#Convert the "thal" variable into a factor with the labels that match the original data set
heart_modified$thal <- factor(heart_modified$thal, levels = c(1,2,3), labels = c("fixed defect", "normal", "reversible defect"))


#Convert the "major_vessels_count" variable into a factor with the  labels that match the original data set
heart_modified$major_vessels_count <- as.factor(heart_modified$major_vessels_count)


#Convert the "sex" variable into a factor with the labels that match the original data set
heart_modified$sex <- factor(heart_modified$sex, levels = c(0,1), labels = c("female", "male"))
#__________________________________________________________________________________________________#

#--------------------------------------------------------------------------------------------------
#           Investigation 1: Exploring the relationship between the type of 
#                            chest pain and the presence of heart disease
#--------------------------------------------------------------------------------------------------

#_________________________________________________________________________________________________#

#Emily's code to subset and summarize the data so that we have the count for the presence and 
#absence of heart disease for all 4 types of chest pain.
#The %>% represents a pipe operator which allows us to chain commands together
heart_cp <- heart_modified %>% #Creating a new variable called heart_cp to store our subset data.
    select(chest_pain_type, heart_disease) %>% #Select the "chest_pain_type" and "heart_disease" variable
    group_by(chest_pain_type, heart_disease) %>% #Group them together
    summarise(tally = n()) #The summarise verb allows us to count the presence and absence of heart disease 
                            #for all 4 types of chest pain using the n() function into a object called tally

View(heart_cp) #View the "heart_cp" object to make sure there are no issues
#__________________________________________________________________________________________________#


#__________________________________________________________________________________________________#
#Saad's code for creating the cluster bar chart from the subset data 

#Select the "heart_cp" object which is the subset data to create the figure.
ggplot(heart_cp, aes(x = chest_pain_type, y = tally, fill = heart_disease)) +
geom_bar(position = "dodge", stat = "identity") + #position = "dodge" creates the cluster bar chart (places elements side by side for each group)
scale_fill_manual(values = c("lightblue3", "red4")) + #manually changing the colors
labs(title = "Heart Disease Frequency per Chest Pain Type", #Creating labels for the graph
    fill = "Heart disease") + #title of legend
xlab("Type of Chest Pain") + #x axis label
ylab("Frequency") + #y axis label
theme_classic() + #creates a white background and removes grid lines 
theme(text = element_text(size = 13), #changing the text size
        plot.title = element_text(hjust = 0.5), #centers the title 
        legend.text = element_text(size = 13), #changes the legend text size
        legend.title = element_text(size = 13), #changes the legend title size
        axis.text.x = element_text(size = 11), #changes the x-axis size
        axis.text.y = element_text(size = 11)) #changes the y-axis size
#__________________________________________________________________________________________________#


#__________________________________________________________________________________________________#
#Victoria's code for the chi-squared test of independence on the clustered bar chart
#Create a contingency table that shows the frequency distribution of chest_pain_type and
#presence/absence of heart disease
table_heart <- table(heart_modified$chest_pain_type, heart_modified$heart_disease)

#View the table 
print(table_heart)

#Perform the chi-squared test 
my_test <- chisq.test(table_heart)

#View the results to find the p-value, degrees of freedom and X^2 values
my_test
#__________________________________________________________________________________________________#

#--------------------------------------------------------------------------------------------------
#           Investigation 2: Predicting the presence of heart disease based 
#                            on max heart rate of patients
#--------------------------------------------------------------------------------------------------

#__________________________________________________________________________________________________#
#Emily's code for subsetting the data to be used in the logistic regression figure and analysis
heart_logistic <- heart_original %>%
    select(max_HR, heart_disease) %>% #Select "max_HR" and "heart_disease" variables
    group_by(max_HR, heart_disease) #Group the data together


#View the new object to see if it was made correctly
View(heart_logistic)
#__________________________________________________________________________________________________#


#__________________________________________________________________________________________________#
#Saad's code to create the logistic regression figure
ggplot(data = heart_logistic, aes(x = max_HR, y = heart_disease)) +
    geom_point() + #Create scatter point
    theme_classic() + #Creates a white background and removes grid lines
    geom_smooth(method = "glm", #Uses the Generalized Linear Model
                method.args = list(family = "quasibinomial"), color = "blue1") + # The probability distribution family is binomial
    ylab("Heart Disease") + #y-axis label
xlab("Max Heart Rate") + #x-axis label
labs(title = "Probability of Heart Disease based on Max Heart Rate") + #Figure title
theme(plot.title = element_text(hjust = 0.5), #Centers the title 
        legend.text = element_text(size = 14), #Changes the legend text size
        legend.title = element_text(size = 14), #Changes the legend title size
        axis.text.x = element_text(size = 12), #Changes the x-axis size
        axis.text.y = element_text(size = 12), #Changes the y-axis size
        axis.title.x = element_text(size = 14), #Changes the x-axis label size
        axis.title.y = element_text(size = 14)) #Changes the y-axis label size) 
#__________________________________________________________________________________________________#


#__________________________________________________________________________________________________#
#Victoria's code for the logistic regression on max heart rate and presence/absence of heart disease
#Use a logistic regression model
model_glm <- glm(heart_disease ~ max_HR, data=heart_original ,family=binomial)

#View the results from the logistic regression
summary(model_glm)

#Plot to check if assumptions are met
plot(model_glm) #Make sure to view all 4 diagnostic plots before moving on with the code
#__________________________________________________________________________________________________#

#--------------------------------------------------------------------------------------------------
#           Investigation 3: Predicting heart disease using all the  
#                           variables in the data set
#--------------------------------------------------------------------------------------------------

#__________________________________________________________________________________________________#
#Victoria's code for the logistic regression on all the variables in the data set and
#their relation to presence/absence of heart disease
#Use a logistic regression model

#The "." after ~ indicates to select all the variables in the data set
model_glm_all <- glm(heart_disease ~ . , data=heart_modified, family=binomial)

#View the results
summary(model_glm_all)

#Create an analysis of deviance table (Type III tests)
Anova(model_glm_all, type = 3)

#Plot to check if assumptions are met
plot(model_glm_all) #Make sure to view all 4 diagnostic plots before moving on with the code

#Check for multi-collinearity 
vif(model_glm_all) #VIF function measures multi-collinearity
                    #Adjusted GVIF values > 10 indicates multi-collinearity issues
#__________________________________________________________________________________________________#


#__________________________________________________________________________________________________#
#Emily's code for creating a new data frame to store the probabilities/fitted values
#from the "model_glm_all" object

#Create a new data frame called "predicted_data"
predicated_data <- data.frame(
    probability_of_heart_disease = model_glm_all$fitted.values, #create a new column to store the probabilities/fitted values
    heart_disease_status = heart_modified$heart_disease) #create another column to store the heart disease status of each patient

#Order the data frame from lowest to highest probability
predicated_data <- predicated_data[
    order(predicated_data$probability_of_heart_disease, decreasing = F),] 

#Create a new column which contains an integer in sequence for each row, effectively ranking the rows of the data frame 
#based on the sorted order. This will help visualize the relationship in the figure
predicated_data$rank <- 1:1018

#View the new data frame to make sure all the columns were created
View(predicated_data)
#__________________________________________________________________________________________________#


#__________________________________________________________________________________________________#
#Saad's code for creating the scatter plot corresponding to the "model_glm_all" object

#Select the "predicted_data" data frame for this figure
ggplot(data = predicated_data, aes(x=rank, y = probability_of_heart_disease)) +
    geom_point(aes(color= heart_disease_status), stroke = 1.4) + #Color by heart disease status. Stroke changes the thickness of the points
    xlab("Index (patients ranked based on their predicted probabilites)") + #X axis label
    ylab("Predicted Probability of Heart Disease") + #Y axis label
    scale_color_manual(
        name = "Heart Disease", #Title for the legend
        values = c("lightblue3", "darkred"), #Manually changing the color
        labels = c("Absent", "Present")) + #Legend labels
    theme_classic() + #change to classic theme
    theme(text = element_text(size = 13), #X and Y Label text size 
        legend.text = element_text(size = 15), #Legend text size
        legend.title = element_text(size = 15)) #Legend title text size
#__________________________________________________________________________________________________#

#--------------------------------------------------------------------------------------------------
#           Investigation 4: Exploring the Interaction between Heart Disease and Sex 
#                           on Maximum Heart Rate  
#--------------------------------------------------------------------------------------------------

#__________________________________________________________________________________________________#
#Victoria's code for the linear model to explore the interaction between heart disease and sex
#on max heart rate

model_lm <- lm(max_HR ~ heart_disease * sex, data = heart_modified)

#View the results
summary(model_lm)

#Plot to check if assumptions are met
plot(model_lm) #Make sure to view all 4 diagnostic plots before moving on with the code

#Check for multi-collinearity
vif(model_lm, type = "predictor") #VIF function measures multi-collinearity
                                    #Adjusted GVIF values > 10 indicates multi-collinearity issues
#__________________________________________________________________________________________________#


#__________________________________________________________________________________________________#
#Emily's code for subsetting the data that will be used to create the figure to visualize the 
#interaction between heart disease and sex on max heart rate

interaction_data <- heart_modified %>% #Make a new object called "interaction_data"
    group_by(heart_disease, sex) %>% #Group the data by heart disease status and sex
    summarise(mean_max_HR = mean(max_HR)) #Find the mean max heart rate for each group
#__________________________________________________________________________________________________#


#__________________________________________________________________________________________________#
#Saad's code for creating the figure to visualize the interaction between heart disease and sex
#on max heart rate

ggplot(interaction_data, aes(x = heart_disease, y = mean_max_HR, color = sex, group = sex, shape = sex, linetype = sex)) +
        geom_point(size = 3) + #Point size
    geom_line(size = 1) + #Line size
    scale_color_manual(values = c("blue1", "darkred")) + #Color of the lines
    scale_shape_manual(values = c(16, 17)) + #The shape for each sex
    scale_linetype_manual(values = c("solid", "solid")) + #Line type for each sex
    labs(x = "Heart Disease", #x-label
        y = "Max Heart Rate", #y-label
        color = "Sex", shape = "Sex", linetype = "Sex") +
    scale_x_discrete(expand = c(0, 0.3)) + #Adjust space between labels on X axis
    theme_classic() + #Change theme to classic
    theme(text = element_text(size = 13), #X and Y axis label text size
        legend.text = element_text(size = 14), #Legend text size
        legend.title = element_text(size = 14)) #Legend title size
#__________________________________________________________________________________________________#










