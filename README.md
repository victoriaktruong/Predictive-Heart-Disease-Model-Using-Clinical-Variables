# Predictive Heart Disease Model

## Background

Heart disease stands as a significant public health concern, ranking as the second leading cause of death in Canada. Statistics reveal hat approximately one in twelve Canadians aged twenty and above sufer from heart disease, underling the pervasive nature of this health issue. Predictive modeling for heart disease vital for preventative care; the ability to anticipate an individual's risk of heart disease holds the potential to revolutionize preventative care strategies. Timely identification and intervention can substantially reduce the likelihood of fatal outcomes, contributing to improved patient outcomes and overall well-being. 

## Dataset 

The dataset used for this project consists of clinical data collected from 1018 high-risk patients, encompassing 13 variables, each holding valuable insights into potential risk factors and clinical indicators. Originally sourced from the [UCI Machine Learning Repository][1], the dataset was made available on [Kaggle][2]. The roots of this dataset can be traced back to a research paper published in 1989 by [Detrano et al][3]. The dataset's incorporation of diverse clinical variables from this study not only enriches our analysis but also aligns with the continuous efforts to advance predictive healthcare models and enhance preventive care strategies.

The table below shows the variables used for analysis:

| Continuous          | Categorical                   |          | 
|--------------------|-----------------------------|----------| 
| Age (mean = 54.4) | Sex (M/F)                      | Resting ECG (normal/abnormal) | 
| Resting blood pressure | Fasting blood sugar (<120/>120) | Exercise induced angina (presence/absence) |
| Cholesterol        | Heart disease (presence/absence)  | ST slope (upsloping/downsloping/flat) |
| Maximum heart rate | Major vessels count (0-4)     | Chest pain type (typical angina/atypical angina/non-anginal pain/asymptomatic) |
| Old peak *          |  | |

\* ST depression induced by exercise relative to the rest, where ST relates to depression on the ECG plot. 

[1]: https://archive.ics.uci.edu/dataset/45/heart+disease
[2]: https://www.kaggle.com/datasets/johnsmith88/heart-disease-dataset
[3]: https://www.semanticscholar.org/paper/International-application-of-a-new-probability-for-Detrano-J%C3%A1nosi/a7d714f8f87bfc41351eb5ae1e5472f0ebbe0574

## Methodology
The methodology of this project involves a structured approach to analyzing heart disease using clinical variables. This involves the following key steps:

1. **Data Setup and Preparation:** Installing and loading essential R packages, such as Tidyverse, GGcorplot, Factoextra, Vegan, and Car. These packages facilitate data manipulation, visualization, and statistical analysis. The heart disease dataset is loaded in. Rows containing "NA" are removed.

2. **Exploratory Data Analysis (EDA):** EDA is a critical step in understanding the dataset's structure, distributions, and relationships between variables. Visualizations, such as box plots, help detect outliers and potential data issues. The correlation matrix visualized using GGcorrplot provides insights into variable relationships. Principle component analysis (PCA) helps identify underlying patterns in the dataset.

3. **Data Preprocessing:** Categorical variables are converted into factors, ensuring that they are correctly treated during modeling. Labels for various categorical variables, such as heart disease status, chest pain type, and more, are transformed to enhance interpretability.

4. **Investigations:** A series of investigations are performed to explore different aspects of heart disease relationships using statistical analysis and visualizations.

### Exploratory Data Analysis 
The EDA phase aims to reveal initial insights and patterns within the heart disease dataset.

1. **Box Plots for Outlier Detection:**  A set of box plots is generated for continuous variables (age, resting BP, max HR, cholesterol, oldpeak) to identify potential outliers. This step helps ensure data quality and informs subsequent analysis.

2. **Correlation Matrix Visualization:** The correlation matrix is computed to understand relationships between numerical variables. GGcorrplot is used to visualize this matrix, providing a clear view of variable correlations and potential multicollinearity.

3. **Principle Component Analysis (PCA):** PCA is employed to identify patterns and reduce dimensionality within the dataset. PCA reveals the most significant components that explain the variance in the data.

## Investigations
### Investigation 1: 

### Investigation 2:

### Investigation 3: (investigation 2 in ppt)

### Investigation 4: (investigation 1 in ppt)

## Conclusion
The combination of exploratory data analysis, PCA, and predictive modeling offers a comprehensive understanding of the relationships between clinical variables and heart disease.