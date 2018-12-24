# Employee HR attrition (IBM dataset) 
Read Me
1. The dataset is available as attrition.csv. (To be downloaded by students from kaggle.com). URL: https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset 


Scope: How does Attrition affect companies? and how does HR Analytics help in analyzing attrition? A major problem in high employee attrition is its cost to an organization. Job postings, hiring processes, paperwork and new hire training are some of the common expenses of losing employees and replacing them. 


Objective: To investigate how the company objective factors influence in attrition of employees, and what kind of working environment is most likely to cause attrition


Business Problem Statement:Uncover the factors that lead to employee attrition and explore important questions such as ‘show me a breakdown of distance from home by job role and attrition’ or ‘compare average monthly income by education and attrition’. This is a fictional data set created by IBM data scientists.

Analytics Approach
I shall be looking at all variables through some plots and infer about it in my exploratory analysis. And through my exploration I shall try to identify the Variables that tend to have an impact in the attrition of the most experienced and talented employees and try to fit a classification models and use it to test hypotheses and draw inferences.
The problem will be explored in following stages:
1.	Hypothesis Generation – understanding the problem statement better by working out possible factors that can have impact on the outcome
2.	Data Exploration (EDA) – looking at categorical and continuous variables summaries and making inferences about the data.
3.	Data Cleaning – imputing missing values in the data and checking for outliers and eliminating insignificant Variables form data set, scaling.
4.	Data Splitting – Randomly splitting the data set into train (80%) and test (20%) data sets. 
5.	Feature Engineering – modifying existing variables and creating new ones for analysis.
6.	Model Building – making predictive models on the train data and testing it.
7.	Champion Model Selection – after building multiple predictive models, the model with high accuracy is selected as Champion model

KPI: 
The Key Performance Indicator in this dataset would be the employee turnover rate which is calculated as the number of employees who left the organization for any reason divided by the average employee headcount. It indicates the percentage of employees who left the organization, either voluntarily or involuntarily, during a specified period. It represents the total employee turnover within the organization.


Employee Attrition Prediction
Log into Kaggle and download the dataset for IBM HR Analytics Employee Attrition & Performance
Data contains differnet attributes of an employee and the target variable Atrition. EmployeeNumber is the primary key. We use this dataset to predict employee churn.
Data definitions for categorical variables: 
Education 1 'Below College' 2 'College' 3 'Bachelor' 4 'Master' 5 'Doctor'
EnvironmentSatisfaction 1 'Low' 2 'Medium' 3 'High' 4 'Very High'
JobInvolvement 1 'Low' 2 'Medium' 3 'High' 4 'Very High'
JobSatisfaction 1 'Low' 2 'Medium' 3 'High' 4 'Very High'
PerformanceRating 1 'Low' 2 'Good' 3 'Excellent' 4 'Outstanding'
RelationshipSatisfaction 1 'Low' 2 'Medium' 3 'High' 4 'Very High'
WorkLifeBalance 1 'Bad' 2 'Good' 3 'Better' 4 'Best'
