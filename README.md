# sda_project

Data retrieved from https://www.kaggle.com/sulianova/cardiovascular-disease-dataset.

### Goal

To determine the presence or absence of cardiovascular disease in patients from five numerical and six categorical variables.
The data set has 11 features or predictors, one target or response variable, and 70000 instances or records.
All data set values were collected at the moment of a medical examination.

### Data description

There are 3 types of input features:

**Objective**: factual information;
**Examination**: results of medical examination;
**Subjective**: information given by the patient.

##### Features:

Age | Objective Feature | age | int (days)
Gender | Objective Feature | gender | categorical: 1 = women, 2 = men
Height | Objective Feature | height | int (cm)
Weight | Objective Feature | weight | float (kg)
Systolic blood pressure | Examination Feature | ap_hi | int
Diastolic blood pressure | Examination Feature | ap_lo | int
Cholesterol | Examination Feature | cholesterol | categorical: 1 = normal, 2 = above normal, 3 = well above normal
Glucose | Examination Feature | gluc | categorical: 1 = normal, 2 = above normal, 3 = well above normal
Smoking | Subjective Feature | smoke | binary: 0 = non-smoker, 1 = smoker
Alcohol intake | Subjective Feature | alco | binary: 0 = non-drinker, 1 = drinker
Physical activity | Subjective Feature | active | binary: 0 = non-active, 1 = active
Presence or absence of cardiovascular disease | Target Variable | cardio | binary: 0 = no cardiovascular disease, 1 = cardiovascular disease

All of the data set values were collected at the moment of medical examination.