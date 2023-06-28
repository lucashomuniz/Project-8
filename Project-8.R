# Project-8 Financial Analysis with SQL Language and Linear Regression in R Language
# packages
library(dplyr)
library(sqldf)

# Load the dataset
dados <- read.csv('dataset.csv')

# Dimensions
dim(dados)

# Visualize the data
View(dados) 

# Checking occurrence of NA values
colSums(is.na(dados)) 

# Only 1 record with NA value. Let's remove it.
dados <- na.omit(dados)

# Checking occurrence of NA values
colSums(is.na(dados)) 

# Data types
str(dados)

"
=======================================
EXPLORATORY ANALYSIS WITH SQL LANGUAGE
=======================================
"

# Column names
names(dados)

# 1 - How many races are represented in the dataset?
num_racas <- sqldf('SELECT RACE, COUNT(RACE) as Num_Races FROM dados GROUP BY RACE')
num_racas

# 2 - What is the average age of patients?
idade_media <- sqldf('SELECT AVG(AGE) as Idade_Media FROM dados')
idade_media

# 3 - What is the patient's age mode?
idade_moda <- sqldf(" SELECT AGE as Idade FROM (SELECT AGE, COUNT(AGE) AS count_age 
FROM dados GROUP BY AGE ORDER BY count_age DESC) LIMIT 1")
idade_moda

# 4 - What is the variance of the age column?
idade_variancia <- sqldf("SELECT SUM((AGE - (SELECT AVG(AGE) FROM dados)) * 
(AGE - (SELECT AVG(AGE) FROM dados))) / (COUNT(AGE) - 1) AS variancia FROM dados")
idade_variancia

# 5 - What is the total cost of hospital admissions by age?
gasto_total_idade <- sqldf('SELECT AGE as Idade, SUM(TOTCHG) as Gasto_Total 
FROM dados GROUP BY AGE')
gasto_total_idade

# 6 - What age generates the highest total expenditure on hospital admissions?
class(gasto_total_idade)
arrange(gasto_total_idade, desc(Gasto_Total))[1,]

# 7 - What is the total expenditure on hospital admissions by gender?
gasto_total_genero <- sqldf('SELECT FEMALE as Genero, SUM(TOTCHG) as Gasto_Total 
FROM dados GROUP BY FEMALE')
gasto_total_genero

# 8 - What is the average cost of hospital admissions by patient's race?
gasto_medio_race <- sqldf('SELECT RACE, AVG(TOTCHG) as Gasto_Medio 
FROM dados GROUP BY RACE')
gasto_medio_race

# 9 - For patients over 10 years old, what is the average total expense with hospital admissions?
gasto_medio_idade_acima_10anos <- sqldf('SELECT AGE as Idade, AVG(TOTCHG) as Gasto_Medio 
FROM dados WHERE AGE > 10 GROUP BY AGE')
gasto_medio_idade_acima_10anos

# 10 - Considering the previous item, what age has an average spending greater than 3000?
gasto_medio_idade_acima_10anos_acima3k <- sqldf('SELECT AGE as Idade, AVG(TOTCHG) 
as Gasto_Medio FROM dados WHERE AGE > 10 GROUP BY AGE HAVING AVG(TOTCHG) > 3000')
gasto_medio_idade_acima_10anos_acima3k

"
=====================================
STATISTICAL ANALYSIS WITH R LANGUAGE
=====================================
"

# Question 1:
# What is the age distribution of patients attending the hospital?
hist(dados$AGE)
hist(dados$AGE, 
     main = "Histogram of the Age Distribution of Patients Attending the Hospital",
     xlab = "Age", 
     border = "black", 
     col = c("light gray", "dark green"), 
     xlim = c(0,20), 
     ylim = c(0,350))

# If we do the summary with a numerical type variable, the result is a statistical summary.
summary(dados$AGE)

# We convert the variable to the factor type and then get the summary we need.
resumo_idade <- summary(as.factor(dados$AGE))
resumo_idade

# Answer to Question 1:
# Children between 0 and 1 year old are the ones who attend the hospital the most.

# Question 2:
# Which age group has the highest total hospital spending?
gasto_total_baseado_idade = aggregate(TOTCHG ~ AGE, FUN = sum, data = dados)
View(gasto_total_baseado_idade)
which.max(tapply(gasto_total_baseado_idade$TOTCHG, gasto_total_baseado_idade$TOTCHG, FUN = sum))
barplot(tapply(gasto_total_baseado_idade$TOTCHG, gasto_total_baseado_idade$AGE, FUN = sum))

# Answer to Question 2:
# Children between 0 and 1 year old are the ones that generate the highest expenses in the hospital.

# Question 3:
# Which diagnosis-based group (Aprdrg) has the highest total hospital spend?
gasto_total_baseado_diag = aggregate(TOTCHG ~ APRDRG, FUN = sum, data = dados)
View(gasto_total_baseado_diag)
gasto_total_baseado_diag[which.max(gasto_total_baseado_diag$TOTCHG), ]

# Answer to Question 3:
# The 640 group has the highest total spend.

# Question 4:
# Is the patient's race related to the total amount spent on hospital stays?

# We will use an ANOVA Test.
# Dependent variable in the ANOVA Test: TOTCHG
# Independent variable in the ANOVA Test: Race

# H0: There is no RACE effect on TOTCHG.
# H1: There is RACE effect on TOTCHG.

summary(dados$RACE)
summary(as.factor(dados$RACE))
modelo_anova_1 <- aov(TOTCHG ~ RACE, data = dados)
summary(modelo_anova_1)

# Answer to Question 4:
# The p-value is greater than 0.05. We failed to reject H0.
# The patient's race does not influence the total cost of hospitalization.

# Question 5:
# The combination of age and gender of patients influences the total cost
# in hospital admissions?

# We will use an ANOVA Test.
# Dependent variable in the ANOVA Test: TOTCHG
# Independent variables in the ANOVA Test: AGE, FEMALE

# H0: There is no effect of AGE and FEMALE on TOTCHG.
# H1: There is effect of AGE and FEMALE in TOTCHG.

modelo_anova_2 <- aov(TOTCHG ~ AGE + FEMALE, data = dados)
summary(modelo_anova_2)

# Answer to Question 5:
# In both cases the p-value is less than 0.05. We reject the null hypothesis.
# There is a significant effect of age and gender on hospital costs.

# Question 6:
# As length of stay is the crucial factor for inpatients,
# we want to find out if the dwell time can be predicted from
# age, gender and race.

# We will use a Linear Regression model.
# Dependent variable: LOS
# Independent variables: AGE, FEMALE and RACE

# H0: There is no linear relationship between dependent and independent variables.
# H1: There is a linear relationship between dependent and independent variables.

modelo_lr <- lm(LOS ~ AGE + FEMALE + RACE, data = dados)
summary(modelo_lr) 

# Answer to Question 6:
# P-value greater than 0.05 in all cases, so we fail to reject H0.
# Length of stay cannot be predicted from the independent variables used.

# Question 7:
# Which variables have the greatest impact on hospitalization costs?

# We will use a Linear Regression model.
# Dependent variable: TOTCHG
# Independent variables: AGE, FEMALE, LOS, RACE and APRDRG

# H0: There is no linear relationship between dependent and independent variables.
# H1: There is a linear relationship between dependent and independent variables.

modelo_lr_geral <- lm(TOTCHG ~ ., data = dados)
summary(modelo_lr_geral) 

# As we can see from the values of the coefficients, the variables age,
# length of stay (LOS) and refined patient diagnosis group (APRDRG)
# have three asterisks (***) next to them. So they're the ones with statistical significance
# Also, RACE is not significant. Let's remove RACE and build another version of the model.

modelo_lr_4var <- lm(TOTCHG ~ AGE + FEMALE + LOS + APRDRG, data = dados)
summary(modelo_lr_4var) 

# Note that the variable representing gender has the least significance for the model.
# Let's remove it and create another version of the model.

modelo_lr_3var <- lm(TOTCHG ~ AGE + LOS + APRDRG, data = dados)
summary(modelo_lr_3var) 

# The 3 variables have high significance, but APRDRG has a negative t-value.
# Let's remove it and create another version of the model.

modelo_lr_2var <- lm(TOTCHG ~ AGE + LOS, data = dados)
summary(modelo_lr_2var) 

# Removing race and gender does not change the value of R2.
# Removing the APRDRG in the model increases the standard error.
# So the model model_lr_3var seems to be the best and we will use it for our conclusion.
summary(modelo_lr_3var) 

# Conclusion:
# As is evident from the various models above, health care costs depend on
# of age, length of stay and diagnosis group.
# These are the 3 most relevant variables to explain and predict spending on
# hospital admissions.
