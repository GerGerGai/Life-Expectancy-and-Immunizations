rm(list = ls())

# Import required libraries
#install.packages("carData")
#install.packages("GGally")
library(carData)
library(car)
library(GGally)
library(ggplot2)
#install.packages("reshape2")
library(reshape2)
#install.packages("olsrr")
library(olsrr)
library(leaps)
install.packages("ggcorrplot")
library(ggcorrplot)



# Import dataset  
expectancy = read.csv("./Life Expectancy Data.csv")

expectancy$Status = factor(expectancy$Status, levels = c("Developing", "Developed"))

# Change the column names so they are more clear for plots
expectancy_names <- expectancy
names(expectancy_names) <- c("Country",
                       "Year",
                       "Status",
                       "Life expectancy",
                       "Adult Mortality",
                       "Infant deaths",
                       "Alcohol",
                       "Percentage expenditure",
                       "Hepatitis B",
                       "Measles",
                       "BMI",
                       "Under five deaths",
                       "Polio",
                       "Total expenditure",
                       "Diphtheria",
                       "HIV AIDS",
                       "GDP",
                       "Population",
                       "Thinness 1-19 years",
                       "Thinness 5-9 years",
                       "Income composition of resources",
                       "Schooling")



# A basic overview of the whole dataset:

# Notice some columns do contain NAs. 
# We need to either ignore them or replace with certain values (means or medians) for later analysis
summary_expectancy = summary(expectancy)
summary_expectancy


# Construct and Visualize a correlation matrix
# Step1: Extract Continuous Variables
continuous_vars <- expectancy_names[sapply(expectancy, is.numeric)]

# Step2: Build correlation matrix
# use = "pairwise.complete.obs" means we only use non-missing data
cor_matrix = cor(continuous_vars, use = "pairwise.complete.obs")

# Step3: Visualize correlation matrix
cor_matrix_long <- melt(cor_matrix) # Convert the matrix into a long format

ggcorrplot(cor_matrix, method = "circle") + 
  scale_fill_gradient2(low = "#6D9EC1", high = "#E46726", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(x = "", y = "", title = "Correlation Matrix") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))

# Key Features: Adult Mortality, HIV.AIDS, Schooling and Income composition of resources
#               have the highest correlation with life expectancy.
#               Immunization related factors (Hepatitis B, Polio, Diphtheria) also have
#               moderately strong correlations with life expectancy.
#               Immunization related factors may have collinearity issues when doing linear
#               regression, we might need to do some work on reducint this issue

# Pair Plots
selected_pairs = expectancy_names[c("Life expectancy","Year","Hepatitis B","Polio","Diphtheria")]
ggpairs(selected_pairs) +   theme_minimal()


# Key features: Moderate correlation between immunization factors and life expectancy.
#               Colinearity issues among immunization factors

# Initial brief visualization to set the motivation
ggplot(expectancy, aes(x = Polio, y = Life.expectancy, color = Status)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  
  theme_minimal() +
  labs(title = "Life Expectancy vs. Polio Coverage, with Lines for developed and developing countries",
       x = "Polio Coverage (%)", y = "Life Expectancy (age)")

# Key features: 1. There seems to be a significant difference of Life Expectancy between developing countries and
#                   developed countries
#               2. The effect of Polio coverage on life expectancy is much more obvious and positive for
#               developing countries than those for developed countries
#               3. This motivates us to do more thorogh analysis on the datasets and see how immunization factors
#               could potentially improve life expectancy in developing countries


# Some ideas for analysis:
# 1. two sample t test to see if there is a real difference between the expectancy of developing and developed countries
# 2. two sample t test..... difference of immunization factors
#   comment: I am thinking of if (2) is necessary, if (1) already tells us that the life expectancy of developing 
#       and developed countries have different life expectancy, this should suffice for us to do regression based on
#       them separately.
#       Also, if we do regression including categorical variable based on status of country, wouldn't the model selection process would tell us
#       if they are having significant difference? Not sure if we should/have to do (1) tbh

# 3. build the first full model interactions?
# 4. vifs to confirm that collinearity is actually not a big deal here
# 5. backward to select parameters
# 6. PCA to select the best of the three factors

#1. two sample t test to see if there is a real difference between the expectancy of developing and developed countries
# Extract expectancy columns for developing and developed countries, removing NAs
e_developing = expectancy[expectancy$Status == "Developing",]$Life.expectancy
e_developed = expectancy[expectancy$Status == "Developed",]$Life.expectancy
e_developing = e_developing[!is.na(e_developing)]
e_developed = e_developed[!is.na(e_developed)]

# Assumptions check for two sample t-test:
#     Independent observations: We assume it to be true
#     Normality: the data in both groups should be approximately normally distributed.
#               We check this by creating a qqnorm plots for both groups.
#               They're indeed approximately normally distributed
par(mfrow = c(1, 2))

qqnorm(e_developing, col = "#6D9EC1", main = "Normal QQ Plot of Developing Countries")
qqline(e_developing, col = "#E46726", main = "Normal QQ Plot of Developing Countries")

qqnorm(e_developed, col = "#6D9EC1", main = "Normal QQ Plot of Developed Countries")
qqline(e_developed, col = "#E46726", main = "Normal QQ Plot of Developed Countries")


#     Equality of variances: two groups should have similar variances:
#     This seems not true in our case.
#     We will set var.equal = FALSE in t.test() to address this
var(e_developed)
var(e_developing)

t.test(e_developed,e_developing, var.equal = FALSE)

# Key notes: t = ...., p = ...., there is indeed a significant difference of ......

#2. two sample t test..... difference of immunization factors
# Construct a new column immunization_coverage, which will take the average of the coverage of the three factors
expectancy["Immun"] = (expectancy$Hepatitis.B + expectancy$Polio + expectancy$Diphtheria)/3
# Extract columns for developing and developed countries, removing NAs
im_developing = expectancy[expectancy$Status == "Developing",]$Immun
im_developed = expectancy[expectancy$Status == "Developed",]$Immun
im_developing = im_developing[!is.na(im_developing)]
im_developed = im_developed[!is.na(im_developed)]

# Assumptions check for two sample t-test:
#     Independent observations: We assume it to be true
#     Normality: the data in both groups should be approximately normally distributed.
#               We check this by creating a qqnorm plots for both groups.
#               They're not normally distributed. But the sample size is large enough so we can relax this requirement

qqnorm(im_developing, col = "#6D9EC1", main = "Normal QQ Plot of Developing Countries")
qqline(im_developing, col = "#E46726", main = "Normal QQ Plot of Developing Countries")

qqnorm(im_developed, col = "#6D9EC1", main = "Normal QQ Plot of Developed Countries")
qqline(im_developed, col = "#E46726", main = "Normal QQ Plot of Developed Countries")

#     Equality of variances: two groups should have similar variances:
#     This seems not true in our case.
#     We will set var.equal = FALSE in t.test() to address this
var(im_developed)
var(im_developing)

t.test(im_developed,im_developing, var.equal = FALSE)

# Key notes: t = ...., p = ...., there is indeed a significant difference of ......


#3. since there are significant difference between life expectancy of developed and developing country,
#   we build model, allowing Status interact with different immunization factor
expectancy
reg_full = lm(Life.expectancy ~ (Hepatitis.B + Polio + Diphtheria + Year) * Status, data = expectancy)
summary(reg_full)
#observation: only some parameters are significant, lets do stepwise backward selection / regsubset
# (backward selection seems not showing stepwise summary correctly)
backward_model_ols = ols_step_backward_p(reg_full)
ols_step_backward_p(reg_full)
summary(backward_model_ols$model)

#regsubset (ignore, we just do backward)
reg_subset = regsubsets(Life.expectancy ~ (Hepatitis.B + Polio + Diphtheria + Year) * Status, data = expectancy, method="exhaustive")
summary_reg_subset = summary(reg_subset)
summary_reg_subset$which
summary_reg_subset$rsq
summary_reg_subset$adjr2
summary_reg_subset$rss
summary_reg_subset$cp

#based on the above, we should choose the model having 8 parameters (***including intercepts, i.e.
# (Intercept, Polio, Diphtheria,  Year ,StatusDeveloped ,Hepatitis.B:StatusDeveloped , Diphtheria:StatusDeveloped, Year:StatusDeveloped)
# and check if vif is good
vif(backward_model_ols$model, type = 'predictor')

#since scaled gvif of polio , diphtheria and status is too high, try dropping status
# for here, keypoint is, correlation between polio and status, that between diphtheria and status, are high
# meaning that collinearity is a problem
# so we try dropping Diphtheria (highest scaled GVIF)
reg_drop = lm(Life.expectancy ~ ( Polio  + Year) * Status, data = expectancy)
summary(reg_drop)
#contains insignificant result, perform model seelction
backward_model_ols_drop = ols_step_backward_p(reg_drop)
summary(backward_model_ols_drop$model)
#check vif
vif(backward_model_ols_drop$model,type = 'predictor')
#vif is good now, all parameters are significant also
#this is our final model

# question: seems like we get two best models, one is from backward selection and one is from reg_subsets.
# Do we want to compare these two? by residual plots? 

best_model1 = backward_model_ols_drop$model
summary(best_model1)
residuals_1 = best_model1$residuals
best_model1$fitted.values

plot(residuals_1~best_model1$fitted.values, cex = 0.5, xlab = "Fitted Life Expectancy (age)", ylab = "Residuals (age)",
     main = "Residual Plot", col = ifelse(expectancy$Status == "Developed", "#6D9EC1", "#E46726")) +
  legend("topright", legend = unique(expectancy$Status), col = unique(ifelse(expectancy$Status == "Developed", "#6D9EC1", "#E46726")), pch = 19)
abline(h = 0, col = "black")