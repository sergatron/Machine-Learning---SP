## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:
setwd("C:/Users/mouz/Desktop/DataSci/Machine Learning/Logistic Regression/logistic_regression/logistic_regression")
?attributes
NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels
glimpse(NH11)
glimpse(labs)

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev

# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels = c("2 No", "1 Yes"))
levels(NH11$hypev) # check levels of hypev
summary(NH11$hypev)

# run our regression model
hyp.out <- glm(hypev ~ age_p + sex + sleep + bmi,
              data=NH11, family="binomial")
summary(hyp.out)
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab
?exp
## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
?expand.grid
?with
predDat <- with(NH11,
                expand.grid(age_p = c(30,60),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
class(predDat)
glimpse(NH11)


# predict hypertension at those levels
?cbind
?predict
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

predict(hyp.out, type = "response",
        newdata = predDat)


##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

# install.packages('effects')
library(effects)
plot(allEffects(hyp.out))

## ---- Exercise: logistic regression ----
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).

##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.
library(mice)
library(caTools)
library(dplyr)
?mice
glimpse(NH11)
table(NH11$everwrk)
table(NH11$r_maritl)

# seleect the needed columns 
nh_sub = 
  NH11 %>%
  select(everwrk, age_p, r_maritl)
glimpse(nh_sub)

# check levels
levels(nh_sub$everwrk)
levels(nh_sub$r_maritl)

table(nh_sub$r_maritl)
table(nh_sub$everwrk)
# remove any category with 0 

nh_sub$everwrk = factor(imputed_nh$everwrk, levels = c("2 No", "1 Yes"))

nh_sub$r_maritl = factor(imputed_nh$r_maritl, levels = c("1 Married - spouse in household" ,  
                                         "2 Married - spouse not in household", "4 Widowed", "5 Divorced",
                                         "6 Separated", "7 Never married" ,  "8 Living with partner"))
summary(nh_sub)
summary(nh_sub$everwrk)
# ---- Replacing Missing Values ----
set.seed(50)
imputed_nh = complete(mice(nh_sub))

# view summary of clean data frame
summary(imputed_nh)
glimpse(imputed_nh)

# ---- Train and Test ----
split = sample.split(imputed_nh$everwrk, SplitRatio = 0.8)
train_nh = subset(imputed_nh, split == TRUE)
test_nh = subset(imputed_nh, split == FALSE)

summary(test_nh)
summary(train_nh)

# baseline
table(imputed_nh$everwrk)
28002/(5012 + 28002) # -> 0.8482
# 84.81 % of people said that they worked 


# ---- 1. Use glm to conduct a logistic regression to predict ever worked ----
##      (everwrk) using age (age_p) and marital status (r_maritl). 

# ---- Create Model ----
glimpse(imputed_nh$everwrk)
work_mod = glm(everwrk ~ age_p + r_maritl, data = train_nh, family = 'binomial')

plot(work_mod)
summary(work_mod)

work_mod_coef = coef(summary(work_mod))
work_mod_coef[, "Estimate"] <- exp(coef(work_mod))
work_mod_coef
summary(work_mod)

# ---- Prediction w/Training set ----
# make predictions on a training set
workPred = predict(work_mod, type = 'response')
summary(workPred) 
tapply(workPred, train_nh$everwrk, mean)
# the range seems very narrow, 0.5469 to 0.9764. Most people replied to YES???

glimpse(work_mod)
glimpse(train_nh$everwrk)
#
#
#                           |    Predicted   |  Predicted   |
#                           |     -NO (0)    |   -YES (1)   |
#          ---------------------------------------------------
#           Actual NO (0)   | True Neg TN    | False Pos FP |
#           Actual YES (1)  |  Fasle Neg FN  | True Pos TP  |
# 

table(train_nh$everwrk, workPred >= 0.7) 
total_train = (17623 + 1080 + 571 + 2735)
571 / (2735 + 571) # true neg rate = 0.1727
17623 / (17623 + 1080) # true pos rate = 0.9423
(17623 + 571) / total_train # accuracy = 0.8267 when t >= 0.7


table(train_nh$everwrk, workPred >= 0.8) 
1381 / (1381 + 1925) # true neg = 0.4177
14874 / (14874 + 3829) # true pos = 0.7923
(14874 + 1381) / total_train # accuracy = 0.7386 when t >= 0.8

# ---- ROC Curves ----

library(ROCR)
ROCRpred = prediction(workPred, train_nh$everwrk)
ROCRperformance = performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperformance)
plot(ROCRperformance, colorize = TRUE)
plot(ROCRperformance, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))


# ---- Prediction w/Test set ----
workPred2 = predict(work_mod, type = 'response', newdata = test_nh)
summary(workPred2)
class(workPred2)
typeof(workPred2)

table(test_nh$everwrk, workPred2 >= 0.7) 
total_test = (8741 + 607 + 1328 + 312 + 11 + 2 + 4)
(8757 + 279 )/ total_test 
# 0.8210

table(test_nh$everwrk, workPred2 >= 0.8) 
(8206 + 477) / total_test 
# 0.7890

# ---- predict using Train and Test sets
# Train set predictions
head(train_nh$everwrk)
?tapply
head(workPred)
summary(workPred)

tibble(probability = tapply(workPred, train_nh$everwrk, mean), 
       worked = c("1 Yes", "2 No"))

# Test set predictions
tibble(probability = tapply(workPred2, test_nh$everwrk, mean), 
       worked = c("1 Yes", "2 No"))



##  ---- 2. Predict the probability of working for each level of marital status ----
# compute the mean probability for each marital level 

# use tibble() and compute probability of 'ever worked' for each category in marital status
tibble(probability = tapply(workPred, train_nh$r_maritl, mean), 
       marital_sts =  c("1 Married - spouse in household" ,  
                        "2 Married - spouse not in household", "4 Widowed", "5 Divorced",
                        "6 Separated", "7 Never married" ,  "8 Living with partner"))

ever_worked = 
  tibble(probability_mean = tapply(workPred2, test_nh$r_maritl, mean), 
       category = c("1 Married - spouse in household" ,  
                    "2 Married - spouse not in household", "4 Widowed", "5 Divorced",
                    "6 Separated", "7 Never married" ,  "8 Living with partner"))
ever_worked


# Create a dataset with predictors set at desired levels
?expand.grid
?with
levels(imputed_nh$r_maritl)
table(imputed_nh$r_maritl)
everWork = with(imputed_nh,
                expand.grid(age_p = c(30),
                            everwrk = c("1 Yes", "2 No"),
                            r_maritl = c("1 Married - spouse in household" ,  
                                         "2 Married - spouse not in household", "4 Widowed", "5 Divorced",
                                         "6 Separated", "7 Never married" ,  "8 Living with partner")))

cbind(everWork, predict(work_mod, type = "response",
                       se.fit = TRUE, interval = "confidence",
                       newdata = everWork))




# Create a dataset with predictors set at desired levels
everWork2 = with(imputed_nh,
                 expand.grid(age_p = c(20,30,40,50),
                             r_maritl = '1 Married - spouse in household',
                             everwrk = "2 No"))

cbind(everWork2, predict(work_mod, type = "response",
                         se.fit = TRUE, interval = "confidence",
                         newdata = everWork2))



