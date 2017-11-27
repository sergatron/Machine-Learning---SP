#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")

##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder
list.files(getwd())

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
sts.ex.sat
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]
?methods()

##   • Use function methods to get more information about the fit

?confint
confint(sat.mod)
hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional
plot(sat.mod)


## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
summary(sat.voting.mod)
summary(sat.mod)
coef(summary(sat.voting.mod))

## ---- Exercise: least squares regression ----
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

# 1. Examine/plot the data before fitting the model
states.info
head(states.data, n = 10)
energyReg = lm(energy ~ metro, data = states.data)
energyReg$residuals
summary(energyReg)

# 2. Print and interpret the model `summary'
sts_energy_metro = subset(states.data, select = c("energy", "metro"))
cor(na.omit(sts_energy_metro))
summary(sts_energy_metro)

plot(states.data$energy, states.data$metro)

confint(energyReg)
hist(residuals(energyReg))

# The intercept is a significant coefficient and metro shows minor significance


# 3. `plot' the model to look for deviations from modeling assumptions
plot(energyReg)


# ---- Part 2 ----
##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

# 1. Examine/plot the data before fitting the model
# adding college, pop, area
states.info
head(states.data, n = 10)
states_data = na.omit(states.data)
energyReg = lm(energy ~ metro + area  + toxic + green, data = states_data)
summary(energyReg)

energyReg2 = lm(energy ~ metro + green, data = states_data)
summary(energyReg2)

energyReg3 = lm(energy ~ metro + green + area, data = states_data)
summary(energyReg3)

energyReg$residuals
confint(energyReg)
hist(residuals(energyReg))

# 2. Print and interpret the model `summary'
sts_energy_metro = subset(states.data, select = c("energy", 'green', 'toxic', 'metro'))

cor(na.omit(sts_energy_metro))
summary(sts_energy_metro)

# adding all the columns to the equation and then removing one at a time yields the following columns having greatest impact: area, toxic, green
# 'metro' becomes less significant as a result

plot(states_data$energy, states_data$metro)
plot(states_data$energy, states_data$area)
plot(states_data$energy, states_data$toxic)
plot(states_data$energy, states_data$green)


# 3. `plot' the model to look for deviations from modeling assumptions
plot(energyReg)



## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table
  summary(sat.expense.by.percent)
## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
summary(sat.region)
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
?contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
coef(summary(lm(csat ~ C(region, base = 2),
                data=states.data)))

# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.
?contr.treatment
?relevel

## ---- Exercise: interactions and factors ----
## ────────────────────────────────────────

##   Use the states data set.
states_data
states.info

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.
energyReg0 = lm(energy ~ metro + pop * high, data = states_data)
summary(energyReg0)
energyReg = lm(energy ~ metro + area  + toxic + green, data = states_data)
summary(energyReg)

states_data$region <- factor(states_data$region)

# population * high -> portion of pop with HS diploma
energyReg2 = lm(energy ~ metro + area  + toxic + green + (pop * high), data = states_data)
summary(energyReg2)

# metro * high -> portion of metro pop with HS diploma
energyReg3 = lm(energy ~ metro + green + area + metro * high, data = states_data)
summary(energyReg3)

# metro * college -> portion of metro pop with HS diploma
energyReg4 = lm(energy ~ metro + green + area + metro * college, data = states_data)
summary(energyReg4)

# income / (pop * high) -> income / HS educated portion of pop
energyReg5 = lm(energy ~ metro + green + area + income / (pop * high), data = states_data)
summary(energyReg5)

# ---- toxic / density -> lbs / (pop / sq.mile) -> (lbs * sq. mile )/ pop
energyReg6 = lm(energy ~ metro + green + area + toxic / density, data = states_data)
summary(energyReg6)


confint(energyReg6)
hist(residuals(energyReg6))
plot(energyReg6)


# green / density
energyReg7 = lm(energy ~ metro + green + area + green / density, data = states_data)
summary(energyReg7)

states_data
states.info
# green / density
energyReg8 = lm(energy ~ metro + green + area + miles / density, data = states_data)
summary(energyReg8)



energyReg$residuals
confint(energyReg)
hist(residuals(energyReg))


##   2. Try adding region to the model. Are there significant differences
##      across the four regions?
# There is minor differences between the four regions. The South region's t-value is larger than the other regions; 
# therefore, South appears to be slightly more significant within the model than the other three.

states_data$region <- factor(states_data$region)
energyReg6_region = lm(energy ~ metro + green + area + toxic / density + region, data = states_data)
summary(energyReg6_region)
anova(energyReg6_region)
plot(energyReg6_region)

# sse, sum of sqrd error
sse = sum(energyReg6_region$residuals ** 2)
# rmse, root mean square error
rmse = sqrt(sse/nrow(states_data))
# error of 44.47 Btu
mean_energy = mean(states_data$energy) # 343 Btu
rmse / mean_energy * 100 # 12.94% error

# correlation between columns
states_data$toxic_den = states_data$toxic / states_data$density
sts_energy= subset(states_data, select = c("energy", 'green', 'toxic_den', 'metro', 'toxic', 'high', 'college'))
cor(sts_energy)


# ---- without Region
energyReg6 = lm(energy ~ metro + green + area + toxic / density, data = states_data)
summary(energyReg6)
anova(energyReg6)
plot(energyReg6)


anova(energyReg6, energyReg6_region)

# sse, sum of sqrd error
sse = sum(energyReg6$residuals ** 2)
# rmse, root mean square error
rmse = sqrt(sse/nrow(states_data))
# error of 44.47 Btu
mean_energy = mean(states_data$energy) # 343 Btu
rmse / mean_energy * 100 # 13.27% error



