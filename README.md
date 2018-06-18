# R_MoneyBall_OLR_Regression
Assignment for Generalized Linear Models Course

Assignment Description:
This data set contains approximately 2200 records. Each record represents a professional baseball team from the years 1871 to 2006 inclusive. Each record has the performance of the team for the given year, with all of the statistics adjusted to match the performance of a 162 game season. You are to use OLS (“Linear”) Regression and the given statistics to predict the number of wins for the team. You can only use the variables given to you (or variable that you derive from the variables provided).

My solution explored using several variable selection techniques: stepwise, backward step, and CP Mallow. I also tried using imputed variables flag variables for those variables with missing data. I imputed variables with missing data using decision trees. Outliers were identified using IQR and variables were capped based on where they landed on the range. The distribution of the variables data was also taken into consideration and some variables were transformed to make their distributions more normal. Data visualization was used (boxplots, histograms, and correlation matrices) to help in making these decisions. Variables that didn't make sense in the model due to signs being in the opposite direction were discarded and the models rerun until an adequate model was discovered. Models were evaluated using the number of parameters, AIC, Adjusted R-Square, BIC, Root Mean Squared Error, and CP-Mallow. The final model selected was run against the holdout data and an output CSV was created.

Files included in repository:

moneyball.csv -- Training data set                                                                                                    
moneyball_test.csv -- Holdout data set
Kim_Kaminsky_HW1_R.R -- R code used to analyze data and create models
