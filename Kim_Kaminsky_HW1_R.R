###################################
# Install packages and libraries  #
###################################
# install.packages("https://cran.r-project.org/bin/windows/contrib/3.3/RGtk2_2.20.31.zip", repos=NULL)
# install.packages(c("Hmisc", "leaps", "hydroGOF", "CombMSC", "locfit", "car", "rattle"))
# install.packages("rattle")
library(Hmisc)
library(leaps)
library(hydroGOF)
library(CombMSC)
library(locfit)
library(car)
library(rattle)
library(rpart)

#######################
# Clear out memory    #
#######################
rm(list=ls())


##########################################
# Read in Files and view data structures #
##########################################
BB <- read.csv("D:/Kim MSPA/Predict 411/Homework 1/moneyball.csv", sep = ",")

# Setup max print option so all rows will be displayed
options(max.print = 2276)

# 'data.frame': 2276 obs. of  17 variables
str(BB, list.len = ncol(BB))

# Start the EDA
describe(BB)

#####################################################################
# Start working on Model 1. The object of this model is to create   #
# an initial model as a baseline. Do the minimum work: clean up     #
# variables with missing values and created imputed flag variables  #
####################################################################;


# Create TEMP data file to store all changes made to the dataset
TEMP <- BB

# Drop variables with too many missing values
TEMP$TEAM_BATTING_HBP <- NULL

# Clean up missing values and created imputed variable flags

# TEAM_BATTING_SO
TEMP$M_TEAM_BATTING_SO <- 0
TEMP$M_TEAM_BATTING_SO[is.na(TEMP$TEAM_BATTING_SO)] <- 1

TEMP$IMP_TEAM_BATTING_SO <- TEMP$TEAM_BATTING_SO
median_tbso <- median(TEMP$TEAM_BATTING_SO[!is.na(TEMP$TEAM_BATTING_SO)])
TEMP$IMP_TEAM_BATTING_SO[is.na(TEMP$TEAM_BATTING_SO)] <- median_tbso
TEMP$TEAM_BATTING_SO <- NULL

# TEAM_BASERUN_SB
TEMP$M_TEAM_BASERUN_SB <- 0
TEMP$M_TEAM_BASERUN_SB[is.na(TEMP$TEAM_BASERUN_SB)] <- 1

TEMP$IMP_TEAM_BASERUN_SB <- TEMP$TEAM_BASERUN_SB
median_tbsb <- median(TEMP$TEAM_BASERUN_SB[!is.na(TEMP$TEAM_BASERUN_SB)])
TEMP$IMP_TEAM_BASERUN_SB[is.na(TEMP$TEAM_BASERUN_SB)] <- median_tbsb
TEMP$TEAM_BASERUN_SB <- NULL


# TEAM_BASERUN_CS
TEMP$M_TEAM_BASERUN_CS <- 0
TEMP$M_TEAM_BASERUN_CS[is.na(TEMP$TEAM_BASERUN_CS)] <- 1

TEMP$IMP_TEAM_BASERUN_CS <- TEMP$TEAM_BASERUN_CS
median_tbcs <- median(TEMP$TEAM_BASERUN_CS[!is.na(TEMP$TEAM_BASERUN_CS)])
TEMP$IMP_TEAM_BASERUN_CS[is.na(TEMP$TEAM_BASERUN_CS)] <- median_tbcs
TEMP$TEAM_BASERUN_CS <- NULL


# TEAM_PITCHING_SO
TEMP$M_TEAM_PITCHING_SO <- 0
TEMP$M_TEAM_PITCHING_SO[is.na(TEMP$TEAM_PITCHING_SO)] <- 1

TEMP$IMP_TEAM_PITCHING_SO <- TEMP$TEAM_PITCHING_SO
median_tpso <- median(TEMP$TEAM_PITCHING_SO[!is.na(TEMP$TEAM_PITCHING_SO)])
TEMP$IMP_TEAM_PITCHING_SO[is.na(TEMP$TEAM_PITCHING_SO)] <- median_tpso
TEMP$TEAM_PITCHING_SO <- NULL

# TEAM_FIELDING_DP
TEMP$M_TEAM_FIELDING_DP <- 0
TEMP$M_TEAM_FIELDING_DP[is.na(TEMP$TEAM_FIELDING_DP)] <- 1

TEMP$IMP_TEAM_FIELDING_DP <- TEMP$TEAM_FIELDING_DP
median_tfdp <- median(TEMP$TEAM_FIELDING_DP[!is.na(TEMP$TEAM_FIELDING_DP)])
TEMP$IMP_TEAM_FIELDING_DP[is.na(TEMP$TEAM_FIELDING_DP)] <- median_tfdp
TEMP$TEAM_FIELDING_DP <- NULL

TEMP$INDEX <- NULL;


# Check the data to make sure it looks as expected
head(TEMP)
describe(TEMP)


#########################################################
# Create Model1 using 3 different selection techniques  #
#########################################################

# All variables
# Adjusted R-squared:  0.4069 
summary(Model1 <- lm(TARGET_WINS ~ ., data = TEMP))


# Backward Selection
# Adjusted R-squared:   0.4074
# Commenting this out as it produced the same model as stepwise
# summary(Model1_back <- step(Model1, direction = "back"))

# Stepwise Selection
# Adjusted R-squared:  0.4074
summary(Model1_step <- step(Model1, direction = "both"))

# Decision Tree Variable Selection
#rattle()

summary(Model1_dt <- lm(TARGET_WINS ~  IMP_TEAM_BASERUN_SB + IMP_TEAM_PITCHING_SO +      
                        TEAM_BATTING_BB + TEAM_BATTING_H + TEAM_FIELDING_E + TEAM_PITCHING_H, data = TEMP ))

# CP Mallow Selection
cp_out <- leaps(x = TEMP[, 2:20], y = TEMP[, 1], names = names(TEMP)[2:20], method = "Cp", strictly.compatible = F)

# This line produces a vector with the difference between the CP value and the # of regressors
cp_diff <- abs(cp_out$Cp - cp_out$size)

# This line prints out the variables selected by the CP Mallow which had the least difference between 
# the CP Mallow value and the # of regresssors
cp_out$which[which(cp_diff == min(cp_diff)),]
cp_out$which[176,]


# create a data frame that removes the variables not used in the CP selected model
CP.TEMP <- TEMP
CP.TEMP$IMP_TEAM_PITCHING_SO <- NULL

# Now rerun model using lm with the CP variables
# Adjusted R-squared:  0.4069 
summary(Model1_CP <- lm(TARGET_WINS ~ ., data = CP.TEMP))


##################################################################################################
# Each of the models has variables that don't make sense - the signs are in the wrong direction  #
# Check for multicollinearity in the variables and rerun models                                  #
##################################################################################################

vif(Model1_step)

# Use call variable from model to get all variabls used to cut and paste into scatterplot matrix
Model1_step$call

pairs( ~ TEAM_BATTING_H + TEAM_BATTING_2B +
        TEAM_BATTING_3B + TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_PITCHING_H +
        TEAM_FIELDING_E + M_TEAM_BATTING_SO + IMP_TEAM_BATTING_SO +
        M_TEAM_BASERUN_SB + IMP_TEAM_BASERUN_SB + IMP_TEAM_PITCHING_SO +
        M_TEAM_FIELDING_DP + IMP_TEAM_FIELDING_DP, data = TEMP, main = "Scatterplot Matrix")

CORR.VAR <- subset(TEMP, select = c(TEAM_BATTING_H, TEAM_BATTING_2B,
                                    TEAM_BATTING_3B, TEAM_BATTING_HR, TEAM_BATTING_BB, TEAM_PITCHING_H,
                                    TEAM_FIELDING_E, M_TEAM_BATTING_SO, IMP_TEAM_BATTING_SO,
                                    M_TEAM_BASERUN_SB, IMP_TEAM_BASERUN_SB, IMP_TEAM_PITCHING_SO,
                                    M_TEAM_FIELDING_DP, IMP_TEAM_FIELDING_DP))

cor(CORR.VAR)

###########################
# Rerun Model 1 stepwise  #
###########################

# Remove M_TEAM_FIELDING_DP TEAM_BATTING_2B, TEAM_PITCHING_H, 
TEMP2S <- subset(TEMP, select = c(TARGET_WINS, TEAM_BATTING_H,IMP_TEAM_FIELDING_DP,
                               TEAM_BATTING_3B, TEAM_BATTING_HR, TEAM_BATTING_BB,
                               TEAM_FIELDING_E, M_TEAM_BATTING_SO, IMP_TEAM_BATTING_SO,
                               M_TEAM_BASERUN_SB, IMP_TEAM_BASERUN_SB, IMP_TEAM_PITCHING_SO))

# All variables
# Adjusted R-squared:  0.3814 
summary(Model1 <- lm(TARGET_WINS ~ ., data = TEMP2S))

# Backward Selection
# Adjusted R-squared:  0.3812 
summary(Model1_step <- step(Model1, direction = "both"))

cor(TEMP2S)


pairs( ~ TEAM_BATTING_H + TEAM_BATTING_2B +
        TEAM_BATTING_3B + TEAM_BATTING_HR +
        TEAM_FIELDING_E + M_TEAM_BATTING_SO +
        IMP_TEAM_FIELDING_DP, data = TEMP2, main = "Scatterplot Matrix")

##############################################
# Evaluate and rerun Model 1 - CP selection  #
# Backward selection produced same model as  #
# stepwise, so will eliminate that           #  
##############################################

vif(Model1_CP)

ld.varsF <- attributes(alias(Model1_CP)$Complete)$dimnames[[1]]
ld.varsF

# Use call variable from model to get all variabls used to cut and paste into scatterplot matrix
Model1_CP$call

pairs( ~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_BB +
      TEAM_PITCHING_H + TEAM_PITCHING_HR + TEAM_PITCHING_BB + TEAM_FIELDING_E + M_TEAM_BATTING_SO +
      IMP_TEAM_BATTING_SO + M_TEAM_BASERUN_SB + IMP_TEAM_BASERUN_SB + M_TEAM_BASERUN_CS + IMP_TEAM_BASERUN_CS +
       M_TEAM_FIELDING_DP + IMP_TEAM_FIELDING_DP, data = TEMP, main = "Scatterplot Matrix")

CORR.VAR <- subset(TEMP, select = c(TEAM_BATTING_H, TEAM_BATTING_2B, TEAM_BATTING_3B, TEAM_BATTING_HR, TEAM_BATTING_BB,
                                  TEAM_PITCHING_H, TEAM_PITCHING_HR, TEAM_PITCHING_BB, TEAM_FIELDING_E,
                                  M_TEAM_BATTING_SO, IMP_TEAM_BATTING_SO, M_TEAM_BASERUN_SB, IMP_TEAM_BASERUN_SB,
                                  M_TEAM_BASERUN_CS, IMP_TEAM_BASERUN_CS, M_TEAM_FIELDING_DP, IMP_TEAM_FIELDING_DP))

cor(CORR.VAR)


# Remove  M_TEAM_PITCHING_SO, TEAM_BATTING_HR, TEAM_BATTING_2B 
TEMP2CP <- subset(TEMP, select = c(TARGET_WINS, TEAM_BATTING_H, TEAM_BATTING_3B, TEAM_BATTING_BB,
                                 TEAM_PITCHING_H, TEAM_PITCHING_HR, TEAM_PITCHING_BB, TEAM_FIELDING_E,
                                 M_TEAM_BATTING_SO, IMP_TEAM_BATTING_SO, M_TEAM_BASERUN_SB, IMP_TEAM_BASERUN_SB,
                                 M_TEAM_BASERUN_CS, IMP_TEAM_BASERUN_CS, M_TEAM_FIELDING_DP,
                                 IMP_TEAM_FIELDING_DP))

length(TEMP2CP)

# CP Mallow Selection
cp_out <- leaps(x = TEMP2CP[, 2:16], y = TEMP2CP[, 1], names = names(TEMP)[2:16], method = "Cp", strictly.compatible = F)

# This line produces a vector with the difference between the CP value and the # of regressors
cp_diff <- abs(cp_out$Cp - cp_out$size)

# This line prints out the variables selected by the CP Mallow which had the least difference between 
# the CP Mallow value and the # of regresssors
cp_out$which[which(cp_diff == min(cp_diff)),]


# create a data frame that removes the variables not used in the CP selected model
CP.TEMP <- TEMP2CP
CP.TEMP$IMP_TEAM_PITCHING_SO <- NULL

# Now rerun model using lm with the CP variables
# Adjusted R-squared:  0.402 
summary(Model1_CP <- lm(TARGET_WINS ~ ., data = CP.TEMP))



##############################
# Now evaluate the 3 models  #
##############################


modelStats <- function(model, modelName) {

    Model <- modelName
    ANOVA <- anova(model)
    p <- length(model$coefficients)
    AIC <- round(AIC(model, k = p), 2)
    ADJRSQ <- round(summary(model)$adj.r.squared, 4)
    BIC <- round(stats::BIC(model), 2)
    MSE <- round(ANOVA$`Mean Sq`[length(ANOVA$`Mean Sq`)], 2)
    RMSE <- round(sqrt(ANOVA$`Mean Sq`[length(ANOVA$`Mean Sq`)]), 2)
    Cp <- round(((ANOVA$`Sum Sq`[length(ANOVA$`Sum Sq`)] * p) /
           (ANOVA$`Mean Sq`[length(ANOVA$`Mean Sq`)] * p)) - nrow(TEMP) + 2 * p, 2)

    df.ModelStats <- data.frame(Model, p, AIC, ADJRSQ, BIC, MSE, RMSE, Cp)
    colnames(df.ModelStats) <- c("Model", "NumParameters", "AIC", "AdjRSqaured", "BIC", "MSE", "RMSE", "CP")
    return(df.ModelStats)
}

df.Model1 <- modelStats(Model1, "Model1")
df.Model1_step <- modelStats(Model1_step, "Model1_Step")
df.Model1_cp <- modelStats(Model1_CP, "Model1_CP")
df.Model1_dt <- modelStats(Model1_dt, "Model1_dt")
all.model.stats <- rbind(df.Model1, df.Model1_step, df.Model1_cp, df.Model1_dt)

#####################################################################
# Start working on Model 2. Perform EDA on variables to determine   #
# if variable transformation is needed, to deal with outliers, and  #
# create calculated variables.	                                    #
#####################################################################

##############################
# Create derived variables  #
##############################
TEMP$SB_PCT[(TEMP$IMP_TEAM_BASERUN_SB + TEMP$IMP_TEAM_BASERUN_CS) == 0] <- 0
TEMP$SB_PCT[(TEMP$IMP_TEAM_BASERUN_SB + TEMP$IMP_TEAM_BASERUN_CS) != 0] <-
  TEMP$IMP_TEAM_BASERUN_SB[(TEMP$IMP_TEAM_BASERUN_SB + TEMP$IMP_TEAM_BASERUN_CS) != 0] /
  (TEMP$IMP_TEAM_BASERUN_SB[(TEMP$IMP_TEAM_BASERUN_SB + TEMP$IMP_TEAM_BASERUN_CS) != 0] +
     TEMP$IMP_TEAM_BASERUN_CS[(TEMP$IMP_TEAM_BASERUN_SB + TEMP$IMP_TEAM_BASERUN_CS) != 0])
TEMP$TWOB_PCT <- TEMP$TEAM_BATTING_2B / TEMP$TEAM_BATTING_H
TEMP$THREEB_PCT <- TEMP$TEAM_BATTING_3B / TEMP$TEAM_BATTING_H
TEMP$HR_PCT <- TEMP$TEAM_BATTING_HR / TEMP$TEAM_BATTING_H
TEMP$SO_PCT <- TEMP$IMP_TEAM_BATTING_SO / (TEMP$IMP_TEAM_BATTING_SO + TEMP$TEAM_BATTING_H)
TEMP$H_PCT <- TEMP$TEAM_BATTING_H / (TEMP$IMP_TEAM_BATTING_SO + TEMP$TEAM_BATTING_H)
TEMP$PHR_PCT <- TEMP$TEAM_PITCHING_HR / TEMP$TEAM_PITCHING_H

######################
# Identify Outliers  #
######################
lower <- function(x) {
    quantile(x, .25) - (1.5 * IQR(x))
}

upper <- function(x) {
    quantile(x, .75) + (1.5 * IQR(x))
}

extremel <- function(x) {
    quantile(x, .25) - (3 * IQR(x))
}

extremeu <- function(x) {
    quantile(x, .75) + (3 * IQR(x))
}


multi.fun <- function(x) {
    c(summary = summary(x), IQR = IQR(x), P1 = quantile(x, .01), P99 = quantile(x, .99),
    LOWER = lower(x), upper = upper(x), extremel = extremel(x), extremeu = extremeu(x))
}


summary.metric.data <- sapply(TEMP, multi.fun)
summary.metric.data



############################################################################
# Plots to visualize outliers - create new variables to deal with outliers #
############################################################################

# TARGET_WINS
boxplot(TEMP$TARGET_WINS, data = TEMP, main = "Boxplot: Wins for Baseball Teams",
        col = "gold")

TEMP$CAP_TARGET_WINS <- TEMP$TARGET_WINS
TEMP$CAP_TARGET_WINS[TEMP$TARGET_WINS < 38] <- 38
TEMP$CAP_TARGET_WINS[TEMP$TARGET_WINS > 123] <- 123
TEMP$TARGET_WINS <- NULL

boxplot(TEMP$CAP_TARGET_WINS, data = TEMP, main = "Boxplot: Wins for Baseball Teams",
        col = "gold")

hist(TEMP$CAP_TARGET_WINS, main = "Histogram: Wins for Baseball Teams", col = "gold",
     xlab = "CAP_TARGET_WINS")


# TEAM_PITCHING_H
boxplot(TEMP$TEAM_PITCHING_H, data = TEMP, main = "Boxplot: Wins for Baseball Teams",
        col = "gold")

TEMP$CAP_TEAM_PITCHING_H <- TEMP$TEAM_PITCHING_H
TEMP$CAP_TEAM_PITCHING_H[TEMP$TEAM_PITCHING_H > 2475] <- 2475

boxplot(TEMP$CAP_TEAM_PITCHING_H, data = TEMP, main = "Boxplot: Team Pitching Hits",
        col = "gold")

hist(TEMP$CAP_TEAM_PITCHING_H, main = "Histogram: Team Pitching Hits", col = "gold",
     xlab = "CAP_TEAM_PITCHING_H")

# Doesn't look normal - try a tranformation 
TEMP$R3_TEAM_PITCHING_H[TEMP$TEAM_PITCHING_H != 0] <- (1 / TEMP$TEAM_PITCHING_H[TEMP$TEAM_PITCHING_H != 0]) ** 3
TEMP$R3_TEAM_PITCHING_H[TEMP$TEAM_PITCHING_H == 0] <- 0

boxplot(TEMP$R3_TEAM_PITCHING_H, data = TEMP, main = "Boxplot: Team Pitching Hits (Transformed)",
        col = "gold")

hist(TEMP$R3_TEAM_PITCHING_H, main = "Histogram: Team Pitching Hits (Transformed)", col = "gold",
     xlab = "R3_TEAM_PITCHING_H")

TEMP$TEAM_PITCHING_H <- NULL

# IMP_TEAM_BASERUN_CS
boxplot(TEMP$IMP_TEAM_BASERUN_CS, data = TEMP, main = "Boxplot: Caught Stealing",
        col = "gold")

hist(TEMP$IMP_TEAM_BASERUN_CS, main = "Histogram: Caught Stealing", col = "gold",
     xlab = "IMP_TEAM_BASERUN_CS")

TEMP$L2_IMP_TEAM_BASERUN_CS[TEMP$IMP_TEAM_BASERUN_CS != 0] <-
          log(TEMP$IMP_TEAM_BASERUN_CS[TEMP$IMP_TEAM_BASERUN_CS != 0]) ** 2
TEMP$L2_IMP_TEAM_BASERUN_CS[TEMP$IMP_TEAM_BASERUN_CS == 0] <- 0

boxplot(TEMP$L2_IMP_TEAM_BASERUN_CS, data = TEMP, main = "Boxplot: Caught Stealing (Transformed)",
        col = "gold")

hist(TEMP$L2_IMP_TEAM_BASERUN_CS, main = "Histogram: Caught Stealing (Transformed)", col = "gold",
     xlab = "L2_IMP_TEAM_BASERUN_CS")


# Cap IMP_TEAM_BASERUN_CS at both ends of extreme;
TEMP$CAP_IMP_TEAM_BASERUN_CS <- TEMP$IMP_TEAM_BASERUN_CS
TEMP$CAP_IMP_TEAM_BASERUN_CS[TEMP$CAP_IMP_TEAM_BASERUN_CS > 86] <- 86
TEMP$CAP_IMP_TEAM_BASERUN_CS[TEMP$CAP_IMP_TEAM_BASERUN_CS < 12] <- 12

boxplot(TEMP$CAP_IMP_TEAM_BASERUN_CS, data = TEMP, main = "Boxplot: Caught Stealing (Capped)",
        col = "gold")

hist(TEMP$CAP_IMP_TEAM_BASERUN_CS, main = "Histogram: Caught Stealing (Capped)", col = "gold",
     xlab = "CAP_IMP_TEAM_BASERUN_CS")

TEMP$IMP_TEAM_BASERUN_CS <- NULL


# IMP_TEAM_BASERUN_SB didn't look normal so try a transformation
boxplot(TEMP$IMP_TEAM_BASERUN_SB, data = TEMP, main = "Boxplot: Stolen Bases",
        col = "gold")

hist(TEMP$IMP_TEAM_BASERUN_SB, main = "Histogram: Stolen Bases", col = "gold",
     xlab = "IMP_TEAM_BASERUN_CS")

TEMP$LOG_IMP_TEAM_BASERUN_SB[TEMP$IMP_TEAM_BASERUN_SB != 0] <-
                              log(TEMP$IMP_TEAM_BASERUN_SB[TEMP$IMP_TEAM_BASERUN_SB != 0])
TEMP$LOG_IMP_TEAM_BASERUN_SB[TEMP$IMP_TEAM_BASERUN_SB == 0] <- 0

boxplot(TEMP$LOG_IMP_TEAM_BASERUN_SB, data = TEMP, main = "Boxplot: Stolen Bases (Transformed)",
        col = "gold")

hist(TEMP$LOG_IMP_TEAM_BASERUN_SB, main = "Histogram: Stolen Bases (Transformed)", col = "gold",
     xlab = "LOG_IMP_TEAM_BASERUN_SB")


# Lower end of the range okay for TEAM_PITCHING_H so leave that alone. Cap at upper extreme;
TEMP$CAP_IMP_TEAM_BASERUN_SB <- TEMP$IMP_TEAM_BASERUN_SB;
TEMP$CAP_IMP_TEAM_BASERUN_SB[TEMP$CAP_IMP_TEAM_BASERUN_SB > 403] <- 403

boxplot(TEMP$CAP_IMP_TEAM_BASERUN_SB, data = TEMP, main = "Boxplot: Stolen Bases (Capped)",
        col = "gold")

hist(TEMP$CAP_IMP_TEAM_BASERUN_SB, main = "Histogram: Stolen Bases (Capped)", col = "gold",
     xlab = "CAP_IMP_TEAM_BASERUN_SB")

TEMP$IMP_TEAM_BASERUN_SB <- NULL

# IMP_TEAM_BATTING_SO looks fairly normal so don't mess with this too much. Cap at the lower extreme
boxplot(TEMP$IMP_TEAM_BATTING_SO, data = TEMP, main = "Boxplot: Strike Outs",
        col = "gold")

hist(TEMP$IMP_TEAM_BATTING_SO, main = "Histogram: Strike Outs", col = "gold",
     xlab = "IMP_TEAM_BATTING_SO")

TEMP$CAP_IMP_TEAM_BATTING_SO <- TEMP$IMP_TEAM_BATTING_SO
TEMP$CAP_IMP_TEAM_BATTING_SO[TEMP$CAP_IMP_TEAM_BATTING_SO < 4] <- 4

boxplot(TEMP$CAP_IMP_TEAM_BATTING_SO, data = TEMP, main = "Boxplot: Strike Outs (Capped)",
        col = "gold")

hist(TEMP$CAP_IMP_TEAM_BATTING_SO, main = "Histogram: Strike Outs (Capped)", col = "gold",
     xlab = "CAP_IMP_TEAM_BATTING_SO")

TEMP$IMP_TEAM_BATTING_SO <- NULL

# IMP_TEAM_PITCHING_SO has extreme outliers - cap at extreme value
boxplot(TEMP$IMP_TEAM_PITCHING_SO, data = TEMP, main = "Boxplot: Pitching Strike Outs",
        col = "gold")

hist(TEMP$IMP_TEAM_PITCHING_SO, main = "Histogram: Pitching Strike Outs", col = "gold",
     xlab = "IMP_TEAM_PITCHING_SO")

TEMP$CAP_IMP_TEAM_PITCHING_SO <- TEMP$IMP_TEAM_PITCHING_SO
TEMP$CAP_IMP_TEAM_PITCHING_SO[TEMP$CAP_IMP_TEAM_PITCHING_SO > 1950] <- 1950

boxplot(TEMP$CAP_IMP_TEAM_PITCHING_SO, data = TEMP, main = "Boxplot: Pitching Strike Outs (Capped)",
        col = "gold")

hist(TEMP$CAP_IMP_TEAM_PITCHING_SO, main = "Histogram: Pitching Strike Outs (Capped)", col = "gold",
     xlab = "CAP_IMP_TEAM_PITCHING_SO")

TEMP$IMP_TEAM_PITCHING_SO <- NULL

# TEAM_BATTING_3B didn't look normal so try a transformation
boxplot(TEMP$TEAM_BATTING_3B, data = TEMP, main = "Boxplot: Batting 3 Bases",
        col = "gold")

hist(TEMP$TEAM_BATTING_3B, main = "Histogram: Batting 3 Bases", col = "gold",
     xlab = "TEAM_BATTING_3B")

TEMP$L2_TEAM_BATTING_3B[TEMP$TEAM_BATTING_3B != 0] <-
                          log(TEMP$TEAM_BATTING_3B[TEMP$TEAM_BATTING_3B != 0]) ** 2
TEMP$L2_TEAM_BATTING_3B[TEMP$TEAM_BATTING_3B == 0] <- 0

boxplot(TEMP$L2_TEAM_BATTING_3B, data = TEMP, main = "Boxplot: Batting 3 Bases (Transformed)",
        col = "gold")

hist(TEMP$L2_TEAM_BATTING_3B, main = "Histogram: Batting 3 Bases (Transformed)", col = "gold",
     xlab = "L2_TEAM_BATTING_3B")


# Lower end of the range okay for TEAM_BATTING_3B so leave that alone. Cap at upper extreme
TEMP$CAP_TEAM_BATTING_3B <- TEMP$TEAM_BATTING_3B
TEMP$CAP_TEAM_BATTING_3B[TEMP$CAP_TEAM_BATTING_3B > 186] <- 186


boxplot(TEMP$CAP_TEAM_BATTING_3B, data = TEMP, main = "Boxplot: Batting 3 Bases (Capped)",
        col = "gold")

hist(TEMP$CAP_TEAM_BATTING_3B, main = "Histogram: Batting 3 Bases (Capped)", col = "gold",
     xlab = "CAP_TEAM_BATTING_3B")

TEMP$TEAM_BATTING_3B <- NULL

# TEAM_BATTING_HR - no outliers but right skewed. Try transformation
# Not sure this is an improvement so keep original variable
# TO DO: copy this in SAS code
boxplot(TEMP$TEAM_BATTING_HR, data = TEMP, main = "Boxplot: Batting Home Runs",
        col = "gold")

hist(TEMP$TEAM_BATTING_HR, main = "Histogram: Batting Home Runs", col = "gold",
     xlab = "TEAM_BATTING_HR")

TEMP$L3_TEAM_BATTING_HR[TEMP$TEAM_BATTING_HR != 0] <- log(TEMP$TEAM_BATTING_HR[TEMP$TEAM_BATTING_HR != 0]) ** 3
TEMP$L3_TEAM_BATTING_HR[TEMP$TEAM_BATTING_HR == 0] <- 0

boxplot(TEMP$L3_TEAM_BATTING_HR, data = TEMP, main = "Boxplot: Batting Home Runs (Transformed)",
        col = "gold")

hist(TEMP$L3_TEAM_BATTING_HR, main = "Histogram: Batting Home Runs (Transformed)", col = "gold",
     xlab = "L3_TEAM_BATTING_HR")


# TEAM_FIELDING_E didn't look normal so try a transformation;
boxplot(TEMP$TEAM_FIELDING_E, data = TEMP, main = "Boxplot: Fielding Errors",
        col = "gold")

hist(TEMP$TEAM_FIELDING_E, main = "Histogram:Fielding Errors", col = "gold",
     xlab = "TEAM_BATTING_HR")

TEMP$REC_TEAM_FIELDING_E <- 1 / (TEMP$TEAM_FIELDING_E)

boxplot(TEMP$REC_TEAM_FIELDING_E, data = TEMP, main = "Boxplot: Fielding Errors (Transformed)",
        col = "gold")

hist(TEMP$REC_TEAM_FIELDING_E, main = "Histogram:Fielding Errors (Transformed)", col = "gold",
     xlab = "REC_TEAM_FIELDING_E")

# Lower end of the range okay for TEAM_FIELDING_E so leave that alone. Cap at upper extreme;
TEMP$CAP_TEAM_FIELDING_E <- TEMP$TEAM_FIELDING_E
TEMP$CAP_TEAM_FIELDING_E[TEMP$CAP_TEAM_FIELDING_E > 617] <- 617

boxplot(TEMP$CAP_TEAM_FIELDING_E, data = TEMP, main = "Boxplot: Fielding Errors (Capped)",
        col = "gold")

hist(TEMP$CAP_TEAM_FIELDING_E, main = "Histogram:Fielding Errors (Capped)", col = "gold",
     xlab = "CAP_TEAM_FIELDING_E")

TEMP$TEAM_FIELDING_E <- NULL

# TEAM_PITCHING_BB has extreme outliers. Try capping
boxplot(TEMP$TEAM_PITCHING_BB, data = TEMP, main = "Boxplot: Pitching - walks allowed",
        col = "gold")

hist(TEMP$TEAM_PITCHING_BB, main = "Histogram: Pitching - walks allowed", col = "gold",
     xlab = "TEAM_PITCHING_BB")

TEMP$CAP_TEAM_PITCHING_BB <- TEMP$TEAM_PITCHING_BB
TEMP$CAP_TEAM_PITCHING_BB[TEMP$CAP_TEAM_PITCHING_BB > 1016] <- 1016
TEMP$CAP_TEAM_PITCHING_BB[TEMP$CAP_TEAM_PITCHING_BB < 71] <- 71

boxplot(TEMP$CAP_TEAM_PITCHING_BB, data = TEMP, main = "Boxplot: Pitching - walks allowed (Capped)",
        col = "gold")

hist(TEMP$CAP_TEAM_PITCHING_BB, main = "Histogram: Pitching - walks allowed (Capped)", col = "gold",
     xlab = "CAP_TEAM_PITCHING_BB")

TEMP$TEAM_PITCHING_BB <- NULL


# Outliers at the upper end of TEAM_PITCHING_HR  Try capping at upper;
boxplot(TEMP$TEAM_PITCHING_HR, data = TEMP, main = "Boxplot: Pitching - Homeruns allowed",
        col = "gold")

hist(TEMP$TEAM_PITCHING_HR, main = "Histogram: Pitching - Homeruns allowed", col = "gold",
     xlab = "TEAM_PITCHING_HR")

TEMP$CAP_TEAM_PITCHING_HR <- TEMP$TEAM_PITCHING_HR
TEMP$CAP_TEAM_PITCHING_HR[TEMP$CAP_TEAM_PITCHING_HR > 300] <- 300

boxplot(TEMP$TEAM_PITCHING_HR, data = TEMP, main = "Boxplot: Pitching - Homeruns allowed",
        col = "gold")

hist(TEMP$TEAM_PITCHING_HR, main = "Histogram: Pitching - Homeruns allowed", col = "gold",
     xlab = "TEAM_PITCHING_HR")

TEMP$TEAM_PITCHING_HR <- NULL


######################################
# Changes finished - check the data  #
######################################
head(TEMP)


#############################################################
# Create Model 2 using three different selection methods  	#
#############################################################;


# All variables
# Adjusted R-squared:  0.4088 
summary(Model2 <- lm(CAP_TARGET_WINS ~ ., data = TEMP))

# Backward Selection
# Adjusted R-squared:  0.4091
summary(Model2_back <- step(Model2, direction = "back"))


# Commenting this out as it produced the same model as backward selectoin
# Stepwise Selection
# Adjusted R-squared:   0.4157
#summary(Model2_step <- step(Model2, direction = "both"))

# Decision Tree Variable Selection
#rattle()

summary(Model2_dt <- lm(CAP_TARGET_WINS~ CAP_IMP_TEAM_PITCHING_SO + CAP_TEAM_FIELDING_E + LOG_IMP_TEAM_BASERUN_SB + R3_TEAM_PITCHING_H +
                         TEAM_BATTING_BB + TEAM_BATTING_H, data = TEMP))

length(TEMP)
summary(TEMP)

# CP Mallow Selection
# Was getting a vague error that wasn't defined anywhere. When I removed all the log variables it ran fine
# Maybe this was a problem with perfect collinearity
TEMP2 <- TEMP
TEMP2$L3_TEAM_BATTING_HR <- NULL
TEMP2$L2_IMP_TEAM_BASERUN_CS <- NULL
TEMP2$R3_TEAM_PITCHING_H <- NULL
TEMP2$REC_TEAM_FIELDING_E <- NULL
TEMP2$L2_TEAM_BATTING_3B <- NULL
TEMP2$LOG_IMP_TEAM_BASERUN_SB <- NULL
length(TEMP2)
cp_out <- leaps(x = TEMP2[, c(2:(which(colnames(TEMP2) == "CAP_TARGET_WINS") - 1), (which(colnames(TEMP2) == "CAP_TARGET_WINS") + 1):27)],
                 y = TEMP[, which(colnames(TEMP2) == "CAP_TARGET_WINS")],
                 names = names(TEMP2)[c(2:(which(colnames(TEMP2) == "CAP_TARGET_WINS") - 1), (which(colnames(TEMP2) == "CAP_TARGET_WINS") + 1):27)],
                 method = "Cp", strictly.compatible = F, nbest = 1)


# This line produces a vector with the difference between the CP value and the # of regressors
cp_diff <- abs(cp_out$Cp - cp_out$size)

# This line prints out the variables selected by the CP Mallow which had the least difference between 
# the CP Mallow value and the # of regresssors
cp_out$which[which(cp_diff == min(cp_diff)),]

# create a data frame that removes the variables not used in the CP selected model
CP.TEMP <- TEMP2
CP.TEMP$TEAM_BATTING_HR <- NULL
CP.TEMP$M_TEAM_BATTING_SO <- NULL
CP.TEMP$M_TEAM_BASERUN_CS <- NULL
CP.TEMP$H_PCT <- NULL
CP.TEMP$CAP_TEAM_PITCHING_H <- NULL
CP.TEMP$CAP_IMP_TEAM_PITCHING_SO <- NULL
CP.TEMP$CAP_TEAM_PITCHING_BB <- NULL
CP.TEMP$CAP_TEAM_PITCHING_HR <- NULL

summary(CP.TEMP)

# Now rerun model using lm with the CP variables
# Adjusted R-squared:  0.4042 
summary(Model2_CP <- lm(CAP_TARGET_WINS ~ ., data = CP.TEMP))


##################################################################################################
# Each of the models has variables that don't make sense - the signs are in the wrong direction  #
# Check for multicollinearity in the variables and rerun models                                  #
##################################################################################################

vif(Model2_back)

# Use call variable from model to get all variabls used to cut and paste into scatterplot matrix
Model2_back$call

pairs( ~ TEAM_BATTING_H + TEAM_BATTING_HR +
        TEAM_BATTING_BB + M_TEAM_BATTING_SO + M_TEAM_BASERUN_SB +
        M_TEAM_BASERUN_CS + M_TEAM_FIELDING_DP + IMP_TEAM_FIELDING_DP +
        THREEB_PCT + HR_PCT + SO_PCT + PHR_PCT + R3_TEAM_PITCHING_H +
        CAP_IMP_TEAM_BASERUN_CS + LOG_IMP_TEAM_BASERUN_SB + CAP_IMP_TEAM_BASERUN_SB +
        CAP_IMP_TEAM_BATTING_SO + L3_TEAM_BATTING_HR + REC_TEAM_FIELDING_E +
        CAP_TEAM_FIELDING_E + CAP_TEAM_PITCHING_BB + CAP_TEAM_PITCHING_HR, data = TEMP, main = "Scatterplot Matrix")

CORR.VAR <- subset(TEMP, select = c(CAP_TARGET_WINS, TEAM_BATTING_H,
                                  TEAM_BATTING_BB, M_TEAM_BATTING_SO, M_TEAM_BASERUN_SB,
                                  M_TEAM_BASERUN_CS, M_TEAM_FIELDING_DP, IMP_TEAM_FIELDING_DP,
                                  THREEB_PCT, SO_PCT, R3_TEAM_PITCHING_H, HR_PCT,
                                  CAP_IMP_TEAM_BASERUN_CS, CAP_IMP_TEAM_BASERUN_SB,
                                  CAP_IMP_TEAM_BATTING_SO, CAP_TEAM_FIELDING_E,
                                  CAP_TEAM_PITCHING_BB, CAP_TEAM_PITCHING_HR))

cor(CORR.VAR)

###########################
# Rerun Model 2 BACKWARD  #
###########################
# .4022
# Remove PHR_PCT, LOG_IMP_TEAM_BASERUN_SB ,  L3_TEAM_BATTING_HR , REC_TEAM_FIELDING_E , CAP_TEAM_PITCHING_HR ,SO_PCT,R3_TEAM_PITCHING_H,
TEMP2B <- subset(TEMP, select = c(CAP_TARGET_WINS, TEAM_BATTING_H,
                                TEAM_BATTING_BB, M_TEAM_BATTING_SO, M_TEAM_BASERUN_SB,
                                M_TEAM_BASERUN_CS, M_TEAM_FIELDING_DP, IMP_TEAM_FIELDING_DP,
                                THREEB_PCT,   HR_PCT,
                                CAP_IMP_TEAM_BASERUN_CS, CAP_IMP_TEAM_BASERUN_SB,
                                CAP_IMP_TEAM_BATTING_SO, CAP_TEAM_FIELDING_E,
                                 CAP_TEAM_PITCHING_BB))

# All variables
# Adjusted R-squared: .3891
summary(Model2_back <- lm(CAP_TARGET_WINS ~ ., data = TEMP2B))

##############################################
# Evaluate and rerun Model 2 - CP selection  #
# Backward selection produced same model as  #
# stepwise, so will eliminate that           #  
##############################################

vif(Model2_CP)

# Use call variable from model to get all variabls used to cut and paste into scatterplot matrix
Model2_CP$call

pairs( ~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_BB + M_TEAM_BASERUN_SB + M_TEAM_PITCHING_SO +
      M_TEAM_FIELDING_DP + IMP_TEAM_FIELDING_DP + SB_PCT + TWOB_PCT + THREEB_PCT + HR_PCT + SO_PCT +
      PHR_PCT + CAP_IMP_TEAM_BASERUN_CS + CAP_IMP_TEAM_BASERUN_SB + CAP_IMP_TEAM_BATTING_SO +
      CAP_TEAM_BATTING_3B + CAP_TEAM_FIELDING_E, data = TEMP, main = "Scatterplot Matrix")

CORR.VAR <- subset(TEMP, select = c(CAP_TARGET_WINS, TEAM_BATTING_H, TEAM_BATTING_BB, M_TEAM_BASERUN_SB,
                                 M_TEAM_PITCHING_SO, M_TEAM_FIELDING_DP, IMP_TEAM_FIELDING_DP, SB_PCT,
                                  THREEB_PCT, CAP_IMP_TEAM_BASERUN_CS, PHR_PCT, TEAM_BATTING_2B,
                                 CAP_IMP_TEAM_BASERUN_SB, CAP_IMP_TEAM_BATTING_SO,
                                 CAP_TEAM_FIELDING_E))

cor(CORR.VAR)


# Remove  HR_PCT, CAP_TEAM_BATTING_3B,TWOB_PCT,SO_PCT,M_TEAM_FIELDING_DP,
TEMP2CP <- subset(TEMP, select = c(CAP_TARGET_WINS, TEAM_BATTING_H, TEAM_BATTING_BB, M_TEAM_BASERUN_SB,
                                 M_TEAM_PITCHING_SO,  SB_PCT,IMP_TEAM_FIELDING_DP,
                                  THREEB_PCT, CAP_IMP_TEAM_BASERUN_CS, PHR_PCT, TEAM_BATTING_2B,
                                 CAP_IMP_TEAM_BASERUN_SB, CAP_IMP_TEAM_BATTING_SO,
                                 CAP_TEAM_FIELDING_E))

# Now rerun model using lm with the CP variables
# Adjusted R-squared:  0.3869
summary(Model2_CP <- lm(CAP_TARGET_WINS ~ ., data = TEMP2CP))

vif(Model2_CP)


#######################
# Evaluate the Models #
#######################


df.Model2 <- modelStats(Model2, "Model2")
df.Model2_back <- modelStats(Model2_back, "Model2_back")
df.Model2_cp <- modelStats(Model2_CP, "Model2_CP")
df.Model2_dt <- modelStats(Model2_dt, "Model2_dt")
all.model.stats <- rbind(all.model.stats, df.Model2, df.Model2_back, df.Model2_cp, df.Model2_dt)
all.model.stats <- all.model.stats[order(all.model.stats$AIC),]
all.model.stats


#############################################################
# Start working on Model 3. For this model I used decision  #
# trees to impute missing values and select variables       #
#############################################################

TEMP3 <- BB
head(TEMP3)

# Use rattle() for GUI interface
#rattle()

############################################################################################################
# Impute TEAM_BATTING_SO  - This was a little tricky. The original tree used TEAM_PITCHING_SO as the root  #
# node, but it had exactly the same missing values so had to exlude that variable from the analysis.       #
############################################################################################################
TEMP3$M_TEAM_BATTING_SO <- 0
TEMP3$M_TEAM_BATTING_SO[is.na(TEMP3$TEAM_BATTING_SO)] <- 1

TEMP3$IMP_TEAM_BATTING_SO <- TEMP3$TEAM_BATTING_SO
TEMP3$IMP_TEAM_BATTING_SO[TEMP3$TEAM_FIELDING_E < 161.5 & TEMP3$TEAM_BATTING_HR >= 122.5 & is.na(TEMP3$IMP_TEAM_BATTING_SO)] <- 944.49
TEMP3$IMP_TEAM_BATTING_SO[TEMP3$TEAM_FIELDING_E >= 161.5 & TEMP3$TEAM_PITCHING_H >= 1565 & is.na(TEMP3$IMP_TEAM_BATTING_SO)] <- 472.50
TEMP3$IMP_TEAM_BATTING_SO[TEMP3$TEAM_FIELDING_E < 161.5 & TEMP3$TEAM_BATTING_HR < 122.5 & is.na(TEMP3$IMP_TEAM_BATTING_SO)] <- 804.24
TEMP3$IMP_TEAM_BATTING_SO[TEMP3$TEAM_FIELDING_E >= 161.5 & TEMP3$TEAM_PITCHING_H < 1565 & is.na(TEMP3$IMP_TEAM_BATTING_SO)] <- 679.42

TEMP3$TEAM_BATTING_SO <- NULL

summary(TEMP3$IMP_TEAM_BATTING_SO)


###########################
# Impute TEAM_BASERUN_SB  #
###########################
TEMP3$M_TEAM_BASERUN_SB <- 0
TEMP3$M_TEAM_BASERUN_SB[is.na(TEMP3$TEAM_BASERUN_SB)] <- 1

TEMP3$IMP_TEAM_BASERUN_SB <- TEMP3$TEAM_BASERUN_SB
TEMP3$IMP_TEAM_BASERUN_SB[TEMP3$TEAM_FIELDING_E < 255.5 & TEMP3$TEAM_PITCHING_HR >= 33.5 & is.na(TEMP3$IMP_TEAM_BASERUN_SB)] <- 91.52
TEMP3$IMP_TEAM_BASERUN_SB[TEMP3$TEAM_FIELDING_E < 255.5 & TEMP3$TEAM_PITCHING_HR < 33.5 & is.na(TEMP3$IMP_TEAM_BASERUN_SB)] <- 164.59
TEMP3$IMP_TEAM_BASERUN_SB[TEMP3$TEAM_FIELDING_E >= 255.5 & TEMP3$TEAM_PITCHING_BB < 531.5 & is.na(TEMP3$IMP_TEAM_BASERUN_SB)] <- 191.26
TEMP3$IMP_TEAM_BASERUN_SB[TEMP3$TEAM_FIELDING_E >= 255.5 & TEMP3$TEAM_PITCHING_BB >= 531.5 & is.na(TEMP3$IMP_TEAM_BASERUN_SB)] <- 294.85

TEMP3$TEAM_BASERUN_SB <- NULL

summary(TEMP3$IMP_TEAM_BASERUN_SB)


###########################
# Impute TEAM_BASERUN_CS  #
###########################

TEMP3$M_TEAM_BASERUN_CS <- 0
TEMP3$M_TEAM_BASERUN_CS[is.na(TEMP3$TEAM_BASERUN_CS)] <- 1

TEMP3$IMP_TEAM_BASERUN_CS <- TEMP3$TEAM_BASERUN_CS
TEMP3$IMP_TEAM_BASERUN_CS[TEMP3$IMP_TEAM_BASERUN_SB >= 91.5 & TEMP3$TEAM_PITCHING_HR >= 55.5 & is.na(TEMP3$IMP_TEAM_BASERUN_CS)] <- 60.39
TEMP3$IMP_TEAM_BASERUN_CS[TEMP3$IMP_TEAM_BASERUN_SB < 91.5 & TEMP3$IMP_TEAM_BASERUN_SB >= 47.5 & is.na(TEMP3$IMP_TEAM_BASERUN_CS)] <- 45.13
TEMP3$IMP_TEAM_BASERUN_CS[TEMP3$IMP_TEAM_BASERUN_SB < 91.5 & TEMP3$IMP_TEAM_BASERUN_SB < 47.5 & is.na(TEMP3$IMP_TEAM_BASERUN_CS)] <- 31
TEMP3$IMP_TEAM_BASERUN_CS[TEMP3$IMP_TEAM_BASERUN_SB >= 91.5 & TEMP3$TEAM_PITCHING_HR < 55.5 & is.na(TEMP3$IMP_TEAM_BASERUN_CS)] <- 103.72

TEMP3$TEAM_BASERUN_CS <- NULL

summary(TEMP3$IMP_TEAM_BASERUN_CS)


##############################################################################################
# Impute TEAM_BATTING_HBP - Too many missing values so delete but create missing value flag  #
##############################################################################################

TEMP3$M_TEAM_BATTING_HBP <- 0
TEMP3$M_TEAM_BATTING_HBP[is.na(TEMP3$TEAM_BATTING_HBP)] <- 1

TEMP3$TEAM_BATTING_HBP <- NULL


###########################
# Impute TEAM_PITCHING_SO #
###########################

TEMP3$M_TEAM_PITCHING_SO <- 0
TEMP3$M_TEAM_PITCHING_SO[is.na(TEMP3$TEAM_PITCHING_SO)] <- 1

TEMP3$IMP_TEAM_PITCHING_SO <- TEMP3$TEAM_PITCHING_SO
TEMP3$IMP_TEAM_PITCHING_SO[TEMP3$IMP_TEAM_BATTING_SO >= 723 & TEMP3$TEAM_PITCHING_H < 1954 & is.na(TEMP3$IMP_TEAM_PITCHING_SO)] <- 949.04
TEMP3$IMP_TEAM_PITCHING_SO[TEMP3$IMP_TEAM_BATTING_SO < 723 & TEMP3$IMP_TEAM_BATTING_SO < 538 & is.na(TEMP3$IMP_TEAM_PITCHING_SO)] <- 514.45
TEMP3$IMP_TEAM_PITCHING_SO[TEMP3$IMP_TEAM_BATTING_SO < 723 & TEMP3$IMP_TEAM_BATTING_SO >= 538 & is.na(TEMP3$IMP_TEAM_PITCHING_SO)] <- 702.30
TEMP3$IMP_TEAM_PITCHING_SO[TEMP3$IMP_TEAM_BATTING_SO >= 723 & TEMP3$TEAM_PITCHING_H >= 1954 & is.na(TEMP3$IMP_TEAM_PITCHING_SO)] <- 2172.62

TEMP3$TEAM_PITCHING_SO <- NULL

summary(TEMP3$IMP_TEAM_PITCHING_SO)



###########################
# Impute TEAM_FIELDING_DP #
###########################

TEMP3$M_TEAM_FIELDING_DP <- 0
TEMP3$M_TEAM_FIELDING_DP[is.na(TEMP3$TEAM_FIELDING_DP)] <- 1

TEMP3$IMP_TEAM_FIELDING_DP <- TEMP3$TEAM_FIELDING_DP
TEMP3$IMP_TEAM_FIELDING_DP[TEMP3$TEAM_FIELDING_E < 244.5 & TEMP3$IMP_TEAM_BASERUN_SB < 120.5 & is.na(TEMP3$IMP_TEAM_FIELDING_DP)] <- 156.94
TEMP3$IMP_TEAM_FIELDING_DP[TEMP3$TEAM_FIELDING_E < 244.5 & TEMP3$IMP_TEAM_BASERUN_SB > 120.5 & is.na(TEMP3$IMP_TEAM_FIELDING_DP)] <- 144.62
TEMP3$IMP_TEAM_FIELDING_DP[TEMP3$TEAM_FIELDING_E >= 244.5 & TEMP3$TEAM_BATTING_HR < 33.5 & is.na(TEMP3$IMP_TEAM_FIELDING_DP)] <- 105.37
TEMP3$IMP_TEAM_FIELDING_DP[TEMP3$TEAM_FIELDING_E >= 244.5 & TEMP3$TEAM_BATTING_HR >= 33.5 & is.na(TEMP3$IMP_TEAM_FIELDING_DP)] <- 121.35

TEMP3$TEAM_FIELDING_DP <- NULL

summary(TEMP3$IMP_TEAM_FIELDING_DP)

summary(TEMP3)

##############################
# Create derived variables   #
##############################

TEMP3$SB_PCT[(TEMP3$IMP_TEAM_BASERUN_SB + TEMP3$IMP_TEAM_BASERUN_CS) == 0] <- 0
TEMP3$SB_PCT[(TEMP3$IMP_TEAM_BASERUN_SB + TEMP3$IMP_TEAM_BASERUN_CS) != 0] <-
  TEMP3$IMP_TEAM_BASERUN_SB[(TEMP3$IMP_TEAM_BASERUN_SB + TEMP3$IMP_TEAM_BASERUN_CS) != 0] /
  (TEMP3$IMP_TEAM_BASERUN_SB[(TEMP3$IMP_TEAM_BASERUN_SB + TEMP3$IMP_TEAM_BASERUN_CS) != 0] +
     TEMP3$IMP_TEAM_BASERUN_CS[(TEMP3$IMP_TEAM_BASERUN_SB + TEMP3$IMP_TEAM_BASERUN_CS) != 0])
TEMP3$TWOB_PCT <- TEMP3$TEAM_BATTING_2B / TEMP3$TEAM_BATTING_H
TEMP3$THREEB_PCT <- TEMP3$TEAM_BATTING_3B / TEMP3$TEAM_BATTING_H
TEMP3$HR_PCT <- TEMP3$TEAM_BATTING_HR / TEMP3$TEAM_BATTING_H
TEMP3$SO_PCT <- TEMP3$IMP_TEAM_BATTING_SO / (TEMP3$IMP_TEAM_BATTING_SO + TEMP3$TEAM_BATTING_H)
TEMP3$H_PCT <- TEMP3$TEAM_BATTING_H / (TEMP3$IMP_TEAM_BATTING_SO + TEMP3$TEAM_BATTING_H)
TEMP3$PHR_PCT <- TEMP3$TEAM_PITCHING_HR / TEMP3$TEAM_PITCHING_H


######################
# Identify Outliers  #
######################

summary.metric.data <- sapply(TEMP3, multi.fun)
summary.metric.data

###########################################
# Create Capped and Transformed Variables #
###########################################

# TARGET_WINS
TEMP3$CAP_TARGET_WINS <- TEMP3$TARGET_WINS
TEMP3$CAP_TARGET_WINS[TEMP3$TARGET_WINS < 38] <- 38
TEMP3$CAP_TARGET_WINS[TEMP3$TARGET_WINS > 123] <- 123
TEMP3$TARGET_WINS <- NULL


# TEAM_PITCHING_H
TEMP3$CAP_TEAM_PITCHING_H <- TEMP3$TEAM_PITCHING_H
TEMP3$CAP_TEAM_PITCHING_H[TEMP3$TEAM_PITCHING_H > 2475] <- 2475

# Doesn't look normal - try a tranformation 
TEMP3$R3_TEAM_PITCHING_H[TEMP3$TEAM_PITCHING_H != 0] <- (1 / TEMP3$TEAM_PITCHING_H[TEMP3$TEAM_PITCHING_H != 0]) ** 3
TEMP3$R3_TEAM_PITCHING_H[TEMP3$TEAM_PITCHING_H == 0] <- 0

TEMP3$TEAM_PITCHING_H <- NULL

# IMP_TEAM_BASERUN_CS
boxplot(TEMP3$IMP_TEAM_BASERUN_CS, data = TEMP3, main = "Boxplot: Caught Stealing",
        col = "gold")

hist(TEMP3$IMP_TEAM_BASERUN_CS, main = "Histogram: Caught Stealing", col = "gold",
     xlab = "IMP_TEAM_BASERUN_CS")

TEMP3$L2_IMP_TEAM_BASERUN_CS[TEMP3$IMP_TEAM_BASERUN_CS != 0] <-
  log(TEMP3$IMP_TEAM_BASERUN_CS[TEMP3$IMP_TEAM_BASERUN_CS != 0]) ** 2
TEMP3$L2_IMP_TEAM_BASERUN_CS[TEMP3$IMP_TEAM_BASERUN_CS == 0] <- 0

boxplot(TEMP3$L2_IMP_TEAM_BASERUN_CS, data = TEMP3, main = "Boxplot: Caught Stealing (Transformed)",
        col = "gold")

hist(TEMP3$L2_IMP_TEAM_BASERUN_CS, main = "Histogram: Caught Stealing (Transformed)", col = "gold",
     xlab = "L2_IMP_TEAM_BASERUN_CS")

# This transformation no longer makes sense with the way the data looks with imputed variables
# Make this change in SAS
TEMP3$L2_IMP_TEAM_BASERUN_CS <- NULL

# Cap IMP_TEAM_BASERUN_CS at upper bound;
TEMP3$CAP_IMP_TEAM_BASERUN_CS <- TEMP3$IMP_TEAM_BASERUN_CS
TEMP3$CAP_IMP_TEAM_BASERUN_CS[TEMP3$CAP_IMP_TEAM_BASERUN_CS > 155.125] <- 155.125


boxplot(TEMP3$CAP_IMP_TEAM_BASERUN_CS, data = TEMP3, main = "Boxplot: Caught Stealing (Capped)",
        col = "gold")

hist(TEMP3$CAP_IMP_TEAM_BASERUN_CS, main = "Histogram: Caught Stealing (Capped)", col = "gold",
     xlab = "CAP_IMP_TEAM_BASERUN_CS")

TEMP3$IMP_TEAM_BASERUN_CS <- NULL


# IMP_TEAM_BASERUN_SB didn't look normal so try a transformation
boxplot(TEMP3$IMP_TEAM_BASERUN_SB, data = TEMP3, main = "Boxplot: Stolen Bases",
        col = "gold")

hist(TEMP3$IMP_TEAM_BASERUN_SB, main = "Histogram: Stolen Bases", col = "gold",
     xlab = "IMP_TEAM_BASERUN_CS")

TEMP3$LOG_IMP_TEAM_BASERUN_SB[TEMP3$IMP_TEAM_BASERUN_SB != 0] <-
  log(TEMP3$IMP_TEAM_BASERUN_SB[TEMP3$IMP_TEAM_BASERUN_SB != 0])
TEMP3$LOG_IMP_TEAM_BASERUN_SB[TEMP3$IMP_TEAM_BASERUN_SB == 0] <- 0

boxplot(TEMP3$LOG_IMP_TEAM_BASERUN_SB, data = TEMP3, main = "Boxplot: Stolen Bases (Transformed)",
        col = "gold")

hist(TEMP3$LOG_IMP_TEAM_BASERUN_SB, main = "Histogram: Stolen Bases (Transformed)", col = "gold",
     xlab = "LOG_IMP_TEAM_BASERUN_SB")


# Lower end of the range okay for IMP_TEAM_BASERUN_SB so leave that alone. Cap at upper extreme;
TEMP3$CAP_IMP_TEAM_BASERUN_SB <- TEMP3$IMP_TEAM_BASERUN_SB;
TEMP3$CAP_IMP_TEAM_BASERUN_SB[TEMP3$CAP_IMP_TEAM_BASERUN_SB > 403] <- 403

boxplot(TEMP3$CAP_IMP_TEAM_BASERUN_SB, data = TEMP3, main = "Boxplot: Stolen Bases (Capped)",
        col = "gold")

hist(TEMP3$CAP_IMP_TEAM_BASERUN_SB, main = "Histogram: Stolen Bases (Capped)", col = "gold",
     xlab = "CAP_IMP_TEAM_BASERUN_SB")

TEMP3$IMP_TEAM_BASERUN_SB <- NULL


# IMP_TEAM_BATTING_SO looks fairly normal so don't mess with this too much. Cap at the lower extreme
boxplot(TEMP3$IMP_TEAM_BATTING_SO, data = TEMP3, main = "Boxplot: Strike Outs",
        col = "gold")

hist(TEMP3$IMP_TEAM_BATTING_SO, main = "Histogram: Strike Outs", col = "gold",
     xlab = "IMP_TEAM_BATTING_SO")

TEMP3$CAP_IMP_TEAM_BATTING_SO <- TEMP3$IMP_TEAM_BATTING_SO
TEMP3$CAP_IMP_TEAM_BATTING_SO[TEMP3$CAP_IMP_TEAM_BATTING_SO < 4] <- 4

boxplot(TEMP3$CAP_IMP_TEAM_BATTING_SO, data = TEMP3, main = "Boxplot: Strike Outs (Capped)",
        col = "gold")

hist(TEMP3$CAP_IMP_TEAM_BATTING_SO, main = "Histogram: Strike Outs (Capped)", col = "gold",
     xlab = "CAP_IMP_TEAM_BATTING_SO")

TEMP3$IMP_TEAM_BATTING_SO <- NULL

# IMP_TEAM_PITCHING_SO has extreme outliers - cap at extreme value
boxplot(TEMP3$IMP_TEAM_PITCHING_SO, data = TEMP3, main = "Boxplot: Pitching Strike Outs",
        col = "gold")

hist(TEMP3$IMP_TEAM_PITCHING_SO, main = "Histogram: Pitching Strike Outs", col = "gold",
     xlab = "IMP_TEAM_PITCHING_SO")

TEMP3$CAP_IMP_TEAM_PITCHING_SO <- TEMP3$IMP_TEAM_PITCHING_SO
TEMP3$CAP_IMP_TEAM_PITCHING_SO[TEMP3$CAP_IMP_TEAM_PITCHING_SO > 1950] <- 1950

boxplot(TEMP3$CAP_IMP_TEAM_PITCHING_SO, data = TEMP3, main = "Boxplot: Pitching Strike Outs (Capped)",
        col = "gold")

hist(TEMP3$CAP_IMP_TEAM_PITCHING_SO, main = "Histogram: Pitching Strike Outs (Capped)", col = "gold",
     xlab = "CAP_IMP_TEAM_PITCHING_SO")

TEMP3$IMP_TEAM_PITCHING_SO <- NULL

# TEAM_BATTING_3B didn't look normal so try a transformation
boxplot(TEMP3$TEAM_BATTING_3B, data = TEMP3, main = "Boxplot: Batting 3 Bases",
        col = "gold")

hist(TEMP3$TEAM_BATTING_3B, main = "Histogram: Batting 3 Bases", col = "gold",
     xlab = "TEAM_BATTING_3B")

TEMP3$L2_TEAM_BATTING_3B[TEMP3$TEAM_BATTING_3B != 0] <-
  log(TEMP3$TEAM_BATTING_3B[TEMP3$TEAM_BATTING_3B != 0]) ** 2
TEMP3$L2_TEAM_BATTING_3B[TEMP3$TEAM_BATTING_3B == 0] <- 0

boxplot(TEMP3$L2_TEAM_BATTING_3B, data = TEMP3, main = "Boxplot: Batting 3 Bases (Transformed)",
        col = "gold")

hist(TEMP3$L2_TEAM_BATTING_3B, main = "Histogram: Batting 3 Bases (Transformed)", col = "gold",
     xlab = "L2_TEAM_BATTING_3B")


# Lower end of the range okay for TEAM_BATTING_3B so leave that alone. Cap at upper extreme
TEMP3$CAP_TEAM_BATTING_3B <- TEMP3$TEAM_BATTING_3B
TEMP3$CAP_TEAM_BATTING_3B[TEMP3$CAP_TEAM_BATTING_3B > 186] <- 186


boxplot(TEMP3$CAP_TEAM_BATTING_3B, data = TEMP3, main = "Boxplot: Batting 3 Bases (Capped)",
        col = "gold")

hist(TEMP3$CAP_TEAM_BATTING_3B, main = "Histogram: Batting 3 Bases (Capped)", col = "gold",
     xlab = "CAP_TEAM_BATTING_3B")

TEMP3$TEAM_BATTING_3B <- NULL

# TEAM_BATTING_HR - no outliers but right skewed. Try transformation
# Not sure this is an improvement so keep original variable
# TO DO: copy this in SAS code
boxplot(TEMP3$TEAM_BATTING_HR, data = TEMP3, main = "Boxplot: Batting Home Runs",
        col = "gold")

hist(TEMP3$TEAM_BATTING_HR, main = "Histogram: Batting Home Runs", col = "gold",
     xlab = "TEAM_BATTING_HR")

TEMP3$L3_TEAM_BATTING_HR[TEMP3$TEAM_BATTING_HR != 0] <- log(TEMP3$TEAM_BATTING_HR[TEMP3$TEAM_BATTING_HR != 0]) ** 3
TEMP3$L3_TEAM_BATTING_HR[TEMP3$TEAM_BATTING_HR == 0] <- 0

boxplot(TEMP3$L3_TEAM_BATTING_HR, data = TEMP3, main = "Boxplot: Batting Home Runs (Transformed)",
        col = "gold")

hist(TEMP3$L3_TEAM_BATTING_HR, main = "Histogram: Batting Home Runs (Transformed)", col = "gold",
     xlab = "L3_TEAM_BATTING_HR")


# TEAM_FIELDING_E didn't look normal so try a transformation;
boxplot(TEMP3$TEAM_FIELDING_E, data = TEMP3, main = "Boxplot: Fielding Errors",
        col = "gold")

hist(TEMP3$TEAM_FIELDING_E, main = "Histogram:Fielding Errors", col = "gold",
     xlab = "TEAM_BATTING_HR")

TEMP3$REC_TEAM_FIELDING_E <- 1 / (TEMP3$TEAM_FIELDING_E)

boxplot(TEMP3$REC_TEAM_FIELDING_E, data = TEMP3, main = "Boxplot: Fielding Errors (Transformed)",
        col = "gold")

hist(TEMP3$REC_TEAM_FIELDING_E, main = "Histogram:Fielding Errors (Transformed)", col = "gold",
     xlab = "REC_TEAM_FIELDING_E")

# Lower end of the range okay for TEAM_FIELDING_E so leave that alone. Cap at upper extreme;
TEMP3$CAP_TEAM_FIELDING_E <- TEMP3$TEAM_FIELDING_E
TEMP3$CAP_TEAM_FIELDING_E[TEMP3$CAP_TEAM_FIELDING_E > 617] <- 617

boxplot(TEMP3$CAP_TEAM_FIELDING_E, data = TEMP3, main = "Boxplot: Fielding Errors (Capped)",
        col = "gold")

hist(TEMP3$CAP_TEAM_FIELDING_E, main = "Histogram:Fielding Errors (Capped)", col = "gold",
     xlab = "CAP_TEAM_FIELDING_E")

TEMP3$TEAM_FIELDING_E <- NULL

# TEAM_PITCHING_BB has extreme outliers. Try capping
boxplot(TEMP3$TEAM_PITCHING_BB, data = TEMP3, main = "Boxplot: Pitching - walks allowed",
        col = "gold")

hist(TEMP3$TEAM_PITCHING_BB, main = "Histogram: Pitching - walks allowed", col = "gold",
     xlab = "TEAM_PITCHING_BB")

TEMP3$CAP_TEAM_PITCHING_BB <- TEMP3$TEAM_PITCHING_BB
TEMP3$CAP_TEAM_PITCHING_BB[TEMP3$CAP_TEAM_PITCHING_BB > 1016] <- 1016
TEMP3$CAP_TEAM_PITCHING_BB[TEMP3$CAP_TEAM_PITCHING_BB < 71] <- 71

boxplot(TEMP3$CAP_TEAM_PITCHING_BB, data = TEMP3, main = "Boxplot: Pitching - walks allowed (Capped)",
        col = "gold")

hist(TEMP3$CAP_TEAM_PITCHING_BB, main = "Histogram: Pitching - walks allowed (Capped)", col = "gold",
     xlab = "CAP_TEAM_PITCHING_BB")

TEMP3$TEAM_PITCHING_BB <- NULL


# Outliers at the upper end of TEAM_PITCHING_HR  Try capping at upper;
boxplot(TEMP3$TEAM_PITCHING_HR, data = TEMP3, main = "Boxplot: Pitching - Homeruns allowed",
        col = "gold")

hist(TEMP3$TEAM_PITCHING_HR, main = "Histogram: Pitching - Homeruns allowed", col = "gold",
     xlab = "TEAM_PITCHING_HR")

TEMP3$CAP_TEAM_PITCHING_HR <- TEMP3$TEAM_PITCHING_HR
TEMP3$CAP_TEAM_PITCHING_HR[TEMP3$CAP_TEAM_PITCHING_HR > 300] <- 300

boxplot(TEMP3$CAP_TEAM_PITCHING_HR, data = TEMP3, main = "Boxplot: Pitching - Homeruns allowed",
        col = "gold")

hist(TEMP3$CAP_TEAM_PITCHING_HR, main = "Histogram: Pitching - Homeruns allowed", col = "gold",
     xlab = "TEAM_PITCHING_HR")

TEMP3$TEAM_PITCHING_HR <- NULL
TEMP3$INDEX <-NULL

#############################################################
# Create Model 3 using three different selection methods  	#
#############################################################;


# All variables
# Adjusted R-squared:  0.4177 (31 variables)
summary(Model3 <- lm(CAP_TARGET_WINS ~ ., data = TEMP3))

# Backward Selection
# Adjusted R-squared:  0.4127 (26 variables)
summary(Model3_back <- step(Model3, direction = "back"))

# Commenting this out as it produced the same model as backward selectoin
# Stepwise Selection
# Adjusted R-squared:   0.4157
#summary(Model3_step <- step(Model3, direction = "both"))

# Variable selection using Decision Trees
summary(Model3_dt <- lm(CAP_TARGET_WINS ~ CAP_IMP_TEAM_PITCHING_SO + CAP_TEAM_FIELDING_E + LOG_IMP_TEAM_BASERUN_SB + R3_TEAM_PITCHING_H +
                          SB_PCT +  TEAM_BATTING_BB + TEAM_BATTING_H, data = TEMP3))

# CP Mallow Selection
# Was getting a vague error that wasn't defined anywhere. When I removed all the log variables it ran fine
# Maybe this was a problem with perfect collinearity
TEMP4 <- TEMP3
TEMP4$L3_TEAM_BATTING_HR <- NULL
TEMP4$L2_IMP_TEAM_BASERUN_CS <- NULL
TEMP4$R3_TEAM_PITCHING_H <- NULL
TEMP4$REC_TEAM_FIELDING_E <- NULL
TEMP4$L2_TEAM_BATTING_3B <- NULL
TEMP4$LOG_IMP_TEAM_BASERUN_SB <- NULL
length(TEMP4)
cp_out <- leaps(x = TEMP4[, c(2:(which(colnames(TEMP4) == "CAP_TARGET_WINS") - 1), (which(colnames(TEMP4) == "CAP_TARGET_WINS") + 1):28)],
                y = TEMP[, which(colnames(TEMP4) == "CAP_TARGET_WINS")],
                names = names(TEMP4)[c(2:(which(colnames(TEMP4) == "CAP_TARGET_WINS") - 1), (which(colnames(TEMP4) == "CAP_TARGET_WINS") + 1):28)],
                method = "Cp", strictly.compatible = F, nbest = 1)


# This line produces a vector with the difference between the CP value and the # of regressors
cp_diff <- abs(cp_out$Cp - cp_out$size)

# This line prints out the variables selected by the CP Mallow which had the least difference between 
# the CP Mallow value and the # of regresssors
cp_out$which[which(cp_diff == min(cp_diff)),]

# create a data frame that removes the variables not used in the CP selected model
CP.TEMP <- TEMP4
CP.TEMP$M_TEAM_PITCHING_SO <- NULL
CP.TEMP$H_PCT <- NULL
CP.TEMP$CAP_IMP_TEAM_BASERUN_CS <- NULL
CP.TEMP$CAP_IMP_TEAM_BASERUN_SB <- NULL
CP.TEMP$CAP_IMP_TEAM_BATTING_SO <- NULL
CP.TEMP$CAP_IMP_TEAM_PITCHING_SO <- NULL
CP.TEMP$CAP_TEAM_BATTING_3B <- NULL
CP.TEMP$CAP_TEAM_FIELDING_E <- NULL
CP.TEMP$CAP_TEAM_PITCHING_BB <- NULL
CP.TEMP$CAP_TEAM_PITCHING_HR <- NULL

summary(CP.TEMP)

# Now rerun model using lm with the CP variables
# Adjusted R-squared:  0.3113
summary(Model3_CP <- lm(CAP_TARGET_WINS ~ ., data = CP.TEMP))


##################################################################################################
# Each of the models has variables that don't make sense - the signs are in the wrong direction  #
# Check for multicollinearity in the variables and rerun models                                  #
##################################################################################################

vif(Model3_back)

# Use call variable from model to get all variabls used to cut and paste into scatterplot matrix
Model3_back$call

pairs( ~ TEAM_BATTING_H + TEAM_BATTING_BB + 
         M_TEAM_BATTING_SO + M_TEAM_BASERUN_SB + M_TEAM_BATTING_HBP + 
         M_TEAM_FIELDING_DP + IMP_TEAM_FIELDING_DP + THREEB_PCT + 
         CAP_TEAM_PITCHING_H + CAP_IMP_TEAM_BATTING_SO + CAP_IMP_TEAM_BASERUN_SB + 
         CAP_TEAM_PITCHING_BB + CAP_TEAM_PITCHING_HR, data = TEMP3, main = "Scatterplot Matrix")

CORR.VAR <- subset(TEMP, select = c(TEAM_BATTING_H, TEAM_BATTING_2B,
                                    TEAM_BATTING_3B, TEAM_BATTING_HR, TEAM_BATTING_BB, TEAM_PITCHING_H,
                                    TEAM_FIELDING_E, M_TEAM_BATTING_SO, IMP_TEAM_BATTING_SO,
                                    M_TEAM_BASERUN_SB, IMP_TEAM_BASERUN_SB, IMP_TEAM_PITCHING_SO,
                                    M_TEAM_FIELDING_DP, IMP_TEAM_FIELDING_DP))

cor(CORR.VAR)

###########################
# Rerun Model 3 stepwise  #
###########################

# Remove PHR_PCT,TEAM_BATTING_HR,TWOB_PCT,SO_PCT, HR_PCT ,LOG_IMP_TEAM_BASERUN_SB,REC_TEAM_FIELDING_E,CAP_TEAM_FIELDING_E, 
TEMP3S <- subset(TEMP3, select = c(CAP_TARGET_WINS, TEAM_BATTING_H,  TEAM_BATTING_2B,
                                     TEAM_BATTING_BB, M_TEAM_BATTING_SO, M_TEAM_BASERUN_SB, 
                                    M_TEAM_BASERUN_CS, M_TEAM_BATTING_HBP, M_TEAM_FIELDING_DP, 
                                     SB_PCT,  THREEB_PCT,  L3_TEAM_BATTING_HR,IMP_TEAM_FIELDING_DP,
                                    CAP_TEAM_PITCHING_H, CAP_IMP_TEAM_BASERUN_CS, 
                                     CAP_IMP_TEAM_BATTING_SO, CAP_IMP_TEAM_BASERUN_SB,
                                    CAP_TEAM_PITCHING_BB, CAP_TEAM_PITCHING_HR))

# All variables
# Adjusted R-squared:  0.4119
summary(Model3 <- lm(CAP_TARGET_WINS ~ ., data = TEMP3S))


# Backward Selection
# Adjusted R-squared:  0.32
summary(Model3_back <- step(Model3, direction = "back"))


cor(TEMP2S)


pairs( ~ TEAM_BATTING_H + TEAM_BATTING_2B +
         TEAM_BATTING_3B + TEAM_BATTING_HR +
         TEAM_FIELDING_E + M_TEAM_BATTING_SO +
         IMP_TEAM_FIELDING_DP, data = TEMP2, main = "Scatterplot Matrix")


##############################################
# Evaluate and rerun Model 3 - CP selection  #
# Backward selection produced same model as  #
# stepwise, so will eliminate that           #  
##############################################

vif(Model3_CP)

# Use call variable from model to get all variabls used to cut and paste into scatterplot matrix
Model3_CP$call

pairs( ~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_BB + M_TEAM_BASERUN_SB + M_TEAM_PITCHING_SO +
         M_TEAM_FIELDING_DP + IMP_TEAM_FIELDING_DP + SB_PCT + TWOB_PCT + THREEB_PCT + HR_PCT + SO_PCT +
         PHR_PCT + CAP_IMP_TEAM_BASERUN_CS + CAP_IMP_TEAM_BASERUN_SB + CAP_IMP_TEAM_BATTING_SO +
         CAP_TEAM_BATTING_3B + CAP_TEAM_FIELDING_E, data = TEMP, main = "Scatterplot Matrix")

CORR.VAR <- subset(TEMP, select = c(CAP_TARGET_WINS, TEAM_BATTING_H, TEAM_BATTING_BB, M_TEAM_BASERUN_SB,
                                    M_TEAM_PITCHING_SO, M_TEAM_FIELDING_DP, IMP_TEAM_FIELDING_DP, SB_PCT,
                                    THREEB_PCT, CAP_IMP_TEAM_BASERUN_CS, PHR_PCT, TEAM_BATTING_2B,
                                    CAP_IMP_TEAM_BASERUN_SB, CAP_IMP_TEAM_BATTING_SO,
                                    CAP_TEAM_FIELDING_E))

cor(CORR.VAR)


# Remove PHR_PCT,TEAM_BATTING_HR,TWOB_PCT,TEAM_BATTING_2B,
TEMP3CP <- subset(CP.TEMP, select = c(CAP_TARGET_WINS, TEAM_BATTING_H,TEAM_BATTING_BB,M_TEAM_BATTING_SO, M_TEAM_BASERUN_SB,
                                     HR_PCT,IMP_TEAM_FIELDING_DP,
                                   M_TEAM_BASERUN_CS,M_TEAM_BATTING_HBP,M_TEAM_FIELDING_DP,SB_PCT,
                                   THREEB_PCT,SO_PCT,CAP_TEAM_PITCHING_H))

# Now rerun model using lm with the CP variables
# Adjusted R-squared:  0.3108
summary(Model3_CP <- lm(CAP_TARGET_WINS ~ ., data = TEMP3CP))

vif(Model3_CP)

#######################
# Evaluate the Models #
#######################


df.Model3 <- modelStats(Model3, "Model3")
df.Model3_back <- modelStats(Model3_back, "Model3_back")
df.Model3_cp <- modelStats(Model3_CP, "Model3_CP")
df.Model3_dt <- modelStats(Model3_dt, "Model3_dt")
all.model.stats <- rbind(all.model.stats, df.Model3, df.Model3_back, df.Model3_cp, df.Model3_dt)
all.model.stats <- all.model.stats[order(all.model.stats$AIC),]
all.model.stats

###############################################################################################
#  Output from all.model.stats  - slect Model2_back due to simplicity (numParams) & low RMSE
#
#          Model NumParameters      AIC AdjRSqaured      BIC    MSE  RMSE CP
# 7    Model2_CP            14 17961.28      0.3869 17867.24 143.69 11.99 14
# 6  Model2_back            15 17982.23      0.3891 17865.91 143.18 11.97 15
# 1       Model1            12 18013.30      0.3939 17957.79 150.40 12.26 12
# 2  Model1_Step            12 18013.30      0.3939 17957.79 150.40 12.26 12
# 3    Model1_CP            16 18094.44      0.4020 17953.85 148.38 12.18 16
# 12 Model3_back            14 18167.66      0.3288 18073.61 157.32 12.54 14
# 13   Model3_CP            14 18227.66      0.3108 18133.61 161.53 12.71 14
# 14   Model3_dt             8 18271.92      0.2554 18269.49 174.53 13.21  8
# 8    Model2_dt             7 18304.58      0.2396 18310.42 178.22 13.35  7
# 4    Model1_dt             7 18332.72      0.2728 18338.56 180.44 13.43  7
# 11      Model3            19 18335.34      0.3280 18109.94 157.51 12.55 19
# 5       Model2            33 18707.62      0.4088 17898.99 138.57 11.77 35
# 
###############################################################################################

summary(Model2_back)


#######################################
# Create Output CSV for holdout data  #
#######################################
Test <- read.csv("D:/Kim MSPA/Predict 411/Homework 1/moneyball_test.csv", sep = ",")

# Create TEST2 data file to store all changes made to the dataset
TEST2 <- Test

################################################################
# Replicate all the data manipulation done on the training set #
################################################################

# Drop variables with too many missing values
TEST2$TEAM_BATTING_HBP <- NULL

# Clean up missing values and created imputed variable flags

# TEAM_BATTING_SO
TEST2$M_TEAM_BATTING_SO <- 0
TEST2$M_TEAM_BATTING_SO[is.na(TEST2$TEAM_BATTING_SO)] <- 1

TEST2$IMP_TEAM_BATTING_SO <- TEST2$TEAM_BATTING_SO
median_tbso <- median(TEST2$TEAM_BATTING_SO[!is.na(TEST2$TEAM_BATTING_SO)])
TEST2$IMP_TEAM_BATTING_SO[is.na(TEST2$TEAM_BATTING_SO)] <- median_tbso
TEST2$TEAM_BATTING_SO <- NULL

# TEAM_BASERUN_SB
TEST2$M_TEAM_BASERUN_SB <- 0
TEST2$M_TEAM_BASERUN_SB[is.na(TEST2$TEAM_BASERUN_SB)] <- 1

TEST2$IMP_TEAM_BASERUN_SB <- TEST2$TEAM_BASERUN_SB
median_tbsb <- median(TEST2$TEAM_BASERUN_SB[!is.na(TEST2$TEAM_BASERUN_SB)])
TEST2$IMP_TEAM_BASERUN_SB[is.na(TEST2$TEAM_BASERUN_SB)] <- median_tbsb
TEST2$TEAM_BASERUN_SB <- NULL


# TEAM_BASERUN_CS
TEST2$M_TEAM_BASERUN_CS <- 0
TEST2$M_TEAM_BASERUN_CS[is.na(TEST2$TEAM_BASERUN_CS)] <- 1

TEST2$IMP_TEAM_BASERUN_CS <- TEST2$TEAM_BASERUN_CS
median_tbcs <- median(TEST2$TEAM_BASERUN_CS[!is.na(TEST2$TEAM_BASERUN_CS)])
TEST2$IMP_TEAM_BASERUN_CS[is.na(TEST2$TEAM_BASERUN_CS)] <- median_tbcs
TEST2$TEAM_BASERUN_CS <- NULL


# TEAM_PITCHING_SO
TEST2$M_TEAM_PITCHING_SO <- 0
TEST2$M_TEAM_PITCHING_SO[is.na(TEST2$TEAM_PITCHING_SO)] <- 1

TEST2$IMP_TEAM_PITCHING_SO <- TEST2$TEAM_PITCHING_SO
median_tpso <- median(TEST2$TEAM_PITCHING_SO[!is.na(TEST2$TEAM_PITCHING_SO)])
TEST2$IMP_TEAM_PITCHING_SO[is.na(TEST2$TEAM_PITCHING_SO)] <- median_tpso
TEST2$TEAM_PITCHING_SO <- NULL

# TEAM_FIELDING_DP
TEST2$M_TEAM_FIELDING_DP <- 0
TEST2$M_TEAM_FIELDING_DP[is.na(TEST2$TEAM_FIELDING_DP)] <- 1

TEST2$IMP_TEAM_FIELDING_DP <- TEST2$TEAM_FIELDING_DP
median_tfdp <- median(TEST2$TEAM_FIELDING_DP[!is.na(TEST2$TEAM_FIELDING_DP)])
TEST2$IMP_TEAM_FIELDING_DP[is.na(TEST2$TEAM_FIELDING_DP)] <- median_tfdp
TEST2$TEAM_FIELDING_DP <- NULL


# Check the data to make sure it looks as expected
head(TEST2)
describe(TEST2)


##############################
# Create derived variables  #
##############################
TEST2$SB_PCT[(TEST2$IMP_TEAM_BASERUN_SB + TEST2$IMP_TEAM_BASERUN_CS) == 0] <- 0
TEST2$SB_PCT[(TEST2$IMP_TEAM_BASERUN_SB + TEST2$IMP_TEAM_BASERUN_CS) != 0] <-
  TEST2$IMP_TEAM_BASERUN_SB[(TEST2$IMP_TEAM_BASERUN_SB + TEST2$IMP_TEAM_BASERUN_CS) != 0] /
  (TEST2$IMP_TEAM_BASERUN_SB[(TEST2$IMP_TEAM_BASERUN_SB + TEST2$IMP_TEAM_BASERUN_CS) != 0] +
     TEST2$IMP_TEAM_BASERUN_CS[(TEST2$IMP_TEAM_BASERUN_SB + TEST2$IMP_TEAM_BASERUN_CS) != 0])
TEST2$TWOB_PCT <- TEST2$TEAM_BATTING_2B / TEST2$TEAM_BATTING_H
TEST2$THREEB_PCT <- TEST2$TEAM_BATTING_3B / TEST2$TEAM_BATTING_H
TEST2$HR_PCT <- TEST2$TEAM_BATTING_HR / TEST2$TEAM_BATTING_H
TEST2$SO_PCT <- TEST2$IMP_TEAM_BATTING_SO / (TEST2$IMP_TEAM_BATTING_SO + TEST2$TEAM_BATTING_H)
TEST2$H_PCT <- TEST2$TEAM_BATTING_H / (TEST2$IMP_TEAM_BATTING_SO + TEST2$TEAM_BATTING_H)
TEST2$PHR_PCT <- TEST2$TEAM_PITCHING_HR / TEST2$TEAM_PITCHING_H


#######################
# Deal with outliers  #
#######################

# TEAM_PITCHING_H
TEST2$CAP_TEAM_PITCHING_H <- TEST2$TEAM_PITCHING_H
TEST2$CAP_TEAM_PITCHING_H[TEST2$TEAM_PITCHING_H > 2475] <- 2475


# Doesn't look normal - try a tranformation 
TEST2$R3_TEAM_PITCHING_H[TEST2$TEAM_PITCHING_H != 0] <- (1 / TEST2$TEAM_PITCHING_H[TEST2$TEAM_PITCHING_H != 0]) ** 3
TEST2$R3_TEAM_PITCHING_H[TEST2$TEAM_PITCHING_H == 0] <- 0

TEST2$TEAM_PITCHING_H <- NULL

# IMP_TEAM_BASERUN_CS
TEST2$L2_IMP_TEAM_BASERUN_CS[TEST2$IMP_TEAM_BASERUN_CS != 0] <-
  log(TEST2$IMP_TEAM_BASERUN_CS[TEST2$IMP_TEAM_BASERUN_CS != 0]) ** 2
TEST2$L2_IMP_TEAM_BASERUN_CS[TEST2$IMP_TEAM_BASERUN_CS == 0] <- 0


# Cap IMP_TEAM_BASERUN_CS at both ends of extreme;
TEST2$CAP_IMP_TEAM_BASERUN_CS <- TEST2$IMP_TEAM_BASERUN_CS
TEST2$CAP_IMP_TEAM_BASERUN_CS[TEST2$CAP_IMP_TEAM_BASERUN_CS > 86] <- 86
TEST2$CAP_IMP_TEAM_BASERUN_CS[TEST2$CAP_IMP_TEAM_BASERUN_CS < 12] <- 12

TEST2$IMP_TEAM_BASERUN_CS <- NULL


# IMP_TEAM_BASERUN_SB didn't look normal so try a transformation
TEST2$LOG_IMP_TEAM_BASERUN_SB[TEST2$IMP_TEAM_BASERUN_SB != 0] <-
  log(TEST2$IMP_TEAM_BASERUN_SB[TEST2$IMP_TEAM_BASERUN_SB != 0])
TEST2$LOG_IMP_TEAM_BASERUN_SB[TEST2$IMP_TEAM_BASERUN_SB == 0] <- 0


# Lower end of the range okay for TEAM_PITCHING_H so leave that alone. Cap at upper extreme;
TEST2$CAP_IMP_TEAM_BASERUN_SB <- TEST2$IMP_TEAM_BASERUN_SB;
TEST2$CAP_IMP_TEAM_BASERUN_SB[TEST2$CAP_IMP_TEAM_BASERUN_SB > 403] <- 403

TEST2$IMP_TEAM_BASERUN_SB <- NULL

# IMP_TEAM_BATTING_SO looks fairly normal so don't mess with this too much. Cap at the lower extreme
TEST2$CAP_IMP_TEAM_BATTING_SO <- TEST2$IMP_TEAM_BATTING_SO
TEST2$CAP_IMP_TEAM_BATTING_SO[TEST2$CAP_IMP_TEAM_BATTING_SO < 4] <- 4

TEST2$IMP_TEAM_BATTING_SO <- NULL

# IMP_TEAM_PITCHING_SO has extreme outliers - cap at extreme value
TEST2$CAP_IMP_TEAM_PITCHING_SO <- TEST2$IMP_TEAM_PITCHING_SO
TEST2$CAP_IMP_TEAM_PITCHING_SO[TEST2$CAP_IMP_TEAM_PITCHING_SO > 1950] <- 1950
TEST2$IMP_TEAM_PITCHING_SO <- NULL

# TEAM_BATTING_3B didn't look normal so try a transformation
TEST2$L2_TEAM_BATTING_3B[TEST2$TEAM_BATTING_3B != 0] <-
  log(TEST2$TEAM_BATTING_3B[TEST2$TEAM_BATTING_3B != 0]) ** 2
TEST2$L2_TEAM_BATTING_3B[TEST2$TEAM_BATTING_3B == 0] <- 0

# Lower end of the range okay for TEAM_BATTING_3B so leave that alone. Cap at upper extreme
TEST2$CAP_TEAM_BATTING_3B <- TEST2$TEAM_BATTING_3B
TEST2$CAP_TEAM_BATTING_3B[TEST2$CAP_TEAM_BATTING_3B > 186] <- 186
TEST2$TEAM_BATTING_3B <- NULL

# TEAM_BATTING_HR - no outliers but right skewed. Try transformation
# Not sure this is an improvement so keep original variable

TEST2$L3_TEAM_BATTING_HR[TEST2$TEAM_BATTING_HR != 0] <- log(TEST2$TEAM_BATTING_HR[TEST2$TEAM_BATTING_HR != 0]) ** 3
TEST2$L3_TEAM_BATTING_HR[TEST2$TEAM_BATTING_HR == 0] <- 0


# TEAM_FIELDING_E didn't look normal so try a transformation;
TEST2$REC_TEAM_FIELDING_E <- 1 / (TEST2$TEAM_FIELDING_E)

# Lower end of the range okay for TEAM_FIELDING_E so leave that alone. Cap at upper extreme;
TEST2$CAP_TEAM_FIELDING_E <- TEST2$TEAM_FIELDING_E
TEST2$CAP_TEAM_FIELDING_E[TEST2$CAP_TEAM_FIELDING_E > 617] <- 617

TEST2$TEAM_FIELDING_E <- NULL

# TEAM_PITCHING_BB has extreme outliers. Try capping
TEST2$CAP_TEAM_PITCHING_BB <- TEST2$TEAM_PITCHING_BB
TEST2$CAP_TEAM_PITCHING_BB[TEST2$CAP_TEAM_PITCHING_BB > 1016] <- 1016
TEST2$CAP_TEAM_PITCHING_BB[TEST2$CAP_TEAM_PITCHING_BB < 71] <- 71

TEST2$TEAM_PITCHING_BB <- NULL


# Outliers at the upper end of TEAM_PITCHING_HR  Try capping at upper;
TEST2$CAP_TEAM_PITCHING_HR <- TEST2$TEAM_PITCHING_HR
TEST2$CAP_TEAM_PITCHING_HR[TEST2$CAP_TEAM_PITCHING_HR > 300] <- 300

TEST2$TEAM_PITCHING_HR <- NULL


######################################
# Changes finished - check the data  #
######################################
head(TEST2)


##############################
# Build Production output    #
##############################


TEST2$TARGET_WINS <- (	28.249844
                + TEST2$TEAM_BATTING_H *	0.042526
                + TEST2$TEAM_BATTING_BB *	0.036166
                + TEST2$M_TEAM_BATTING_SO *	8.552401
                + TEST2$M_TEAM_BASERUN_SB *	36.542666
                + TEST2$M_TEAM_BASERUN_CS *	1.214194
                + TEST2$M_TEAM_FIELDING_DP *	6.935513
                + TEST2$IMP_TEAM_FIELDING_DP *	-0.101118
                + TEST2$THREEB_PCT *	158.840243
                + TEST2$HR_PCT *	101.165263
                + TEST2$CAP_IMP_TEAM_BASERUN_CS *	-0.077129
                + TEST2$CAP_IMP_TEAM_BASERUN_SB *	0.073818
                + TEST2$CAP_IMP_TEAM_BATTING_SO *	-0.015541
                + TEST2$CAP_TEAM_FIELDING_E *	-0.084205
                + TEST2$CAP_TEAM_PITCHING_BB *	-0.008148)


# Creating the Output file with the id and price_doc
Output <- data.frame(TEST2$INDEX, TEST2$TARGET_WINS)
names(Output) <- c("INDEX", "TARGET_WINS")

# Checking the Output Dataset
str(Output) # 'data.frame': 259 obs. of 2 variables
head(Output)

# Check distribution
describe(Output)

### Write TESTdata to CSV ###
write.csv(Output,
          file = "D:/Kim MSPA/Predict 411/Homework 1/Deliverables/Kim_Kaminsky_HW1_R_Output.csv",
          row.names = FALSE)



