---
title: "Podsie Analysis"
author: "Yu-An Chen"
date: "8/13/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, include = FALSE}
rm(list = ls())
library(devtools)
library(MASS)
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(tidyr)
library(stringr)
library(car)
```

# Loading & Basic Cleaning Omitted
```{r, include = FALSE}
test <-  read.csv("Test.csv")
control <- read.csv("Control.csv")

names(test)[1] <- "Student"
names(control)[1] <- "Student"

test <- test[-c(28,29,30),]
control <- control[-c(19,20,21),]
```

(Cleaning & converting Data to Long format)
```{r, include = FALSE}
for(i in 1:nrow(control)){
  control$Student[i] = i + 27
}

test$Group <- "Experimental"
control$Group <- "Control"

names(test)[13] <- "Time_spent_sec"

testCleaned<- test[-c(10,11,12,13,14,15,16)]
overallCleaned <- rbind(testCleaned, control)

gather_to_long <- function(df, cond1, cond2, cond3){
  keycol <- "condition"
  valuecol <- "Score"
  gathercols <- c(cond1, cond2, cond3)
  return (gather_(df, keycol, valuecol, gathercols, factor_key = TRUE))
}

testLong <- gather_to_long(test, "Pre.Test", "Test", "Post")
names(testLong)[13] <- "Avg_Spacing_Days"


controlLong <- gather_to_long(control, "Pre.Test", "Test", "Post")
overallLong <- rbind(gather_to_long(testCleaned, "Pre.Test", "Test", "Post"), controlLong)
```

(Converting conditions to numeric for LMER) 
```{r, include = FALSE}
overallLong$testNum <- 0
testLong$testNum <- 0

for (i in 1:nrow(overallLong)){
  if (overallLong$condition[i] == "Test"){
    overallLong$testNum[i] = 1
  }
  else if(overallLong$condition[i] == "Post"){
    overallLong$testNum[i] = 2
  }
}

for (i in 1:nrow(testLong)){
  if (testLong$condition[i] == "Test"){
    testLong$testNum[i] = 1
  }
  else if(testLong$condition[i] == "Post"){
    testLong$testNum[i] = 2
  }
}


names(testLong)[8] <- "total_responses"
names(testLong)[9] <- "filtered_responses"
```

# Plots
```{r}

preScore <- data.frame(Type = c("Control", "Experimental"), Avg = c(mean(test$Pre.Test), mean(control$Pre.Test)), 
                       sd = c(sd(test$Pre.Test), sd(control$Pre.Test)))

testScore <- data.frame(Type = c("Control", "Experimental"), Avg = c(mean(test$Test), mean(control$Test)), 
                        sd = c(sd(test$Test), sd(control$Test)))

postScore <- data.frame(Type = c("Control", "Experimental"), Avg = c(mean(test$Post), mean(control$Post)), 
                        sd = c(sd(test$Post), sd(control$Post)))

diffScore <- data.frame(Type = c("Control", "Experimental"), Avg = c(mean(test$Test) - mean(test$Post), 
                                                                     mean(control$Test) -mean(control$Post)),
                        sd = c(sqrt(sd(test$Test)^2 + sd(test$Post)^2), sqrt(sd(control$Test)^2 + sd(control$Post)^2)))

prepostScore <- data.frame(Type = c("Control", "Experimental"), Avg = c(mean(test$Pre.Test) - mean(test$Post), 
                                                                     mean(control$Pre.Test) -mean(control$Post)),
                        sd = c(sqrt(sd(test$Pre.Test)^2 + sd(test$Post)^2), sqrt(sd(control$Pre.Test)^2 + sd(control$Post)^2)))


prePlot <- ggplot(data=preScore, aes(x=Type, y=Avg)) +
  geom_col(width = 0.55)+ labs(y="Score", x = "Group",
              title = "Pre-Test Scores") + geom_errorbar( aes(x=Type, ymin=Avg-sd, ymax=Avg+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)

testPlot <- ggplot(data=testScore, aes(x=Type, y=Avg)) +
  geom_col(width = 0.55)+ labs(y="Score", x = "Group",
              title = "Unit Test Scores") + geom_errorbar( aes(x=Type, ymin=Avg-sd, ymax=Avg+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)

postPlot <- ggplot(data=postScore, aes(x=Type, y=Avg)) +
  geom_col(width = 0.55)+ labs(y="Score", x = "Group",
              title = "Post-Test Scores") + geom_errorbar( aes(x=Type, ymin=Avg-sd, ymax=Avg+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)

diffPlot <- ggplot(data=diffScore, aes(x=Type, y=Avg)) +
  geom_col(width = 0.55)+ labs(y="Score", x = "Group",
              title = "Score differences (Test - Post)") + geom_errorbar( aes(x=Type, ymin=Avg-sd, ymax=Avg+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)

prepostPlot <- ggplot(data=prepostScore, aes(x=Type, y=Avg)) +
  geom_col(width = 0.55)+ labs(y="Score", x = "Group",
              title = "Score differences (Pre - Post)") + geom_errorbar( aes(x=Type, ymin=Avg-sd, ymax=Avg+sd), width=0.4, colour="orange", alpha=0.9, size=1.3)



prePlot
testPlot
postPlot
diffPlot
prepostPlot
```

# LMER Models
# change condition to test 
```{r}
test_All <- lmer(Score ~ testNum + Group + (testNum|Student), data = overallLong)
coef(test_All)
coef(summary(test_All))

test_experimental <- lmer(Score ~ (testNum|Student)+ testNum + days_studying + Avg_Spacing_Days + total_responses + filtered_responses, data = testLong)
coef(test_experimental)
coef(summary(test_experimental))
```


# Regression on Pre to Post
We will now determine if groups (control vs. experimental) have an effect on the improvement from pre-test to post-test
```{r}
test_all_out <- do.call(rbind.data.frame, coef(test_All))

test_all_out$Group <- "Experimental"

for (i in 1: nrow(test_all_out)){
  row_num <- substr(rownames(test_all_out)[i], 9, nchar(rownames(test_all_out)[i]))
  if(strtoi(row_num, base = 0L) >= 28){
    test_all_out$Group[i] <- "Control"
  }
}

final <- lm(testNum ~ Group + `(Intercept)`, data = test_all_out)
summary(final)

```


# Multiple Linear Regression: All Data

First, we will run a multiple linear regression on all the common available predictors. Since the "Group" predictor wasn't statistically significant, we can conclude that it's not an appropriate predictor in our model.
```{r}
lm_all <- lmer(Score ~ condition + Group + (1|Classroom) + (1|Student), data = overallLong)

summary(lm_all)
```
# Multiple Linear Regression: Experimental Group

Next we will look at the data within our experimental condition with all the available predictors
```{r}
lm_exp <- lmer(Score ~ condition + Avg_Spacing_Days + total_responses + filtered_responses + days_studying + log(Time_spent_sec) + (1|Student), data = testLong)
summary(lm_exp)

hist(log(testLong$Time_spent_sec))
```

Since Avg_Spacing_Days and days_studying were not significant predictors, we will drop them from the model. 
```{r}
lm_expCleaned <- lmer(Score ~ condition + Time_spent_sec + (1|Student), data = testLong)
summary(lm_expCleaned)

```

Just for a clearer visualization, here we run a linear regression with only the Time_spent_sec predictor: 
```{r}
timeLm <- lm(Score ~Time_spent_sec, data = subset(testLong, condition!= "Pre.Test"))
summary(timeLm)

plot(testLong$Time_spent_sec, testLong$Score)
abline(lm(Score ~ Time_spent_sec, subset(testLong, condition!= "Pre.Test")))
```

It seems that a quadratic regression may be more fitting, here is the model:
```{r}
testLong$Time_spent_min <- testLong$Time_spent_sec/60
testLong$time2 <- testLong$Time_spent_min^2

timeQm <- lm(Score ~ Time_spent_min + time2, data = testLong)
summary(timeQm)
```

# Logistic Regression: Podsie question success rates
```{r}
podsie_scores <- read.csv("podsie_scores.csv")

add_opportunity <- function(df){
  df$opportunity <- 0
  for(i in 1:max(df$student_id)){
    if(i == 8){
      next
    }
    rows = which(df$student_id == i)
    checkList <- list(14)
    for(j in 1:length(rows)){
      if(list(df$question_id[rows[j]]) %in% checkList){
        df$opportunity[rows[j]] = df$opportunity[rows[j]-1] + 1
      }
      else{
        checkList <- append(checkList, df$question_id[rows[j]])
      }
    }
  }
  ##adding a sub-function that also codes success/failure
  df$correct <- 0
  
  for (i in 1:nrow(df)){
    if(df$success[i] == "TRUE"){
      df$correct[i] = 1
    }
  }
  return(df) 
}

podsie_scores <-  add_opportunity(podsie_scores)
table(subset(podsie_scores, opportunity == 1)$success)
table(subset(podsie_scores, opportunity == 2)$success)
table(subset(podsie_scores, opportunity == 3)$success)
```

# LMER Prediction model

```{r}
names(podsie_scores)[1] <- "Student"

podsie_logit <- glmer(correct ~ opportunity + (opportunity|question_id) + (opportunity|Student), family = binomial, data = podsie_scores)

summary(podsie_logit)
coef(podsie_logit)

podsie_logit_out <- do.call(rbind.data.frame, coef(podsie_logit)[2])

odds <- function(df){
  df$base_odds <- 0
  df$odds_ratio <- 0
  for (i in 1:nrow(df)){
    df$base_odds[i] = exp(1)^df$`(Intercept)`[i]
    df$odds_ratio[i] <- exp(1)^df$opportunity[i]
  }
  return(df)
}

print(odds(podsie_logit_out))
```

# Correlation test between opportunity and pre-post delta
```{r}
podsie_logit_out <- do.call(rbind.data.frame, coef(podsie_logit)[2])

student_num <- function(df){
  df$Student <- 0 
  for (i in 1:nrow(df)){
    row_num <- substr(rownames(df)[i], 9, nchar(rownames(df)[i]))
    df$Student[i] <- row_num 
  }
  return(df)
  }

podsie_logit_out <- student_num(podsie_logit_out)

podsie_overall <- left_join(testCleaned, podsie_logit_out)

summary(lm(pre...post.delta ~ `(Intercept)` + opportunity, data = podsie_overall))
```
