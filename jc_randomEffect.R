library(tidyverse)
library(lme4)
library(lmerTest)
library(car)

subjects <- paste0("s", 1:10)
conditions <- 0:1
df <- expand.grid(subjects, conditions, stringsAsFactors = FALSE)
colnames(df) <- c("subject", "condition")
print(df)


intercept_bysubject_mean <- 800
intercept_bysubject_sd <- 50

set.seed(84)
ranefs_bysubject <- data.frame(subject = subjects,
                               intercept_bysubject = rnorm(length(subjects),
                                                           intercept_bysubject_mean,
                                                           intercept_bysubject_sd),
                               stringsAsFactors = FALSE)
print(ranefs_bysubject)

df <- df %>% left_join(ranefs_bysubject, by = "subject")
print(df)


beta_1 <- 100
sigma <- 50

df$y <- df$intercept_bysubject +
        beta_1 * df$condition +
        rnorm(nrow(df), 0, sigma)
print(df)


lm.fit <- lm(y ~ 1 + condition, data = df)
lmer.fit <- lmer(y ~ 1 + condition + (1|subject), data = df)

summary(lm.fit)
summary(lmer.fit, corr = FALSE)


#### Practice 1 #####

#1 Function for simulating random-intercept data

MEM_fun1 <- function(beta_1, 
                     rand_intercept_mean, 
                     rand_intercept_sd, 
                     error_var, 
                     n_subject)  {
  subjects <- paste0("s", 1:n_subject)
  conditions <- 0:1
  df <- expand.grid(subjects, conditions, stringsAsFactors = FALSE)
  colnames(df) <- c("subject", "condition")
  
  ranefs_bysubject <- data.frame(subject = subjects,
                                 intercept_bysubject = rnorm(length(subjects),
                                                             rand_intercept_mean,
                                                             rand_intercept_sd),
                                 stringsAsFactors = FALSE)
  df <- df %>% left_join(ranefs_bysubject, by = "subject")
  df$y <- df$intercept_bysubject + beta_1 * df$condition + rnorm(nrow(df), 0, error_var)
  
  return(df)
  
}

#2 Test Function

sim1 <- MEM_fun1(500, 625, 50, 25, 10)

print(sim1)

sim1000 <- MEM_fun1(500, 625, 10, 10, 1000)

summary(lmer(y ~ 1 + condition + (1|subject), data = sim1000))

#3 Modify
MEM_fun2 <- function(beta_0, 
                     beta_1, 
                     rand_intercept_sd, 
                     error_var, 
                     n_subject)  {
  subjects <- paste0("s", 1:n_subject)
  conditions <- 0:1
  df <- expand.grid(subjects, conditions, stringsAsFactors = FALSE)
  colnames(df) <- c("subject", "condition")
  
  ranefs_bysubject <- data.frame(subject = subjects,
                                 intercept_bysubject = rnorm(length(subjects),
                                                             0,
                                                             rand_intercept_sd),
                                 stringsAsFactors = FALSE)
  df <- df %>% left_join(ranefs_bysubject, by = "subject")
  df$y <- beta_0 + df$intercept_bysubject + beta_1 * df$condition + rnorm(nrow(df), 0, error_var)
  
  return(df)
  
}

sim1000.v2 <- MEM_fun2(625, 500, 10, 10, 1000)
sim10.v2 <- MEM_fun2(625, 500, 10, 10, 10)
summary(lmer(y ~ 1 + condition + (1|subject), data = sim1000.v2))
summary(lmer(y ~ 1 + condition + (1|subject), data = sim10.v2))

#5
sleep1 <- sleep

sleep.ttest <- t.test(extra ~ group, data = sleep1)

sleep.lm <- lm(extra ~ group, data = sleep1)
summary(sleep.lm)

anova1 <- aov(extra ~ 1 + group, data = sleep1)
summary(anova1)
anova2 <- aov(sleep.lm)
summary(anova2)
?t.test
pair.sleeptest <- t.test(extra ~ group, paired = TRUE, data = sleep1)
pair.sleeptest

aov(extra ~ 1 + group + Error(ID), data = sleep1)

lmer.sleep1 <- summary(lmer(extra ~ 1 + group + (1|ID), data = sleep1))
lmer.sleep1


#Crossed vs Nested Effects
#Practice 2

MEM_fun3 <- function(beta_0, 
                     beta_1, 
                     rand_subject_sd,
                     rand_item_sd,
                     error_var, 
                     n_subject,
                     n_item)  {
  subjects <- paste0("s", 1:n_subject)
  conditions <- 0:1
  items <- paste0("ID", 1:n_item)
  df <- expand.grid(subjects, items, conditions, stringsAsFactors = FALSE)
  colnames(df) <- c("subject", "items", "condition")
  
  ranefs_bysubject <- data.frame(subject = subjects,
                                 intercept_bysubject = rnorm(length(subjects),
                                                             0,
                                                             rand_subject_sd),
                                 stringsAsFactors = FALSE)
  ranefs_byitem <- data.frame(item = items,
                              intercept_byitem = rnorm(length(items),
                                                             0,
                                                          rand_item_sd),
                                 stringsAsFactors = FALSE)
  
  df <- df %>% left_join(ranefs_bysubject, by = "subject") %>%
    left_join(ranefs_byitem, by = "item")
  df$y <- beta_0 + df$intercept_bysubject + df$intercept_byitem + beta_1 * df$condition + rnorm(nrow(df), 0, error_var)
  
  return(df)
  
}

sim.small.rand <- MEM_fun3(beta_0 = 200, 
                     beta_1 = 400, 
                     rand_subject_sd = 14,
                     rand_item_sd = 25,
                     error_var = 10, 
                     n_subject = 3,
                     n_item = 3)
