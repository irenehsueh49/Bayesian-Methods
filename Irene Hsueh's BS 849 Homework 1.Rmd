---
title: "Irene Hsueh's BS 849 Homework 1"
author: "Irene Hsueh"
date: "1/31/2022"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(rjags)
library(coda)
library(formatR)
```

### Death Penalty Support in Texas
prior density: P(θ) = Y ~ Beta(a, b), a=3, b=2
```{r}
#step function is 1 if quantity ≥ 0, 0 if quantity < 0
#indicator1 is 1 if θ>0.8, 0 if θ≤0.8 
#indicator2 is 1 if θ>0.66, 0 if θ≤0.66 
prior_model_bugs <- 
"model {theta ~ dbeta(3, 2)
indicator1 <- step(theta - 0.8)
indicator2 <- step(theta - 0.66)
}"

prior_model <- jags.model(textConnection(prior_model_bugs))
prior_model_gibbs <- update(prior_model, n.iter = 1000)

prior_model_test <- coda.samples(prior_model, c("theta", "indicator1", "indicator2"), n.iter= 10000)
head(prior_model_test)
summary(prior_model_test)
plot(prior_model_test)
```


### Sickle Cell Anemia of African American Ancestry
prior density: P(θ) = Y ~ Beta(a, b), a=2, b=8
data: n=20, Y=7 affected
posterior density: P(θ|Y) = Y ~ Beta(2+7, 8+13) = Y ~ Beta(9, 21)
```{r}
#step function is 1 if quantity ≥ 0, 0 if quantity < 0
#indicator is 1 if Y>50, 0 if Y≤50 
#integral equation1 is 1 if Y<70, 0 if Y≥70  
#integral equation2 is 1 if Y≤30, 0 if Y>30

sickle_cell_model_bugs <- 
"model {
theta ~ dbeta(9, 21)
Y ~ dbin(theta, 100)
indicator <- step(Y - 50)
integral <- step(70 - Y) - step(30 - Y)
}"

sickle_cell_model <- jags.model(textConnection(sickle_cell_model_bugs))
sickle_cell_model_gibbs <- update(sickle_cell_model, n.iter = 1000)

sickle_cell_model_test <- coda.samples(sickle_cell_model, c("theta", "indicator", "integral"), n.iter= 10000)
head(sickle_cell_model_test)
summary(sickle_cell_model_test)
plot(sickle_cell_model_test)
```



