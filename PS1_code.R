library(tidyverse)
library(stargazer)
tb <- tibble(
  female = ifelse(runif(10000)>=0.50,1,0),
  ability = rnorm(10000),
  discrimination = female,
  occupation = 9 + round(1 + 2*ability + 0*female - 2*discrimination + rnorm(10000)),
  wage = 1 - 1*discrimination + 1*occupation + 2*ability + rnorm(10000) 
)

lm_1 <- lm(wage ~ discrimination, tb)
lm_2 <- lm(wage ~ discrimination + occupation, tb)
lm_3 <- lm(wage ~ female + occupation + ability, tb)

stargazer(lm_1,lm_2,lm_3, title="Regression Results",
          align=TRUE, dep.var.labels=c("Earnings"), 
          covariate.labels= c("Discrimination", "Female", "Occupation", "Abillity"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)
