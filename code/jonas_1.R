setwd("C:/Users/q436611/WM_Prediction/fifa-worldcup-prediction/code")

library(data.table)

data = data.table(read.csv("../data/merged.csv"))



# Model for mA


data$FifaA = data$A_ANG + data$A_MIT + data$A_DEF
data$FifaB = data$B_ANG + data$B_MIT + data$B_DEF

mA_glm =  glm(data$GoalA ~ data$EloBBefore + data$FifaA + data$FifaB, family = poisson(link = "log"))
mA_glm

mA =  exp(predict(mA_glm, data))

 
# Model for mB
mB_glm =  glm(data$GoalA ~ data$EloABefore + data$FifaA + data$FifaB, family = poisson(link = "log"))
mB_glm

mB =  exp(predict(mB_glm, data))


# lambda = mean(expected goals A , expected goals B)

lambdaAB = 0.5*(unname(mA) + unname(mB))


# lambda B

lambdaBA_glm =  glm(data$GoalB ~ data$EloABefore + data$GoalA + data$FifaA + data$FifaB, family = poisson(link = "log"))

lambdaBA_glm

lambdaBA =  exp(predict(lambdaBA_glm, data))

mean(lambdaAB)-mean(unname(lambdaBA))





