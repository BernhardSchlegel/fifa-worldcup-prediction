library(data.table)
library(caret)

setwd("C:/Users/q436611/WM_Prediction/fifa-worldcup-prediction/code")

data = data.table(read.csv("../data/merged.csv"))


# ==== Preprocess ====

data$Date = as.Date(data$Date, format = "%Y-%m-%d")

# rearrange date so that A has higher Elo


for (i in 1:nrow(data)){
  
  if(data[i]$EloABefore < data[i]$EloBBefore){
    
    TeamA = data[i, c("TeamA", "GoalA", "EloABefore", "EloAAfter", "A_ANG", "A_MIT", "A_DEF")]
    data[i, c("TeamA", "GoalA", "EloABefore", "EloAAfter", "A_ANG", "A_MIT", "A_DEF")] = 
          data[i, c("TeamB", "GoalB", "EloBBefore", "EloBAfter", "B_ANG", "B_MIT", "B_DEF")]
    data[i, c("TeamB", "GoalB", "EloBBefore", "EloBAfter", "B_ANG", "B_MIT", "B_DEF")] = TeamA
    
  }
  
}

# Features
# Caution: sum of scores is not mean anymore for imputed countries!
# Brazil not correct
data$FifaA = data$A_ANG + data$A_MIT + data$A_DEF
data$FifaB = data$B_ANG + data$B_MIT + data$B_DEF


ggplot(data = data) +
  geom_density(aes(FifaA), alpha = 0.5, fill = "Blue")+
  geom_density(aes(FifaB), alpha = 0.5, fill = "red" )

ggplot(data = data) +
  geom_density(aes(EloABefore), alpha = 0.5, fill = "Blue")+
  geom_density(aes(EloBBefore), alpha = 0.5, fill = "red" )



# Scale to gaussian

features = c("EloABefore", "EloBBefore", "FifaA", "FifaB")

data_backup =data
pp_settings = preProcess(data[, features, with = F], method = c("center", "scale"))
data = predict(pp_settings, data)

# Check
apply(data[,features, with =F ], 2, mean )
apply(data[,features, with =F ], 2, sd )





# ==== Model ====
# Model for mA

GoalA = data$GoalA
EloBBefore = data$EloBBefore
FifaB=data$FifaB

mA_glm =  glm(GoalA ~ EloBBefore + FifaB, family = poisson(link = "log"))
mA_glm


 
# Model for mB

EloABefore = data$EloABefore
FifaA=data$FifaA

mB_glm =  glm(GoalA ~ EloABefore + FifaA, family = poisson(link = "log"))
mB_glm



# lambda B

GoalB = data$GoalB

lambdaBA_glm =  glm(GoalB ~ EloABefore + GoalA + FifaA + FifaB, family = poisson(link = "log"))

lambdaBA_glm



# ==== Predict ====


# predict same data with fitted models

mA =  exp(predict(mA_glm, data))
mB =  exp(predict(mB_glm, data))

# lambda = mean(expected goals A , expected goals B)
lambdaAB = 0.5*(unname(mA) + unname(mB))

lambdaBA =  exp(predict(lambdaBA_glm, data))

# Check the fit in general 
mean(mB)-mean(unname(lambdaBA))
mean(data$GoalA - data$GoalB)



# get the latest Elo score for each country

Teams = as.data.table(unique(c(as.vector(unique(data$TeamA)),as.vector(unique(data$TeamB)))))
colnames(Teams) = "Team"
Teams$latestElo = 0
Teams$latestFifa = 0

# use backup data as this is not scaled
data_backup = data_backup[order(Date, decreasing = T)]

for (i in Teams$Team){
  
  LatestA = head(data_backup[TeamA == i], 1)
  LatestB = head(data_backup[TeamB == i], 1)
  
  if(nrow(head(data_backup[TeamA == i], 1))==0){
    Teams[Team == i]$latestElo = LatestB$EloBAfter
    Teams[Team == i]$latestFifa = LatestB$FifaB    
  }else if(nrow(head(data_backup[TeamB == i], 1))==0){
    Teams[Team == i]$latestElo = LatestA$EloAAfter
    Teams[Team == i]$latestFifa = LatestA$FifaA
  }else if(LatestA$Date > LatestB$Date){
    Teams[Team == i]$latestElo = LatestA$EloAAfter
    Teams[Team == i]$latestFifa = LatestA$FifaA
  }else{
    Teams[Team == i]$latestElo = LatestB$EloBAfter
    Teams[Team == i]$latestFifa = LatestB$FifaB
  }
    
}


# Predict a match

Team1 = "Germany"
Team2 = "Brazil"

if(Teams[Team==Team1]$latestElo > Teams[Team==Team2]$latestElo){
  predict_data = data.frame("TeamA" = Team1, "TeamB" = Team2,
                            "EloABefore" = Teams[Team==Team1]$latestElo,
                            "EloBBefore" = Teams[Team==Team2]$latestElo,
                            "FifaA" = Teams[Team==Team1]$latestFifa,
                            "FifaB" = Teams[Team==Team2]$latestFifa)
}else{
  predict_data = data.frame("TeamA" = Team2, "TeamB" = Team1,
                            "EloABefore" = Teams[Team==Team2]$latestElo,
                            "EloBBefore" = Teams[Team==Team1]$latestElo,
                            "FifaA" = Teams[Team==Team2]$latestFifa,
                            "FifaB" = Teams[Team==Team1]$latestFifa)
}

predict_data = predict(pp_settings, predict_data)

# Expected value for goals A against B

mA =  exp(predict(mA_glm, predict_data))
mB =  exp(predict(mB_glm, predict_data))


# lambda = mean(expected goals A , expected goals B)
lambdaAB = 0.5*(unname(mA) + unname(mB))
lambdaAB

# Expected value for goals B against A

predict_data$GoalA = lambdaAB

lambdaBA =  exp(predict(lambdaBA_glm, predict_data))
lambdaBA











