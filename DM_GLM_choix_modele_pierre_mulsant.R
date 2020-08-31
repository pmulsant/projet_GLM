meteo = read.csv("C:/Users/pierr/OneDrive/Documents/data_science/dauphine/cours/DM/meteo.train.csv")
meteo_test = read.csv("C:/Users/pierr/OneDrive/Documents/data_science/dauphine/cours/DM/meteo.test.csv")

# I. Température demain

# a) Etablissement du modèle

modelTemp = lm(temp.demain ~ 
                 Temperature.daily.mean..2.m.above.gnd.
               + Sunshine.Duration.daily.sum..sfc.
               + Mean.Sea.Level.Pressure.daily.mean..MSL. 
               + Wind.Speed.daily.mean..10.m.above.gnd.
               + Wind.Speed.daily.mean..80.m.above.gnd.
               + factor(Month)
               + Relative.Humidity.daily.mean..2.m.above.gnd.
               + Mean.Sea.Level.Pressure.daily.mean..MSL.
               + Total.Precipitation.daily.sum..sfc.
               + Snowfall.amount.raw.daily.sum..sfc.
               + Total.Cloud.Cover.daily.mean..sfc., data = meteo)
summary(modelTemp)

# modèle emboîté :
modelTemp0 = lm(temp.demain ~
                  Temperature.daily.mean..2.m.above.gnd.
                + Mean.Sea.Level.Pressure.daily.mean..MSL.
                + Wind.Speed.daily.mean..10.m.above.gnd.
                + Wind.Speed.daily.mean..80.m.above.gnd.
                + factor(Month), data = meteo)
summary(modelTemp0)
anova(modelTemp0, modelTemp)

# optimisation de l'AIC
step(modelTemp, data = meteo, direction = "both")
modelTempFinal = lm(temp.demain ~
                      Temperature.daily.mean..2.m.above.gnd.
                    + Mean.Sea.Level.Pressure.daily.mean..MSL.
                    + Wind.Speed.daily.mean..10.m.above.gnd.
                    + Wind.Speed.daily.mean..80.m.above.gnd.
                    + factor(Month)
                    + Relative.Humidity.daily.mean..2.m.above.gnd.
                    + Total.Cloud.Cover.daily.mean..sfc., data = meteo)
summary(modelTempFinal)

# b) Prédictions

temperatures = predict(modelTempFinal, new = meteo_test)
difference = sqrt(sum((temperatures - meteo_test$temp.demain)^2))/length(temperatures)


# II. Pluie demain

# a) Etablissement du modèle

modelPluie = glm(pluie.demain ~ 
                 Temperature.daily.mean..2.m.above.gnd.
               + Sunshine.Duration.daily.sum..sfc.
               + Mean.Sea.Level.Pressure.daily.mean..MSL. 
               + Wind.Speed.daily.mean..10.m.above.gnd.
               + Wind.Speed.daily.mean..80.m.above.gnd.
               + factor(Month)
               + Relative.Humidity.daily.mean..2.m.above.gnd.
               + Mean.Sea.Level.Pressure.daily.mean..MSL.
               + Total.Precipitation.daily.sum..sfc.
               + Snowfall.amount.raw.daily.sum..sfc.
               + Total.Cloud.Cover.daily.mean..sfc., family = binomial, data = meteo)
summary(modelPluie)

# optimisation de l'AIC

step(modelPluie, data = meteo, direction = "both")
modelPluieFinal = glm(pluie.demain ~ 
                   Temperature.daily.mean..2.m.above.gnd.
                 + Mean.Sea.Level.Pressure.daily.mean..MSL. 
                 + Wind.Speed.daily.mean..10.m.above.gnd.
                 + factor(Month)
                 + Relative.Humidity.daily.mean..2.m.above.gnd.
                 + Mean.Sea.Level.Pressure.daily.mean..MSL.
                 + Total.Precipitation.daily.sum..sfc.
                 + Snowfall.amount.raw.daily.sum..sfc.
                 + Total.Cloud.Cover.daily.mean..sfc., family = binomial, data = meteo)

# b) Prédictions

# on cherche à déterminer le meilleur seuil à prendre pour transformer les probabilités en booléen
good_prediction = function(data, probabilities, threshold){
  pluie = probabilities >= threshold
  good = pluie == data$pluie.demain
  table_good = table(good)
  return(100 * table_good[2] / length(good))
}

best_threshold = function(data, model){
  probabilities = predict(model, new = data, type = "response")
  thresholds = seq(0, 1, 0.1)
  successes = integer(11)
  for(index in 1:10){
    successes[index] = good_prediction(data, probabilities, thresholds[index])
  }
  best_index = which.max(successes)
  return(thresholds[best_index])
}

best_threshold(meteo, modelPluieFinal)

# On prédit sur les données de tests
pluie = predict(modelPluieFinal, new = meteo_test, type = "response") >= 0.5
goodPredictions = pluie == meteo_test$pluie.demain
table(goodPredictions)
100 * table(goodPredictions)[2]/length(goodPredictions)


# on écrit les prévisions :
previsions = data.frame(temp.demain = temperatures, pluie.demain = pluie)
write.csv(previsions, "C:/Users/pierr/OneDrive/Documents/data_science/dauphine/cours/DM/previsions.csv")