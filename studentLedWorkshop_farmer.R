#Student-Led Workshop: Bayesian Inference
#ENVB506 3/13/2021

#Package Installation
install.packages("ggplot2")
install.packages("rstanarm")
install.packages("bayestestR")


#Libraries
library(ggplot2)
library(rstanarm)
library(bayestestR)
library(bayesplot)

#Enable Multi-Core (Optional, helps speed up rstanarm if you have excess memory.)
options(mc.cores = parallel::detectCores())



#### 1: Ant Species Richness: Bayesian Linear Regression

#A: Read Data
ants <- read.csv("newEnglandAnts.csv")

#B: Preview Data
head(ants)

#Fit a Frequentist Linear Model
frequentistModel <- lm(Richness.Bog ~ Elevation, data = ants)
summary(frequentistModel)

#Make a new Data Frame with the predicted data
predicted_df <- data.frame(Richness.Bog = predict(frequentistModel, ants), Elevation=ants$Elevation)

#Plot the Frequentist Model
ggplot(ants, aes(Elevation, Richness.Bog))+
  geom_point(color="green")+
  labs(x="Elevation (ft)", y="Species Richness (Bog)", title="Elevation Vs Species Richness (Bog) Frequentist Model")+
  geom_line(color="maroon", data=predicted_df, aes(x=Elevation, y=Richness.Bog))+
  theme_light()


# Fit a Bayesian Linear Model 
bayesianModel <- stan_glm(ants$Richness.Bog ~ ants$Elevation + ants$Latitude, data = ants)
describe_posterior(model)
