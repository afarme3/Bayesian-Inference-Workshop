#Student-Led Workshop: Bayesian Inference
#ENVB506 3/13/2021

#### Setup

#Package Installation
install.packages("ggplot2")
install.packages("rstanarm")
install.packages("bayestestR")

#Libraries
library(ggplot2)
library(rstanarm)
library(bayestestR)
library(bayesplot) #Gets installed with rstanarm

#Set Random Generation Seed
set.seed(5)
#Sets an RNG seed. By setting the same seed, two users can obtain the same results
#from random generation (e.g. random samples)

#Enable Multi-Core Parallel Processing 
#(Optional, helps speed up rstanarm if you have multiple cpu cores and excess memory.)
options(mc.cores = parallel::detectCores())


#### A: Ant Species Richness: Bayesian Linear Regression

#1: Read Data
ants <- read.csv("newEnglandAnts.csv")

#2: Preview Data
head(ants)

#3: Fit a Frequentist Linear Model
frequentistModel <- lm(Richness.Bog ~ Elevation, data = ants)

#4: Make a new Data Frame with the predicted data
frequentistPredicted <- data.frame(Richness.Bog = predict(frequentistModel, ants), Elevation=ants$Elevation)

#5: Plot the Frequentist Model
ggplot(ants, aes(Elevation, Richness.Bog))+
  geom_point(color="chocolate2")+
  labs(x="Elevation (ft)", y="Species Richness (Bog)", title="Elevation Vs Species Richness (Bog) Frequentist Model")+
  geom_line(color="cornflowerblue", data=frequentistPredicted, aes(x=Elevation, y=Richness.Bog))+
  theme_light()

#6: View Frequentist Model Parameters
summary(frequentistModel)


#7: Fit a Bayesian Linear Model 
bayesianModel <- stan_glm(Richness.Bog ~ Elevation, data = ants)

#8. View Model Parameters
print(bayesianModel, digits=3)
#What is the median value for the Elevation parameter?
describe_posterior(bayesianModel)

#9: View MCMC Simulation for Elevation
mcmc_dens(bayesianModel, pars = c("Elevation")) + vline_at(-0.005, col="red")

#9. View Bayesian Model Parameters
