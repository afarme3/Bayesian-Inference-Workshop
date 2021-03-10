#Student-Led Workshop: Bayesian Inference
#ENVB506 3/13/2021

#### Setup

#Package Installation
install.packages("ggplot2")
install.packages("rstanarm")
install.packages("bayestestR")
install.packages("parameters")
#Libraries
library(ggplot2)
library(rstanarm)
library(bayestestR)
library(parameters)
library(bayesplot) #Gets installed with rstanarm

#Set Random Generation Seed
set.seed(5)
#Sets an RNG seed. By setting the same seed, two users can obtain the same results
#from random generation (e.g. random samples)

#Enable Multi-Core Parallel Processing 
#(Optional, helps speed up rstanarm if you have multiple cpu cores and excess memory.)
options(mc.cores = parallel::detectCores())


#### Part 1: Ant Species Richness and Bayesian Linear Regression

## Section A: Data Preview
#1: Read Data
ants <- read.csv("newEnglandAnts.csv")

#2: Preview Data
head(ants)


## Section B: Creating a basic Frequentist linear model
#3: Fit a Frequentist Linear Model
frequentistModel <- lm(Richness.Forest ~ Elevation, data = ants)

#4: View Frequentist Model Parameters
parameters(frequentistModel)

#5: Make a new Data Frame with the predicted data
frequentistPredicted <- data.frame(Richness.Forest = predict(frequentistModel, ants), Elevation=ants$Elevation)

#6: Plot the Frequentist Model
ggplot(ants, aes(Elevation, Richness.Forest))+
  geom_point(color="chocolate2")+
  labs(x="Elevation (ft)", y="Species Richness (Forest)", title="Elevation Vs Forest Ant Species Richness (With Frequentist Model)")+
  geom_line(color="cornflowerblue", data=frequentistPredicted, aes(x=Elevation, y=Richness.Forest))+
  theme_light()


## Section C: Creating a Bayesian linear model
#7: Fit a Bayesian Linear Model 
bayesianModel <- stan_glm(Richness.Forest ~ Elevation, data = ants)

#8. View Bayesian Model Parameters
describe_posterior(bayesianModel)
#Shows the model parameters, and their distribution values. 

#9a: View MCMC Density for Predicted Elevation Values
color_scheme_set("viridisC")
mcmc_dens(bayesianModel, pars = c("Elevation"))+ 
  vline_at(-0.014, col="royalblue3")+
  vline_at(-0.023, col="seagreen1")+
  vline_at(-0.007, col="seagreen1")+
  yaxis_ticks(on=TRUE)+
  yaxis_text(on=TRUE)+
  yaxis_title(on=TRUE)+
  xlab("Markov Chain Elevation Values")+ ylab("Probability")
#Returns asmooth distribution of all the observed elevation values from all 
#MCMC chains. Shows the frequency of the MCMC algorithm producing a particular value.

#9b. View MCMC Histogram for the Elevation variable 
mcmc_hist(bayesianModel, pars = c("Elevation"))+ 
  vline_at(-0.015, col="royalblue3")+
  vline_at(-0.023, col="seagreen1")+
  vline_at(-0.007, col="seagreen1")+
  yaxis_ticks(on=TRUE)+
  yaxis_text(on=TRUE)+
  xlab("Markov Chain Elevation Values")+ ylab("Frequency of Occurance")

#9c. View MCMC Histogram for different chains
mcmc_hist_by_chain(bayesianModel, pars = c("Elevation"))+ 
  vline_at(-0.015, col="royalblue3")+
  vline_at(-0.023, col="seagreen1")+
  vline_at(-0.007, col="seagreen1")+
  yaxis_ticks(on=TRUE)+
  yaxis_text(on=TRUE)+
  xlab("Markov Chain Elevation Values")+ ylab("Frequency of Occurance")

#10. View MCMC Histogram for the Intercept Parameter
color_scheme_set("viridisA")
mcmc_hist(bayesianModel, pars = c("(Intercept)"))+ 
  vline_at(12.515, col="royalblue3")+
  vline_at(10.021, col="seagreen1")+
  vline_at(14.667, col="seagreen1")+
  yaxis_ticks(on=TRUE)+
  yaxis_text(on=TRUE)+
  yaxis_title(on=TRUE)+
  xlab("Markov Chain Intercept Value")+ ylab("Frequency of Occurance")


## Section D: Coefficient Significance
#11. Checking Credibility Intervals 
hdi(bayesianModel)
#Returns the Highest-Density Interval for each parameter of the model. 

#12. Checking how much of each credibility intervals falls within ROPE interval
rope(bayesianModel)
#Shows how much of the credibility interval for each parameter falls within the 
#ROPE interval. This percentages also serves as a probability for the parameter
#being zero and therefore having no effect on the model. 

## Section E: Model Comparison
#13. Make a new data frame with the predicted data from the Bayesian model
bayesianPredicted <- data.frame(Richness.Forest = predict(bayesianModel, ants), Elevation=ants$Elevation)

#14. Plot Data with both Bayesian and Frequentist regression lines
ggplot(ants, aes(Elevation))+
  geom_point(aes(y=Richness.Forest), color="chocolate2")+
  labs(x="Elevation (ft)", y="Species Richness (Forest)", title="Elevation Vs Forest Ant Species Richness (With Both Models)")+
  geom_line(color="cornflowerblue", data=bayesianPredicted, aes(y=Richness.Forest, colour="Bayesian Model"))+
  geom_line(color="goldenrod", data=frequentistPredicted, aes(y=Richness.Forest, colour="Frequentist Model"))+
  theme_light()
