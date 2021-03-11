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
#from random generation (e.g. random samples). If you set.seed(5) on your own
#computer, the posterior distributions for your Bayesian parameters will be
#the same as in this document. 

#Enable Multi-Core Parallel Processing 
#(Optional, helps speed up rstanarm if you have multiple cpu cores and excess memory.)
options(mc.cores = parallel::detectCores())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Part 1: Ant Species Richness and Bayesian Linear Regression
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Section A: Data Preview
#1: Read Data
ants <- read.csv("newEnglandAnts.csv")

#2: Preview Data
head(ants)
##Ans: The Elevation and Latitude variables serve as explanatory variables, while 
#The Richness.Forest and Richness.Bog are response variables. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Section B: Creating a basic Frequentist linear model
#3: Fit a Frequentist Linear Model
frequentistModel <- glm(Richness.Forest ~ Elevation+Latitude, data = ants)
#This glm has Richness.Forest as a response variable and Elevation/Latitude as 
#its explanatory variables. 

#4: View Frequentist Model Parameters
parameters(frequentistModel)
##Ans: The frequentist model parameters are measured with things like standard 
#error, confidence intervals, and p-values. These follow the frequentist 
#paradigm, where parameters are exact values with a probability of being observed.
#For example

#5: Make a new Data Frame with the predicted data
frequentistPredicted <- data.frame(Richness.Forest = predict(frequentistModel, ants), Elevation=ants$Elevation, Latitude=ants$Latitude)

#6a: Plot the Frequentist Model of Elevation vs Forest Richness
ggplot(ants, aes(Elevation, Richness.Forest))+
  geom_point(color="chocolate2")+
  labs(x="Elevation (m)", y="Species Richness (Forest)", title="Elevation Vs Forest Ant Species Richness (With Frequentist Model)")+
  geom_line(color="cornflowerblue", data=frequentistPredicted, aes(x=Elevation, y=Richness.Forest))+
  theme_light()
#Plots Elevation vs Species Richness (Forest) including the model's regression line. 
##Ans: The model somewhat fits the data. The regression line could clearly be better. 

#6b:
ggplot(ants, aes(Latitude, Richness.Forest))+
  geom_point(color="chocolate2")+
  labs(x="Latitude (dec.deg.)", y="Species Richness (Forest)", title="Latitude Vs Forest Ant Species Richness (With Frequentist Model)")+
  geom_line(color="cornflowerblue", data=frequentistPredicted, aes(x=Latitude, y=Richness.Forest))+
  theme_light()
#Plots Latitude vs Species Richness (forest) including the model's regression line.

#6c: 
parameters(frequentistModel)
##Ans: The Latitude component of this model seems to be more important than elevation.
#The regression line from the Latitude graph seems to fit the data more effectively.
#By checking the model parameters again, we see that Elevation has a coeffecient
#of -0.01. This means that Elevation has very little effect on the regression line, 
#while Latitude does (with a coeffecient of -2.01).

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Section C: Creating a Bayesian linear model
#7a: Fit a Bayesian Linear Model 
bayesianModel <- stan_glm(Richness.Forest ~ Elevation+Latitude, data = ants)

#7b: Looking at the Console
##Ans: The default MCMC values for stan_glm yield 4 Markov chains, with 
#2000 iterations each. However, only 1000 iterations of the chain are actually
#sampled, meaning there are 4000 total samples across the 4 chains. 

#8: View Bayesian Model Parameters
parameters(bayesianModel)
#Shows the model parameters, which rather than being set parameters within some
#confidence interval, is instead the median of a distribution with some interval
#of credibility.

##Ans: The only thing that looks similar to the Frequentist model's parameters
#is the CI. However, for Frequentists, this means "confidence interval", whereas
#for Bayesians, this means "credibility interval". Additionally, the Bayesian 
#model uses 89% rather than 95%. 
#The Bayesian model has Medians rather than Coefficients, although these are 
#nearly the same for each parameter. Instead of a p-value, it has pd percentages.
#The Bayesian parameters also include the % In ROPE, Rhat, ESS, and Prior 
#distributions for each parameter. 


#9a. View MCMC Histogram of Latitude for different chains
color_scheme_set("viridisC")
#Sets the distribution color

mcmc_hist_by_chain(bayesianModel, pars = c("Latitude"))+ 
  vline_at(-2.001, col="royalblue3")+
  vline_at(-3.036, col="seagreen1")+
  vline_at(-0.940, col="seagreen1")+
  yaxis_ticks(on=TRUE)+
  yaxis_text(on=TRUE)+
  xlab("Markov Chain Latitude Values")+ ylab("Frequency of Occurance")
#Shows the histogram of Latitude model parameter values on each of the 4 Markov 
#Chains. The blue line represents the parameter median, and the green lines
#represent the 89% Credibility Interval

##Ans: The Markov Chain histograms for Latitude resemble a normal distribution.

#9b. View combined MCMC Histogram for the Latitude variable 
mcmc_hist(bayesianModel, pars = c("Latitude"))+ 
  vline_at(-2.001, col="royalblue3")+
  vline_at(-3.036, col="seagreen1")+
  vline_at(-0.940, col="seagreen1")+
  yaxis_ticks(on=TRUE)+
  yaxis_text(on=TRUE)+
  xlab("Markov Chain Latitude Values")+ ylab("Frequency of Occurance")
#Shows the combined histogram of Markov Chain latitude sample parameters. 

##Ans: The y-axis is in units of frequency, as this is a histogram which 
#represents how frequently each of the 4000 sample parameters from the Markov
#chain fell on a certain value. This histogram is centered at -2.001, which is
#the same exact median as seen for Latitude in the parameters() table. 

#9c: View MCMC Density for Predicted Latitude Values
mcmc_dens(bayesianModel, pars = c("Latitude"))+ 
  vline_at(-2.001, col="royalblue3")+
  vline_at(-3.036, col="seagreen1")+
  vline_at(-0.940, col="seagreen1")+
  yaxis_ticks(on=TRUE)+
  yaxis_text(on=TRUE)+
  yaxis_title(on=TRUE)+
  xlab("Latitude (dec.deg.)")+ ylab("Probability")
#Shows the probability distribution for the latitude model parameter. This
#distribution is derived from the histogram of sampled possible parameters from
#the MCMC algorithm.

##Ans: This distribution is now in terms of (bayesian) probability. The shape
#is exactly the same as the histogram from 9b, but is in units of probability.
#This is the posterior distribution of the Latitude parameter. 

#10. View MCMC Histogram for the Intercept Parameter
color_scheme_set("viridisA")
#Sets a new color for the following histogram. 

mcmc_hist(bayesianModel, pars = c("(Intercept)"))+ 
  vline_at(98.031, col="royalblue3")+
  vline_at(52.704, col="seagreen1")+
  vline_at(142.184, col="seagreen1")+
  yaxis_ticks(on=TRUE)+
  yaxis_text(on=TRUE)+
  yaxis_title(on=TRUE)+
  xlab("Markov Chain Intercept Value")+ ylab("Frequency of Occurance")

#10b. View MCMC probability distribution for the Intercept Parameter
mcmc_dens(bayesianModel, pars = c("(Intercept)"))+ 
  vline_at(98.031, col="royalblue3")+
  vline_at(52.704, col="seagreen1")+
  vline_at(142.184, col="seagreen1")+
  yaxis_ticks(on=TRUE)+
  yaxis_text(on=TRUE)+
  yaxis_title(on=TRUE)+
  xlab("Markov Chain Intercept Value")+ ylab("Probability")

#11: Plot Data with both Bayesian and Frequentist regression lines
bayesianPredicted <- data.frame(Richness.Forest = predict(bayesianModel, ants), Elevation=ants$Elevation, Latitude=ants$Latitude)
#Makes a new data frame with the predicted data from the Bayesian model

ggplot(ants, aes(x=Latitude))+
  geom_point(aes(y=Richness.Forest), color="seagreen")+
  labs(x="Latitude (dec.deg.)", y="Species Richness (Forest)", title="Latitude Vs Forest Ant Species Richness (With Both Models)")+
  geom_line(color="cornflowerblue", data=bayesianPredicted, aes(y=Richness.Forest, colour="Bayesian Model"))+
  geom_line(color="salmon", data=frequentistPredicted, aes(y=Richness.Forest, colour="Frequentist Model"))+
  theme_light()
#Plots the newEnglandAnts data, and plots the regression lines for both the
#Frequentist (pink) and Bayesian (blue) model

##Ans: The two regression lines are almost exactly the same: both types of models
#produced nearly the same parameters even through very different methods.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Section D: Model Metrics and Comparison
#12a: Checking Confidence Intervals
confint(frequentistModel)
#Returns a 95% confidence interval for the parameters of the frequentist model.

#12b: Checking Credibility interval with HDI
hdi(bayesianModel)
#Returns the Highest-Density Interval for each parameter of the model. 
#Similar to a confidence interval, but represents the interval of values which
#we assign an 89% credibility towards being the 'real' parameter. A confidence
#interval, taking a frequentist view, represents how often a parameter will
#fall within an interval

##Ans: The intervals for the Frequentist and Bayesian models are similar, but the
#HDI is slightly tighter. This would make sense, as it is an interval covering
#89% of the distribution of each parameter rather than 95%. 


#13a: Checking p-values from the Frequentist Model
parameters(frequentistModel)
#Returns the parameters table, which includes the p-values for each parameter.

##Ans: The p-value for the latitude parameter is 0.004

#13b: Checking pd percentages from the Bayesian Model
pd(bayesianModel)
#Returns a table with the p-direction values. These are a percentage between
#50%-100%.

##Ans: the p-direction for the latitude parameter is 99.80%, or 0.998

#13c: Comparing p to pd values
p_to_pd(0.004, direction="two-sided")
#Returns the equivalent pd-value for the Frequentist Latitude parameter. 

pd_to_p(0.998, direction="two-sided")
#Returns the equivalent p-value for the Bayesian Latitude parameter.

##Ans: It turns out that these values are equivalent to eachother. The conversion
#between p and pd values is simple, even though they represent different metrics
#under Frequentist vs Bayesian paradigms. 

#14: Checking how much of each credibility intervals falls within ROPE interval
rope(bayesianModel, ci=1)
#Shows how much of the credibility interval for each parameter falls within the 
#ROPE interval. ROPE: Region of Practical Equivalence. Similar to statistical
#significance, where the ROPE interval is the region corresponding to a 'null' 
#hypothesis, i.e. if a portion of a parameter's probability distribution falls
#within the ROPE interval, the parameter has that much probability of being 
#insignificant in the model. 

##Ans: The Intercept and Latitude Parameters are very significant. 0.03% and
#0.7% inside ROPE means these two parameters have a 0.03% and 0.7% chance of
#having no effect on the model, thus being highly significant. The Elevation 
#parameter is completely insignificant in this model as 100% of its sampled 
#posterior distribution falls in the ROPE interval. 

 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##### Part 2: Protist CO2 Flux and Bayesian Hypothesis Testing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Section A: Data Preview
#1: Read the protists data to a data frame
protists <- read.csv("protists.csv")

#2: Preview the contents of the data
head(protists)
#Returns the 