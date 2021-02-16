# Path Analysis
# PER
# Jaime Fanjul

rm(list=ls())
dev.off()


# Define working directory
#\""~
setwd("C:/Users/Jaime Fanjul/path/Regresion/Path_Analysis")


per=read.csv("per.csv",sep=";")
summary(per)

if(!require(lavaan)){install.packages("lavaan")}
if(!require(lavaanPlot)){install.packages("lavaanPlot")}
library(lavaanPlot)
library(lavaan)


# independence "~"
# covariance "~~"
# if the predictor is additive, the symbol is used "+"

# scale the values 

per$sedad=scale(per$edad)
per$srbc=scale(per$rbc)
per$speso=scale(per$peso)
per$splaq=scale(per$plaq)

summary(per)

mediation_model ='
# Estimate the relationship between weight with plaq, blood cells (rbc)
# age as mediation varible
speso~splaq+srbc
# Estimate the relation of age as mediation variable
sedad~splaq
speso~sedad
# Estimae the variance of exogenous variables
splaq~~splaq
srbc~~srbc
# Estimate endogenous variables residuals 
sedad~~sedad
speso~~speso'

mediation_model

adj_mm=lavaan(mediation_model, data = per)

summary(adj_mm,fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

parameterEstimates(adj_mm)

# suggested modifications in the model schema 
# changes must be justified
# ~~ suggests a bidirectional relationship and covariance
modificationIndices(adj_mm)

# plot the model schema with all the parameters
lavaanPlot(model = adj_mm, coefs = TRUE, covs = TRUE)

## Direct Model

# now lets see what is the result removing the mediation variable age and 
# using it as direct effect

direct_model ='
# Estimate the realationship (direct effect)
speso~sedad+srbc+splaq
# Estimate the variance of the exogenous variables
sedad~~sedad
srbc~~srbc
splaq~~splaq
# Exogenous variables residuals
speso~~speso
'
direct_model

adj_dm=lavaan(direct_model, data = per)

summary(adj_dm,fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

parameterEstimates(adj_dm)
modificationIndices(adj_dm)

lavaanPlot(model = adj_dm, coefs = TRUE, covs = TRUE)


# comparing the models with ANOVA test 
anova(adj_mm, adj_dm)

# concluding
# the tendency to lose weight due to a decrease in platelets in the blood in the direct model
# is compounded signifcantly by the mediating variable age in the mediation model increasing the weight

# mediation Model RMSEA = 0.103
# first parameter that recommends modifying the index --> add a covariance between platelets and red blood cells
# of the model fit statistics with mediating variable
# the one that shows the best result is SRMR, being acceptable.
# adding the mediating effect of the number of platelets improves the model with p <0.05
# 
