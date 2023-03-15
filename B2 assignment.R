# 1.) Explore the data and produce exploratory graphs

# 2.) Build a linear model Spp vs Env without interactions and perform stepwise model selection and interpret the outputs

# 3.) Build a linear model Spp vs Env with interactions and perform stepwise model selection and interpret the outputs

# 4.) Perform an nMDS analysis on the total community and produce graph (hint: use the vegan package)

# 5.) Perform an RDA analysis on the total community and produce graph (hint: use the vegan package)

set.seed(1982)
#### Packages ####
pacman::p_load(here, tidyverse, ggplot2, car, GGally, emmeans, corrplot, vegan, viridis)

#### Read data ####

### Raw data
data = read.csv(here("data/ETX3_B2_multivariate_data2023.csv"))

### Only species
sp = data[1:20]

### Only environmental variables
env = data[21:25]


#### Data exploration ####

##### Basic statistics and information #####

# Dimensions
dim(data)   # 30 objects and 25 variables

# Structure
str(data) # species abundance data consists of integers, environmental variables are numeric, i.e. continuous

## Summary
summary(data) # Minumum, 1st quartile, median, mean, 3rd quartile and max values of each variable



#### Distribution and collinearity of predictors ####
do_env = ggpairs(data, columns = c(21:25), lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.7)))+
  theme_classic()
do_env
    
   
    ## All environmental variables are more or less normally distributed
    ## and slightly correlated to each other

    ## Is problematic colinearity present?

## Checking for collinearity with variance inflation parameter (VIF)
vif(lm(spp1 ~ env1 + env2 + env3 + env4 + env5, data = data))

    ## All VIF for predictors are far below 5, hence there is no problematic collinearity


#### Linearity between predictors (environmental variables) and dependent variables (species)

    ## Because of readablility only 5 species and the 5 environmental parameters are plotted in one graph 
do_sp1_sp5_env = ggpairs(data, columns = c(1:5,21:25), lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.7)))+
  theme_classic()
do_sp1_sp5_env

do_sp6_sp10_env = ggpairs(data, columns = c(6:10,21:25), lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.7)))+
  theme_classic()
do_sp6_sp10_env

do_sp11_sp15_env = ggpairs(data, columns = c(11:15,21:25), lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.7)))+
  theme_classic()
do_sp11_sp15_env

do_sp16_sp20_env = ggpairs(data, columns = c(16:20,21:25), lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.7)))+
  theme_classic()
do_sp16_sp20_env




### Correlation plot heatmap
corrplot(cor(data), method = "color", type = "lower",  tl.col = "black", tl.srt = 45)


      ## Correlations of varying degree present between species abundances and environmental variables
      ## Species abundances are often normally distributed but some are skewed, mostly to the right


### Distribution of species 16 (relevant species for linear models)
hist(data$spp16, breaks = 15, xlim = c(0,50), xlab = "Abundance of species 16", main = "Distribution of abundance of species 16 per measuring site")
    ## Approximately normally distributed


do_sp16_env = ggpairs(data, columns = c(16,21:25), lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.7)))+
  theme_classic()
do_sp16_env
    ## There are correlations between abundance of species 16 and the environmental parameters, the correlation between 
    ## spp16 and env4 being significant

#### Linear models ####

##### Stepwise model selection #####


    ## To apply stepwise model selection, the intercept model (model without any environmental predictor) and the
    ## full model (model with all environmental parameters as predictors) are specified

intercept_model = lm(spp16 ~ 1, data = data)


full_model = lm(spp16 ~ env1 + env2 + env3 + env4 + env5, data = data)


##### Forward stepwise model selection (no interaction) #####

for_lm = step(intercept_model, scope=formula(full_model), directions = "forward")
for_lm$coefficients

    ## Resulting model: spp16 = -0.327*env4 + 77
##### Backward stepwise model selection (no interaction) #####

back_lm = step(full_model, scope=formula(full_model), directions = "backward")
back_lm$coefficients

    ## Resulting model: spp16 = -0.327*env4 + 77
##### Both ways stepwise model selection (no interaction) #####

both_lm = step(intercept_model, scope=formula(full_model), directions = "both")
both_lm$coefficients

      ## Resulting model: spp16 = -0.327*env4 + 77


      ## All stepwise model selection methods without interactions resulted in the same model 
      ## using only the predictor env4 to predict abundance of species 16

plot(for_lm)
      ## Assumptions are fulfilled:   
      ## residuals are homogeneous (Homoscedasticity)
      ## Normal distribution of error
      ## There are no infuential outliers
      ## Observations should be independent, but  data source is unknown
      ## Linearity has been checked in line 96
      ## Multicollinearity has been excluded in line 54

summary(for_lm)
      ## The relationship between env4 and spp16 is highly significant (p < 8*10^-5)
      ## But only about 43 % of variance are explained by the model



##### Stepwise model selection (with interaction) #####


## This time the full model includes interactions

intercept_model = lm(spp16 ~ 1, data = data)


full_model_int = lm(spp16 ~ env1 * env2 * env3 * env4 * env5, data = data)
summary(full_model_int)

    ## With all interactions included in the full model, there are more predictors than observations
    ## This is impossible to calculate
    ## To demonstrate the inclusion of interaction terms nevertheless, the parameter env5 is simply removed
    ## from the model

full_model_int = lm(spp16 ~ env1 * env2 * env3 * env4, data = data)
summary(full_model_int)


    ## Now stepwise model selection can be done:

##### Forward stepwise model selection (with interaction) #####

for_lm_int = step(intercept_model, scope=formula(full_model_int), directions = "forward")
for_lm_int$coefficients

## Resulting model: spp16 = -0.327*env4 + 77.552 (only env4 as predictor)
##### Backward stepwise model selection (with interaction) #####

back_lm_int = step(full_model_int, scope=formula(full_model_int), directions = "backward")
back_lm_int$coefficients

## Resulting model contains the predictors env1, env2, env3, env4, env2:env3, env1:env4, env2:env4, 
## env3:env4 and env2:env3:env4

##### Both ways stepwise model selection (with interaction) #####

both_lm_int = step(intercept_model, scope=formula(full_model_int), directions = "both")
both_lm_int$coefficients

## Resulting model: spp16 = -0.327*env4 + 77.552 (only env4 as predictor), same model as forward stepwise selection


## Forward and both directions stepwise selection resulted in a sparce model containing only env4 as a predictor
## Backward stepwise selection included env1, env2, env3, env4, env2:env3, env1:env4, env2:env4, 
## env3:env4 and env2:env3:env4

##### Checking assumptions and interpretation ####

### Forward and both directions stepwise model selection

plot(for_lm_int)
## Assumptions for forward and both direction stepwise selection models are fulfilled:   
## residuals are homogeneous (Homoscedasticity)
## Normal distribution of error
## There are no infuential outliers
## Observations should be independent, but  data source is unknown
## Linearity has been checked in line 96
## Multicollinearity has been excluded in line 54

### Backward model selection

plot(back_lm_int)
## Assumptions for backward stepwise selection model are not completely fulfilled:   
## residuals are only more or less homogeneous (Homoscedasticity)
## Error is normally distributed although not as clearly as the previous models
## There are at least two infuential outliers
## Observations should be independent, but  data source is unknown
## Linearity has been checked in line 96
## Multicollinearity is present due to interaction terms (see below, line 224-228) 

    ## Checking for multilinearity in the linear model with interaction terms
vif(lm(spp1 ~ env1 * env2 * env3 * env4, data = data)) # Very high VIFs -> multicollinearity
vif(lm(spp1 ~ env1 + env2 + env3 + env4, data = data)) # Low VIFs in the model without interactions
vif(lm(spp1 ~ env1:env2 + env2:env3 + env1:env4 + env2:env4 + env3:env4 + env1:env2:env3 +env1:env2:env4 + env1:env3:env4 +
         env2:env3:env4 + env1:env2:env3:env4, data = data)) # High VIFs in model with only interactions

### Interpretation forward and both directions model

summary(for_lm_int)
    ## The relationship between env4 and spp16 is highly significant (p < 8*10^-5)
    ## But only about 43 % of variance are explained by the model (or 41 % when considering adjusted R^2)


### Interpretation backward stepwise selection model
summary(back_lm_int)

    ## There is a significant relationship between model predictors and abundance of species 16 (p = 0.022 < 0.05)
    ## R^2 is higher than the sparse model including only env4, however adjusted R^2 is lower due to the number 
    ## of predictors. The relationship is also less significant.
    ## Overall the sparse model including only env4 (and no interactions) would be preferred as it explains a
    ## considerable degree of variance using only one predictor and very low p-value



#### NMDS ####

  # Calculating NMDS using "Bray-Curtis-dissimilarity" as a distance metric
nmds = metaMDS(comm = data[,1:20],
               distance = "bray",
               try = 100)

  # Plotting the NMDS graph
plot(nmds)
orditorp(nmds,display="species",col="red",air=0.01, cex = 0.8) # indicating species points in graph 
orditorp(nmds,display="sites",cex=1.25,air=0.01) # indicating site points in graph

  ## A multitude of information may be gathered from the NMDS graph. Some of the insights might be:
  ## 1.) site 27 and 2 are very different
  ## 2.) Sites 13, 5, 14, 11, 9 and 17 cluster together and seem to be rather similar
  ## 3.) Species 8 and species 3 seem to be the most similar species
  ## 4.) Species 2 and species 13 are the most different


