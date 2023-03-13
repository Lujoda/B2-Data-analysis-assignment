# 1.) Explore the data and produce exploratory graphs

# 2.) Build a linear model Spp vs Env without interactions and perform stepwise model selection and interpret the outputs

# 3.) Build a linear model Spp vs Env with interactions and perform stepwise model selection and interpret the outputs

# 4.) Perform an nMDS analysis on the total community and produce graph (hint: use the vegan package)

# 5.) Perform an RDA analysis on the total community and produce graph (hint: use the vegan package)


#### Packages ####
pacman::p_load(here, tidyverse, ggplot2, car, GGally, emmeans, corrplot)

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


do_sp16_env = ggpairs(data, columns = c(16,21:25), lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.7)))+
              theme_classic()

### Correlation plot heatmap
corrplot(cor(data), method = "color", type = "lower",  tl.col = "black", tl.srt = 45)


      ## Correlations of varying degree present between species abundances and environmental variables
      ## Species abundances are often normally distributed but some are skewed, mostly to the right


#### Linear models ####

##### Stepwise model selection #####

intercept_model = lm(spp16 ~ 1, data = data)


full_model = lm(spp16 ~ env1 + env2 + env3 + env4 +env5, data = data)


##### Forward stepwise model selection #####

f_lm = step(intercept_model, scope=formula(full_model), directions = "forward")
summary(f_lm)
