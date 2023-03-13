# Explore the data and produce exploratory graphs

# Build a linear model Spp vs Env without interactions and perform stepwise model selection and interpret the outputs

# Build a linear model Spp vs Env with interactions and perform stepwise model selection and interpret the outputs

# Perform an nMDS analysis on the total community and produce graph (hint: use the vegan package)

# Perform an RDA analysis on the total community and produce graph (hint: use the vegan package)


## Packages
pacman::p_load(here, tidyverse, ggplot2, car, GGally, emmeans)

## Read data

### Raw data
data = read.csv(here("data/ETX3_B2_multivariate_data2023.csv"))

### Only species
sp = data[1:20]

### Only enviromental variables
env = data[21:25]


## Data exploration
str(data)
summary(data)
head(data)

# Collinearity checks
(data_overview <- data%>%ggpairs(columns = 1:ncol(data), 
                                   # mapping = ggplot2::aes(colour = island),
                                    lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.7)))+
    theme_classic())

data_overview


## Linear models

## No interactions
lm = lm()