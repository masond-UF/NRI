# 13 April—seed bank/rain logistic GLM ####
library(permute)
library(tidyverse)
raw <- read.csv("PACKETS_SPRING_2020.csv") # load in data
raw <- raw[,c(1:7,19)] # just keep the final outcome data
d <-  raw %>% 
	filter(PLOT != "REF") # separate out the reference plots
ref <-  raw %>% 
	filter(PLOT == "REF") # keep just the reference plots
# Permutate the missing data ####