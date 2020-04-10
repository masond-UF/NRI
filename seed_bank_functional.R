# 9 April 2020—Convert veg data to functional groups ####
library(tidyverse)
library(lubridate)
veg_all <- read.csv("OKMME_veg.csv") 
veg_all$Date <- as_date(paste0(veg_all$Year,"-", 
															 veg_all$Month,"-",
															 veg_all$Day))
# Seed bank lists ####
# DF (5 PY R. glabra, 3 PD Syringa, 10 ND P. sylvestris)
# GG (10 PY B. orellana, 5 PD H. vulgare, 10 ND B. vulgaris)
# OS (5 PY G. max, 5 PD C. annum, 5 ND P. sativum)
# WP (10 PY M. sylvestris, 10 PD T. aestivum, 10 ND S. alba)

PD <- c("BRJA", "BOCU", "AECY", "DICHA2", "OPUNT", "APIAC", "BOHI2", "GAVI", 
				"VARA", "NALE3", "LOPE", "GASUS", "CASTI2", "GAPU", "VARA", "DIOL",
				"NALE3", "PYCA2", "BOYS", "ASAS", "ENPE4", "EVVE", "ALLIUM", "CHAER",
				"SPCOC2", "NOBI2", "LITE3", "ASVE", "PLAR", "HEDR", "HECR9", "PLVI",
				"LIRI", "SOLID", "HEHI2", "AMPS", "ARPU", "CROTON", "ERLO5", "DICI",
				"SISYR", "TROH", "AGHE4", "ANDRO2", "LOBEL", "OENOT", "SETAR", "TRIOD",
				"HYTE2", "VEHA", "ERPU", "ERLE11")
ND <- c("THIFI","BOHI2", "AMDR", "LASE", "KRLA")
PY <- c("SIDA", "CLOVER", "VICIA", "COEQ")

# BOYS = BOIS
# Code for conversion ####
# Collect and sum the columns in each category
ND_veg <- veg_all[ ,intersect(names(veg_all),ND)]
ND_veg <- ND_veg %>% 
	mutate(ND = rowSums(.)) %>% 
	select(ND)
PD_veg <- veg_all[ ,intersect(names(veg_all),PD)]
PD_veg <- PD_veg %>% 
	mutate(PD = rowSums(.)) %>% 
	select(PD)
PY_veg <- veg_all[ ,intersect(names(veg_all),PY)]
PY_veg <- PY_veg %>% 
	mutate(PY = rowSums(.)) %>% 
	select(PY)
# Create a new data frame using the new columns ####
func_veg <- bind_cols(list(PD_veg, PY_veg, ND_veg))
write.csv(func_veg, file = "func_veg.csv")
