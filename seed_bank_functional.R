# 9 April 2020—Convert veg data to functional groups ####
veg_all <- read.csv("OKMME_veg.csv") 
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