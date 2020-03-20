# 19 March 2020 ####
veg_all <- read.csv("OKMME_veg.csv") 
library(tidyr)
library(ggplot2)
library(lubridate)
# Make the data wide
veg_wide <- veg_all %>% 
							 pivot_longer(-Site:-Date,
							 names_to = "species", values_to = "abundance")

REF <- veg_wide %>% 
	filter(Treatment == "REF")

# APRIL ####
# Filter out the reference
BRJA_april-REF <- veg_wide %>% 
	filter(species == "BRJA" & Month == "5" & Treatment == "REF")
# Attach the reference value to the corresponding sites
BRJA_april <- veg_wide %>% 
	filter(species == "BRJA" & Month == "4" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", BRJA_april_REF[3,10],
														ifelse(Site == "GG", BRJA_april_REF[4,10],
																	 ifelse(Site == "DF", BRJA_april_REF[1,10],
																	 			 ifelse(Site == "WP", BRJA_april_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
BRJA_april$Change <- (BRJA_april$abundance - as.numeric(BRJA_april$Reference))
# Calculate the average, SD, and CI 
BRJA_april_mean <- BRJA_april %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
BRJA_april_mean <- BRJA_april_mean %>% 
	mutate(Date = as_date(paste0("2019","-","4","-","21")))
# Calculate the difference between the abundance and the corresponding reference 
BRJA_april$Change <- (BRJA_april$abundance - as.numeric(BRJA_april$Reference))
# Calculate the average, SD, and CI 
BRJA_april_mean <- BRJA_april %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# MAY ####
# Filter out the reference
BRJA_may_REF <- veg_wide %>% 
	filter(species == "BRJA" & Month == "4" & Treatment == "REF")
# Attach the reference value to the corresponding sites
BRJA_may <- veg_wide %>% 
	filter(species == "BRJA" & Month == "4" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", BRJA_may_REF[3,10],
														ifelse(Site == "GG", BRJA_may_REF[4,10],
																	 ifelse(Site == "DF", BRJA_may_REF[1,10],
																	 			 ifelse(Site == "WP", BRJA_may_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
BRJA_may$Change <- (BRJA_may$abundance - as.numeric(BRJA_may$Reference))
# Calculate the average, SD, and CI 
BRJA_may_mean <- BRJA_may %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
BRJA_may_mean <- BRJA_may_mean %>% 
	mutate(Date = as_date(paste0("2019","-","4","-","21")))
# Calculate the difference between the abundance and the corresponding reference 
BRJA_may$Change <- (BRJA_may$abundance - as.numeric(BRJA_may$Reference))
# Calculate the average, SD, and CI 
BRJA_may_mean <- BRJA_may %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
BRJA_may_mean <- BRJA_may_mean %>% 
	mutate(Date = as_date(paste0("2019","-","4","-","21")))



