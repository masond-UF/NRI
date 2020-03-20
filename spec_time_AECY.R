# 19 March 2020 ####
veg_all <- read.csv("OKMME_veg.csv") 
library(tidyr)
library(ggplot2)
library(lubridate)
# Make the data wide
veg_wide <- veg_all %>% 
	pivot_longer(-Site:-Date,
							 names_to = "species", values_to = "abundance")
# APRIL ####
# Filter out the reference
AECY_april-REF <- veg_wide %>% 
	filter(species == "AECY" & Month == "4" & Treatment == "REF")
# Attach the reference value to the corresponding sites
AECY_april <- veg_wide %>% 
	filter(species == "AECY" & Month == "4" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", AECY_april_REF[3,10],
														ifelse(Site == "GG", AECY_april_REF[4,10],
																	 ifelse(Site == "DF", AECY_april_REF[1,10],
																	 			 ifelse(Site == "WP", AECY_april_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
AECY_april$Change <- (AECY_april$abundance - as.numeric(AECY_april$Reference))
# Calculate the average, SD, and CI 
AECY_april_mean <- AECY_april %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
AECY_april_mean <- AECY_april_mean %>% 
	mutate(Date = as_date(paste0("2019","-","4","-","21")))
# MAY ####
# Filter out the reference
AECY_may_REF <- veg_wide %>% 
	filter(species == "AECY" & Month == "5" & Treatment == "REF")
# Attach the reference value to the corresponding sites
AECY_may <- veg_wide %>% 
	filter(species == "AECY" & Month == "5" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", AECY_may_REF[3,10],
														ifelse(Site == "GG", AECY_may_REF[4,10],
																	 ifelse(Site == "DF", AECY_may_REF[1,10],
																	 			 ifelse(Site == "WP", AECY_may_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
AECY_may$Change <- (AECY_may$abundance - as.numeric(AECY_may$Reference))
# Calculate the average, SD, and CI 
AECY_may_mean <- AECY_may %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
AECY_may_mean <- AECY_may_mean %>% 
	mutate(Date = as_date(paste0("2019","-","5","-","6")))
# JUNE ####
# Filter out the reference
AECY_june_REF <- veg_wide %>% 
	filter(species == "AECY" & Month == "6" & Treatment == "REF")
# Attach the reference value to the corresponding sites
AECY_june <- veg_wide %>% 
	filter(species == "AECY" & Month == "6" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", AECY_june_REF[3,10],
														ifelse(Site == "GG", AECY_june_REF[4,10],
																	 ifelse(Site == "DF", AECY_june_REF[1,10],
																	 			 ifelse(Site == "WP", AECY_june_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
AECY_june$Change <- (AECY_june$abundance - as.numeric(AECY_june$Reference))
# Calculate the average, SD, and CI 
AECY_june_mean <- AECY_june %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
AECY_june_mean <- AECY_june_mean %>% 
	mutate(Date = as_date(paste0("2019","-","6","-","2")))
# JULY ####
# Filter out the reference
AECY_july_REF <- veg_wide %>% 
	filter(species == "AECY" & Month == "7" & Treatment == "REF")
# Attach the reference value to the corresponding sites
AECY_july <- veg_wide %>% 
	filter(species == "AECY" & Month == "7" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", AECY_july_REF[3,10],
														ifelse(Site == "GG", AECY_july_REF[4,10],
																	 ifelse(Site == "DF", AECY_july_REF[1,10],
																	 			 ifelse(Site == "WP", AECY_july_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
AECY_july$Change <- (AECY_july$abundance - as.numeric(AECY_july$Reference))
# Calculate the average, SD, and CI 
AECY_july_mean <- AECY_july %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
AECY_july_mean <- AECY_july_mean %>% 
	mutate(Date = as_date(paste0("2019","-","7","-","21")))
# Figure ####
AECY_final <- rbind(AECY_april_mean, AECY_may_mean, 
										AECY_june_mean, AECY_july_mean)
ggplot(AECY_final, aes(x = Date, y = Mean))+
	geom_point()+
	geom_line()+
	geom_errorbar(aes(ymin = Mean-SD, ymax = Mean+SD))+
	facet_wrap(~Treatment)
