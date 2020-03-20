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
AMPS_april-REF <- veg_wide %>% 
	filter(species == "AMPS" & Month == "4" & Treatment == "REF")
# Attach the reference value to the corresponding sites
AMPS_april <- veg_wide %>% 
	filter(species == "AMPS" & Month == "4" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", AMPS_april_REF[3,10],
														ifelse(Site == "GG", AMPS_april_REF[4,10],
																	 ifelse(Site == "DF", AMPS_april_REF[1,10],
																	 			 ifelse(Site == "WP", AMPS_april_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
AMPS_april$Change <- (AMPS_april$abundance - as.numeric(AMPS_april$Reference))
# Calculate the average, SD, and CI 
AMPS_april_mean <- AMPS_april %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
AMPS_april_mean <- AMPS_april_mean %>% 
	mutate(Date = as_date(paste0("2019","-","4","-","21")))
# MAY ####
# Filter out the reference
AMPS_may_REF <- veg_wide %>% 
	filter(species == "AMPS" & Month == "5" & Treatment == "REF")
# Attach the reference value to the corresponding sites
AMPS_may <- veg_wide %>% 
	filter(species == "AMPS" & Month == "5" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", AMPS_may_REF[3,10],
														ifelse(Site == "GG", AMPS_may_REF[4,10],
																	 ifelse(Site == "DF", AMPS_may_REF[1,10],
																	 			 ifelse(Site == "WP", AMPS_may_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
AMPS_may$Change <- (AMPS_may$abundance - as.numeric(AMPS_may$Reference))
# Calculate the average, SD, and CI 
AMPS_may_mean <- AMPS_may %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
AMPS_may_mean <- AMPS_may_mean %>% 
	mutate(Date = as_date(paste0("2019","-","5","-","6")))
# JUNE ####
# Filter out the reference
AMPS_june_REF <- veg_wide %>% 
	filter(species == "AMPS" & Month == "6" & Treatment == "REF")
# Attach the reference value to the corresponding sites
AMPS_june <- veg_wide %>% 
	filter(species == "AMPS" & Month == "6" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", AMPS_june_REF[3,10],
														ifelse(Site == "GG", AMPS_june_REF[4,10],
																	 ifelse(Site == "DF", AMPS_june_REF[1,10],
																	 			 ifelse(Site == "WP", AMPS_june_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
AMPS_june$Change <- (AMPS_june$abundance - as.numeric(AMPS_june$Reference))
# Calculate the average, SD, and CI 
AMPS_june_mean <- AMPS_june %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
AMPS_june_mean <- AMPS_june_mean %>% 
	mutate(Date = as_date(paste0("2019","-","6","-","2")))
# JULY ####
# Filter out the reference
AMPS_july_REF <- veg_wide %>% 
	filter(species == "AMPS" & Month == "7" & Treatment == "REF")
# Attach the reference value to the corresponding sites
AMPS_july <- veg_wide %>% 
	filter(species == "AMPS" & Month == "7" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", AMPS_july_REF[3,10],
														ifelse(Site == "GG", AMPS_july_REF[4,10],
																	 ifelse(Site == "DF", AMPS_july_REF[1,10],
																	 			 ifelse(Site == "WP", AMPS_july_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
AMPS_july$Change <- (AMPS_july$abundance - as.numeric(AMPS_july$Reference))
# Calculate the average, SD, and CI 
AMPS_july_mean <- AMPS_july %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
AMPS_july_mean <- AMPS_july_mean %>% 
	mutate(Date = as_date(paste0("2019","-","7","-","21")))
# Figure ####
AMPS_final <- rbind(AMPS_april_mean, AMPS_may_mean, 
										AMPS_june_mean, AMPS_july_mean)
ggplot(AMPS_final, aes(x = Date, y = Mean))+
	geom_point()+
	geom_line()+
	geom_errorbar(aes(ymin = Mean-SD, ymax = Mean+SD))+
	facet_wrap(~Treatment)
