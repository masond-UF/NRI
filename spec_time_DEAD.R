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
DEAD_april-REF <- veg_wide %>% 
	filter(species == "DEAD" & Month == "4" & Treatment == "REF")
# Attach the reference value to the corresponding sites
DEAD_april <- veg_wide %>% 
	filter(species == "DEAD" & Month == "4" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", DEAD_april_REF[3,10],
														ifelse(Site == "GG", DEAD_april_REF[4,10],
																	 ifelse(Site == "DF", DEAD_april_REF[1,10],
																	 			 ifelse(Site == "WP", DEAD_april_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
DEAD_april$Change <- (DEAD_april$abundance - as.numeric(DEAD_april$Reference))
# Calculate the average, SD, and CI 
DEAD_april_mean <- DEAD_april %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
DEAD_april_mean <- DEAD_april_mean %>% 
	mutate(Date = as_date(paste0("2019","-","4","-","21")))
# MAY ####
# Filter out the reference
DEAD_may_REF <- veg_wide %>% 
	filter(species == "DEAD" & Month == "5" & Treatment == "REF")
# Attach the reference value to the corresponding sites
DEAD_may <- veg_wide %>% 
	filter(species == "DEAD" & Month == "5" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", DEAD_may_REF[3,10],
														ifelse(Site == "GG", DEAD_may_REF[4,10],
																	 ifelse(Site == "DF", DEAD_may_REF[1,10],
																	 			 ifelse(Site == "WP", DEAD_may_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
DEAD_may$Change <- (DEAD_may$abundance - as.numeric(DEAD_may$Reference))
# Calculate the average, SD, and CI 
DEAD_may_mean <- DEAD_may %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
DEAD_may_mean <- DEAD_may_mean %>% 
	mutate(Date = as_date(paste0("2019","-","5","-","6")))
# JUNE ####
# Filter out the reference
DEAD_june_REF <- veg_wide %>% 
	filter(species == "DEAD" & Month == "6" & Treatment == "REF")
# Attach the reference value to the corresponding sites
DEAD_june <- veg_wide %>% 
	filter(species == "DEAD" & Month == "6" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", DEAD_june_REF[3,10],
														ifelse(Site == "GG", DEAD_june_REF[4,10],
																	 ifelse(Site == "DF", DEAD_june_REF[1,10],
																	 			 ifelse(Site == "WP", DEAD_june_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
DEAD_june$Change <- (DEAD_june$abundance - as.numeric(DEAD_june$Reference))
# Calculate the average, SD, and CI 
DEAD_june_mean <- DEAD_june %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
DEAD_june_mean <- DEAD_june_mean %>% 
	mutate(Date = as_date(paste0("2019","-","6","-","2")))
# JULY ####
# Filter out the reference
DEAD_july_REF <- veg_wide %>% 
	filter(species == "DEAD" & Month == "7" & Treatment == "REF")
# Attach the reference value to the corresponding sites
DEAD_july <- veg_wide %>% 
	filter(species == "DEAD" & Month == "7" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", DEAD_july_REF[3,10],
														ifelse(Site == "GG", DEAD_july_REF[4,10],
																	 ifelse(Site == "DF", DEAD_july_REF[1,10],
																	 			 ifelse(Site == "WP", DEAD_july_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
DEAD_july$Change <- (DEAD_july$abundance - as.numeric(DEAD_july$Reference))
# Calculate the average, SD, and CI 
DEAD_july_mean <- DEAD_july %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
DEAD_july_mean <- DEAD_july_mean %>% 
	mutate(Date = as_date(paste0("2019","-","7","-","21")))
# Figure ####
DEAD_final <- rbind(DEAD_april_mean, DEAD_may_mean, 
										DEAD_june_mean, DEAD_july_mean)
ggplot(DEAD_final, aes(x = Date, y = Mean))+
	geom_point()+
	geom_line()+
	geom_errorbar(aes(ymin = Mean-SD, ymax = Mean+SD))+
	facet_wrap(~Treatment)
