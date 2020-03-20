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
THIFI_april-REF <- veg_wide %>% 
	filter(species == "THIFI" & Month == "4" & Treatment == "REF")
# Attach the reference value to the corresponding sites
THIFI_april <- veg_wide %>% 
	filter(species == "THIFI" & Month == "4" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", THIFI_april_REF[3,10],
														ifelse(Site == "GG", THIFI_april_REF[4,10],
																	 ifelse(Site == "DF", THIFI_april_REF[1,10],
																	 			 ifelse(Site == "WP", THIFI_april_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
THIFI_april$Change <- (THIFI_april$abundance - as.numeric(THIFI_april$Reference))
# Calculate the average, SD, and CI 
THIFI_april_mean <- THIFI_april %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
THIFI_april_mean <- THIFI_april_mean %>% 
	mutate(Date = as_date(paste0("2019","-","4","-","21")))
# MAY ####
# Filter out the reference
THIFI_may_REF <- veg_wide %>% 
	filter(species == "THIFI" & Month == "5" & Treatment == "REF")
# Attach the reference value to the corresponding sites
THIFI_may <- veg_wide %>% 
	filter(species == "THIFI" & Month == "5" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", THIFI_may_REF[3,10],
														ifelse(Site == "GG", THIFI_may_REF[4,10],
																	 ifelse(Site == "DF", THIFI_may_REF[1,10],
																	 			 ifelse(Site == "WP", THIFI_may_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
THIFI_may$Change <- (THIFI_may$abundance - as.numeric(THIFI_may$Reference))
# Calculate the average, SD, and CI 
THIFI_may_mean <- THIFI_may %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
THIFI_may_mean <- THIFI_may_mean %>% 
	mutate(Date = as_date(paste0("2019","-","5","-","6")))
# JUNE ####
# Filter out the reference
THIFI_june_REF <- veg_wide %>% 
	filter(species == "THIFI" & Month == "6" & Treatment == "REF")
# Attach the reference value to the corresponding sites
THIFI_june <- veg_wide %>% 
	filter(species == "THIFI" & Month == "6" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", THIFI_june_REF[3,10],
														ifelse(Site == "GG", THIFI_june_REF[4,10],
																	 ifelse(Site == "DF", THIFI_june_REF[1,10],
																	 			 ifelse(Site == "WP", THIFI_june_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
THIFI_june$Change <- (THIFI_june$abundance - as.numeric(THIFI_june$Reference))
# Calculate the average, SD, and CI 
THIFI_june_mean <- THIFI_june %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
THIFI_june_mean <- THIFI_june_mean %>% 
	mutate(Date = as_date(paste0("2019","-","6","-","2")))
# JULY ####
# Filter out the reference
THIFI_july_REF <- veg_wide %>% 
	filter(species == "THIFI" & Month == "7" & Treatment == "REF")
# Attach the reference value to the corresponding sites
THIFI_july <- veg_wide %>% 
	filter(species == "THIFI" & Month == "7" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", THIFI_july_REF[3,10],
														ifelse(Site == "GG", THIFI_july_REF[4,10],
																	 ifelse(Site == "DF", THIFI_july_REF[1,10],
																	 			 ifelse(Site == "WP", THIFI_july_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
THIFI_july$Change <- (THIFI_july$abundance - as.numeric(THIFI_july$Reference))
# Calculate the average, SD, and CI 
THIFI_july_mean <- THIFI_july %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
THIFI_july_mean <- THIFI_july_mean %>% 
	mutate(Date = as_date(paste0("2019","-","7","-","21")))
# Figure ####
THIFI_final <- rbind(THIFI_april_mean, THIFI_may_mean, 
										THIFI_june_mean, THIFI_july_mean)
ggplot(THIFI_final, aes(x = Date, y = Mean))+
	geom_point()+
	geom_line()+
	geom_errorbar(aes(ymin = Mean-SD, ymax = Mean+SD))+
	facet_wrap(~Treatment)
