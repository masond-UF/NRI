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
BOCU_april-REF <- veg_wide %>% 
	filter(species == "BOCU" & Month == "4" & Treatment == "REF")
# Attach the reference value to the corresponding sites
BOCU_april <- veg_wide %>% 
	filter(species == "BOCU" & Month == "4" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", BOCU_april_REF[3,10],
														ifelse(Site == "GG", BOCU_april_REF[4,10],
																	 ifelse(Site == "DF", BOCU_april_REF[1,10],
																	 			 ifelse(Site == "WP", BOCU_april_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
BOCU_april$Change <- (BOCU_april$abundance - as.numeric(BOCU_april$Reference))
# Calculate the average, SD, and CI 
BOCU_april_mean <- BOCU_april %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
BOCU_april_mean <- BOCU_april_mean %>% 
	mutate(Date = as_date(paste0("2019","-","4","-","21")))
# MAY ####
# Filter out the reference
BOCU_may_REF <- veg_wide %>% 
	filter(species == "BOCU" & Month == "5" & Treatment == "REF")
# Attach the reference value to the corresponding sites
BOCU_may <- veg_wide %>% 
	filter(species == "BOCU" & Month == "5" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", BOCU_may_REF[3,10],
														ifelse(Site == "GG", BOCU_may_REF[4,10],
																	 ifelse(Site == "DF", BOCU_may_REF[1,10],
																	 			 ifelse(Site == "WP", BOCU_may_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
BOCU_may$Change <- (BOCU_may$abundance - as.numeric(BOCU_may$Reference))
# Calculate the average, SD, and CI 
BOCU_may_mean <- BOCU_may %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
BOCU_may_mean <- BOCU_may_mean %>% 
	mutate(Date = as_date(paste0("2019","-","5","-","6")))
# JUNE ####
# Filter out the reference
BOCU_june_REF <- veg_wide %>% 
	filter(species == "BOCU" & Month == "6" & Treatment == "REF")
# Attach the reference value to the corresponding sites
BOCU_june <- veg_wide %>% 
	filter(species == "BOCU" & Month == "6" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", BOCU_june_REF[3,10],
														ifelse(Site == "GG", BOCU_june_REF[4,10],
																	 ifelse(Site == "DF", BOCU_june_REF[1,10],
																	 			 ifelse(Site == "WP", BOCU_june_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
BOCU_june$Change <- (BOCU_june$abundance - as.numeric(BOCU_june$Reference))
# Calculate the average, SD, and CI 
BOCU_june_mean <- BOCU_june %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
BOCU_june_mean <- BOCU_june_mean %>% 
	mutate(Date = as_date(paste0("2019","-","6","-","2")))
# JULY ####
# Filter out the reference
BOCU_july_REF <- veg_wide %>% 
	filter(species == "BOCU" & Month == "7" & Treatment == "REF")
# Attach the reference value to the corresponding sites
BOCU_july <- veg_wide %>% 
	filter(species == "BOCU" & Month == "7" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", BOCU_july_REF[3,10],
														ifelse(Site == "GG", BOCU_july_REF[4,10],
																	 ifelse(Site == "DF", BOCU_july_REF[1,10],
																	 			 ifelse(Site == "WP", BOCU_july_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
BOCU_july$Change <- (BOCU_july$abundance - as.numeric(BOCU_july$Reference))
# Calculate the average, SD, and CI 
BOCU_july_mean <- BOCU_july %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
BOCU_july_mean <- BOCU_july_mean %>% 
	mutate(Date = as_date(paste0("2019","-","7","-","21")))
# Figure ####
BOCU_final <- rbind(BOCU_april_mean, BOCU_may_mean, 
										BOCU_june_mean, BOCU_july_mean)
ggplot(BOCU_final, aes(x = Date, y = Mean))+
	geom_point()+
	geom_line()+
	geom_errorbar(aes(ymin = Mean-SD, ymax = Mean+SD))+
	facet_wrap(~Treatment)
