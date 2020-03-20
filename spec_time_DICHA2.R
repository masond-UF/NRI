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
DICHA2_april_REF <- veg_wide %>% 
	filter(species == "DICHA2" & Month == "4" & Treatment == "REF")
# Attach the reference value to the corresponding sites
DICHA2_april <- veg_wide %>% 
	filter(species == "DICHA2" & Month == "4" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", DICHA2_april_REF[3,10],
														ifelse(Site == "GG", DICHA2_april_REF[4,10],
																	 ifelse(Site == "DF", DICHA2_april_REF[1,10],
																	 			 ifelse(Site == "WP", DICHA2_april_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
DICHA2_april$Change <- (DICHA2_april$abundance - as.numeric(DICHA2_april$Reference))
# Calculate the average, SD, and CI 
DICHA2_april_mean <- DICHA2_april %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
DICHA2_april_mean <- DICHA2_april_mean %>% 
	mutate(Date = as_date(paste0("2019","-","4","-","21")))
# MAY ####
# Filter out the reference
DICHA2_may_REF <- veg_wide %>% 
	filter(species == "DICHA2" & Month == "5" & Treatment == "REF")
# Attach the reference value to the corresponding sites
DICHA2_may <- veg_wide %>% 
	filter(species == "DICHA2" & Month == "5" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", DICHA2_may_REF[3,10],
														ifelse(Site == "GG", DICHA2_may_REF[4,10],
																	 ifelse(Site == "DF", DICHA2_may_REF[1,10],
																	 			 ifelse(Site == "WP", DICHA2_may_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
DICHA2_may$Change <- (DICHA2_may$abundance - as.numeric(DICHA2_may$Reference))
# Calculate the average, SD, and CI 
DICHA2_may_mean <- DICHA2_may %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
DICHA2_may_mean <- DICHA2_may_mean %>% 
	mutate(Date = as_date(paste0("2019","-","5","-","6")))
# JUNE ####
# Filter out the reference
DICHA2_june_REF <- veg_wide %>% 
	filter(species == "DICHA2" & Month == "6" & Treatment == "REF")
# Attach the reference value to the corresponding sites
DICHA2_june <- veg_wide %>% 
	filter(species == "DICHA2" & Month == "6" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", DICHA2_june_REF[3,10],
														ifelse(Site == "GG", DICHA2_june_REF[4,10],
																	 ifelse(Site == "DF", DICHA2_june_REF[1,10],
																	 			 ifelse(Site == "WP", DICHA2_june_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
DICHA2_june$Change <- (DICHA2_june$abundance - as.numeric(DICHA2_june$Reference))
# Calculate the average, SD, and CI 
DICHA2_june_mean <- DICHA2_june %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
DICHA2_june_mean <- DICHA2_june_mean %>% 
	mutate(Date = as_date(paste0("2019","-","6","-","2")))
# JULY ####
# Filter out the reference
DICHA2_july_REF <- veg_wide %>% 
	filter(species == "DICHA2" & Month == "7" & Treatment == "REF")
# Attach the reference value to the corresponding sites
DICHA2_july <- veg_wide %>% 
	filter(species == "DICHA2" & Month == "7" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", DICHA2_july_REF[3,10],
														ifelse(Site == "GG", DICHA2_july_REF[4,10],
																	 ifelse(Site == "DF", DICHA2_july_REF[1,10],
																	 			 ifelse(Site == "WP", DICHA2_july_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
DICHA2_july$Change <- (DICHA2_july$abundance - as.numeric(DICHA2_july$Reference))
# Calculate the average, SD, and CI 
DICHA2_july_mean <- DICHA2_july %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
DICHA2_july_mean <- DICHA2_july_mean %>% 
	mutate(Date = as_date(paste0("2019","-","7","-","21")))
# Figure ####
DICHA2_final <- rbind(DICHA2_april_mean, DICHA2_may_mean, 
										DICHA2_june_mean, DICHA2_july_mean)
ggplot(DICHA2_final, aes(x = Date, y = Mean))+
	geom_point()+
	geom_line()+
	geom_errorbar(aes(ymin = Mean-SD, ymax = Mean+SD))+
	facet_wrap(~Treatment)
