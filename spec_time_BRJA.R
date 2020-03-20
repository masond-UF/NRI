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
BRJA_april-REF <- veg_wide %>% 
	filter(species == "BRJA" & Month == "4" & Treatment == "REF")
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
# MAY ####
# Filter out the reference
BRJA_may_REF <- veg_wide %>% 
	filter(species == "BRJA" & Month == "5" & Treatment == "REF")
# Attach the reference value to the corresponding sites
BRJA_may <- veg_wide %>% 
	filter(species == "BRJA" & Month == "5" & Treatment != "REF") %>% 
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
	mutate(Date = as_date(paste0("2019","-","5","-","6")))
# JUNE ####
# Filter out the reference
BRJA_june_REF <- veg_wide %>% 
	filter(species == "BRJA" & Month == "6" & Treatment == "REF")
# Attach the reference value to the corresponding sites
BRJA_june <- veg_wide %>% 
	filter(species == "BRJA" & Month == "6" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", BRJA_june_REF[3,10],
														ifelse(Site == "GG", BRJA_june_REF[4,10],
																	 ifelse(Site == "DF", BRJA_june_REF[1,10],
																	 			 ifelse(Site == "WP", BRJA_june_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
BRJA_june$Change <- (BRJA_june$abundance - as.numeric(BRJA_june$Reference))
# Calculate the average, SD, and CI 
BRJA_june_mean <- BRJA_june %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
BRJA_june_mean <- BRJA_june_mean %>% 
	mutate(Date = as_date(paste0("2019","-","6","-","2")))
# JULY ####
# Filter out the reference
BRJA_july_REF <- veg_wide %>% 
	filter(species == "BRJA" & Month == "7" & Treatment == "REF")
# Attach the reference value to the corresponding sites
BRJA_july <- veg_wide %>% 
	filter(species == "BRJA" & Month == "7" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", BRJA_july_REF[3,10],
														ifelse(Site == "GG", BRJA_july_REF[4,10],
																	 ifelse(Site == "DF", BRJA_july_REF[1,10],
																	 			 ifelse(Site == "WP", BRJA_july_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
BRJA_july$Change <- (BRJA_july$abundance - as.numeric(BRJA_july$Reference))
# Calculate the average, SD, and CI 
BRJA_july_mean <- BRJA_july %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
BRJA_july_mean <- BRJA_july_mean %>% 
	mutate(Date = as_date(paste0("2019","-","7","-","21")))
# Figure ####
BRJA_final <- rbind(BRJA_april_mean, BRJA_may_mean, 
										BRJA_june_mean, BRJA_july_mean)
ggplot(BRJA_final, aes(x = Date, y = Mean))+
	geom_point()+
	geom_line()+
	geom_errorbar(aes(ymin = Mean-SD, ymax = Mean+SD))+
	facet_wrap(~Treatment)
