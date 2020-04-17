# 20 March 2020 
library(ggplot2)
library(cowplot)
library(tidyverse)

spec_time <- function(x){

veg_all <- read.csv("OKMME_veg.csv") 
func_veg <- read.csv("func_veg.csv") 
func_veg <- func_veg[,2:5]
veg_all <- bind_cols(veg_all, func_veg)

library(tidyr)
library(ggplot2)
library(lubridate)
library(dplyr)
# Make the data wide
veg_wide <- veg_all %>% 
	pivot_longer(-Site:-Date,
							 names_to = "species", values_to = "abundance")
# APRIL ####
# Filter out the reference
april_REF <- veg_wide %>% 
	dplyr::filter(species == x & Month == "4" & Treatment == "REF")
# Attach the reference value to the corresponding sites
april <- veg_wide %>% 
	filter(species == x & Month == "4" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", april_REF[3,10],
														ifelse(Site == "GG", april_REF[4,10],
																	 ifelse(Site == "DF", april_REF[1,10],
																	 			 ifelse(Site == "WP", april_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
april$Change <- log(april$abundance/(as.numeric(april$Reference)+0.001)+ 0.001)
# Calculate the average, SD, and CI 
april_mean <- april %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
april_mean <- april_mean %>% 
	mutate(Date = as_date(paste0("2019","-","4","-","21")))
# MAY ####
# Filter out the reference
may_REF <- veg_wide %>% 
	filter(species == x & Month == "5" & Treatment == "REF")
# Attach the reference value to the corresponding sites
may <- veg_wide %>% 
	filter(species == x & Month == "5" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", may_REF[3,10],
														ifelse(Site == "GG", may_REF[4,10],
																	 ifelse(Site == "DF", may_REF[1,10],
																	 			 ifelse(Site == "WP", may_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
may$Change <- log(may$abundance/(as.numeric(may$Reference)+0.001)+ 0.001)
# Calculate the average, SD, and CI 
may_mean <- may %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
may_mean <- may_mean %>% 
	mutate(Date = as_date(paste0("2019","-","5","-","6")))
# JUNE
# Filter out the reference
june_REF <- veg_wide %>% 
	filter(species == x & Month == "6" & Treatment == "REF")
# Attach the reference value to the corresponding sites
june <- veg_wide %>% 
	filter(species == x & Month == "6" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", june_REF[3,10],
														ifelse(Site == "GG", june_REF[4,10],
																	 ifelse(Site == "DF", june_REF[1,10],
																	 			 ifelse(Site == "WP", june_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
june$Change <- log(june$abundance/(as.numeric(june$Reference)+0.001)+ 0.001)
# Calculate the average, SD, and CI 
june_mean <- june %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
june_mean <- june_mean %>% 
	mutate(Date = as_date(paste0("2019","-","6","-","2")))
# JULY
# Filter out the reference
july_REF <- veg_wide %>% 
	filter(species == x & Month == "7" & Treatment == "REF")
# Attach the reference value to the corresponding sites
july <- veg_wide %>% 
	filter(species == x & Month == "7" & Treatment != "REF") %>% 
	mutate(Reference = ifelse(Site == "OS", july_REF[3,10],
														ifelse(Site == "GG", july_REF[4,10],
																	 ifelse(Site == "DF", july_REF[1,10],
																	 			 ifelse(Site == "WP", july_REF[2,10],NA)))))
# Calculate the difference between the abundance and the corresponding reference 
july$Change <- log(july$abundance/(as.numeric(july$Reference)+0.001)+ 0.001)
# Calculate the average, SD, and CI 
july_mean <- july %>% 
	group_by(Treatment) %>% 
	summarise(Mean = mean(Change), SD = sd(Change), CI = (2*sd(Change)))
# Add the date
july_mean <- july_mean %>% 
	mutate(Date = as_date(paste0("2019","-","7","-","21")))
# Final
final <- rbind(april_mean, may_mean, 
										june_mean, july_mean)
final$Treatment <- factor(final$Treatment, levels = c("CH", "CO", "CS", "MH", "MO", "MS"),
											labels = c("Low biomass & herbivore exclusion", 
																 "Low biomass & open",
																 "Low biomass & scavenger exclusion",
																 "High biomass & herbivore exclusion",
																 "High biomass and open",
																 "High biomass & scavenger exclusion"))
return(final)
} 
spec_time_figure <- function(x){
p <- ggplot(x, aes(x = Date, y = Mean))+
	geom_point()+
	ylab("Mean Abundance Effect Size")+
	ylim(-12,8)+
	ggtitle(deparse(substitute(x)))+  
	geom_errorbar(aes(ymin = Mean-SD, ymax = Mean+SD), width = 5)+
	theme_bw()+
	theme(plot.title = element_text(hjust = 0.5))+
	theme(strip.background =element_rect(fill="black"))+
	theme(strip.text = element_text(colour = 'white'))+
	theme(panel.grid.minor = element_blank())+
	facet_wrap(~Treatment)+
	theme(strip.text.x = element_text(size = 8))+
	theme(axis.text.x = element_text(size = 8))+
  scale_x_date(limits = as.Date(c("2019-04-21", "2019-07-21")))+
	scale_x_date(limits = as.Date(c("2019-03-21", "2019-07-21")),
	date_labels = "%b-%y",
	date_breaks = "1 month")
return(p)
}

breaks = c("2019-04-21", "2019-05-06",
					"2019-06-02", "2019-07-21")

PD <- spec_time("PD")
ND <- spec_time("ND")
PY <- spec_time("PY")
NONE <- spec_time("NONE")

PD_fig <- spec_time_figure(PD)
PY_fig <- spec_time_figure(PY)
ND_fig <- spec_time_figure(ND)
NONE_fig <- spec_time_figure(NONE)

panel_c <- plot_grid(PD_fig, PY_fig, ND_fig, NONE_fig, 
					 labels = c('C.1', 'C.2', 'C.3', 'C.4'),
					 label_colour = "red")

for_marcus <- plot_grid(PD_fig, PY_fig, ND_fig, NONE_fig)
