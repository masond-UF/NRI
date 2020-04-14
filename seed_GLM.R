# 13 April—seed bank/rain logistic GLM ####
library(mice)
library(tidyverse)
library(lme4)
library(ggplot2)
raw <- read.csv("PACKETS_SPRING_2020.csv") # load in data
raw <- raw[,c(1:7,19)] # just keep the final outcome data
d <-  raw %>% 
	filter(PLOT != "REF") # separate out the reference plots
ref <-  raw %>% 
	filter(PLOT == "REF") # keep just the reference plots
# Date explortation ####
ggplot(d, aes(x = FINAL.STATUS))+
	geom_histogram()+
	facet_wrap(~TYPE)

ggplot(ref, aes(x = FINAL.STATUS))+
	geom_histogram()+
	facet_wrap(~TYPE)
# Permutate the missing data ####
imputed <- mice(data = d)
d <- complete(imputed)

imputed <- mice(data = ref)
ref <- complete(imputed)

# Conduct the GLM ####


m <- glmer(FINAL.STATUS ~ TYPE + BIOMASS + TREATMENT + (1|SITE) + (1|PACKET), 
					family = binomial, data = d)
summary(m)

# Translate odds-ratio into probabilities
exp(-1.1954)/(1+exp(-1.1954))
exp(-1.1954 - 0.6839)/(1+exp(-1.1954-0.6839))
exp(-1.1954 - 0.7826643)/(1+exp(-1.1954-0.7826643))

# Create effect size dataframe ####

ref_survival <- ref %>% 
	group_by(SITE, PACKET, TYPE) %>% 
	summarize(counts=n(), sums=sum(FINAL.STATUS)) %>% 
	mutate(prop = sums/counts) %>% 
	select(-counts, -sums)

survival <- d %>% 
	unite("TREATMENT", BIOMASS:TREATMENT, sep = "_") %>% 
	group_by(SITE, TREATMENT, PACKET, TYPE) %>% 
	summarize(counts=n(), sums=sum(FINAL.STATUS)) %>% 
	mutate(SURV = sums/counts) %>% 
	select(-counts, -sums) %>% 
	mutate(REF = ifelse((SITE == "DF" & TYPE == "ND") & (PACKET == "AA" | PACKET == "AT"), ref_survival[1,4],
							 ifelse((SITE == "DF" & TYPE == "PD") & (PACKET == "AA" | PACKET == "AT"), ref_survival[2,4],
							 ifelse((SITE == "DF" & TYPE == "PY") & (PACKET == "AA" | PACKET == "AT"), ref_survival[3,4],
							 ifelse((SITE == "DF" & TYPE == "ND") & (PACKET == "BA" | PACKET == "BU"), ref_survival[4,4],
							 ifelse((SITE == "DF" & TYPE == "PD") & (PACKET == "BA" | PACKET == "BU"), ref_survival[5,4],
							 ifelse((SITE == "DF" & TYPE == "PY") & (PACKET == "BA" | PACKET == "BU"), ref_survival[6,4],
							 ifelse((SITE == "GG" & TYPE == "ND") & (PACKET == "AA" | PACKET == "AT"), ref_survival[7,4],
							 ifelse((SITE == "GG" & TYPE == "PD") & (PACKET == "AA" | PACKET == "AT"), ref_survival[8,4],
							 ifelse((SITE == "GG" & TYPE == "PY") & (PACKET == "AA" | PACKET == "AT"), ref_survival[9,4],
							 ifelse((SITE == "GG" & TYPE == "ND") & (PACKET == "BA" | PACKET == "BU"), ref_survival[10,4],
							 ifelse((SITE == "GG" & TYPE == "PD") & (PACKET == "BA" | PACKET == "BU"), ref_survival[11,4],
							 ifelse((SITE == "GG" & TYPE == "PY") & (PACKET == "BA" | PACKET == "BU"), ref_survival[12,4],	
							 ifelse((SITE == "OS" & TYPE == "ND") & (PACKET == "AA" | PACKET == "AT"), ref_survival[13,4],
							 ifelse((SITE == "OS" & TYPE == "PD") & (PACKET == "AA" | PACKET == "AT"), ref_survival[14,4],
							 ifelse((SITE == "OS" & TYPE == "PY") & (PACKET == "AA" | PACKET == "AT"), ref_survival[15,4],
							 ifelse((SITE == "OS" & TYPE == "ND") & (PACKET == "BA" | PACKET == "BU"), ref_survival[16,4],
							 ifelse((SITE == "OS" & TYPE == "PD") & (PACKET == "BA" | PACKET == "BU"), ref_survival[17,4],
							 ifelse((SITE == "OS" & TYPE == "PY") & (PACKET == "BA" | PACKET == "BU"), ref_survival[18,4],
							 ifelse((SITE == "WP" & TYPE == "ND") & (PACKET == "AA" | PACKET == "AT"), ref_survival[19,4],
							 ifelse((SITE == "WP" & TYPE == "PD") & (PACKET == "AA" | PACKET == "AT"), ref_survival[20,4],
							 ifelse((SITE == "WP" & TYPE == "PY") & (PACKET == "AA" | PACKET == "AT"), ref_survival[21,4],
							 ifelse((SITE == "WP" & TYPE == "ND") & (PACKET == "BA" | PACKET == "BU"), ref_survival[22,4],
							 ifelse((SITE == "WP" & TYPE == "PD") & (PACKET == "BA" | PACKET == "BU"), ref_survival[23,4],
							 ifelse((SITE == "WP" & TYPE == "PY") & (PACKET == "BA" | PACKET == "BU"), ref_survival[24,4],
							 			 NA)))))))))))))))))))))))))

# Model the difference ####
survival$diff <- survival$SURV - as.numeric(survival$REF)
survival <- survival %>% 
	separate(TREATMENT, c("BIOMASS", "EXCL"), sep = "_")

hist(survival$diff)

m2 <- lme(fixed = diff ~ TYPE + BIOMASS + EXCL + PACKET,
				 random = ~1|SITE, data = survival)

plot(m2) # residuals homogenous
shapiro.test(m2$residuals) # residuals normally distributed
summary(m2)
# Make a boxplot ####
install.packages("seaborn.boxplot")
survival <- survival %>% 
	mutate(ln = log(survival))

ggplot(data = survival)+
	geom_boxplot(aes(x = PACKET, y = diff, fill  = TYPE))+
	facet_grid(BIOMASS~EXCL)
