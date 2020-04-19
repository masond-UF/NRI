# 13 April—seed bank/rain logistic GLM ####
library(mice)
library(tidyverse)
library(ggplot2)
library(gridExtra)

raw <- read.csv("PACKETS_SPRING_2020.csv") # load in data
d <-  raw %>% 
	filter(PLOT != "REF") # separate out the reference plots
ref <-  raw %>% 
	filter(PLOT == "REF") # keep just the reference plots
# Date explortation ####
# ggplot(d, aes(x = FINAL.STATUS))+
	# geom_histogram()+
	# facet_wrap(~TYPE)

# ggplot(ref, aes(x = FINAL.STATUS))+
	#geom_histogram()+
	#facet_wrap(~TYPE)
# Permutate the missing data ####
imputed <- mice(data = d)
d <- complete(imputed)

imputed <- mice(data = ref)
ref <- complete(imputed)

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
				 random = ~1|SITE, data = survival, method = "ML")
m3 <- lme(fixed = diff ~ TYPE + EXCL + PACKET,
				 random = ~1|SITE, data = survival, method = "ML")
m4 <- lme(fixed = diff ~ TYPE + BIOMASS + PACKET,
				 random = ~1|SITE, data = survival, method = "ML")
m5 <- lme(fixed = diff ~ TYPE + PACKET,
				 random = ~1|SITE, data = survival, method = "ML")
# Make a boxplot [SKIP]####

# ggplot(data = survival, aes(x = PACKET, y = diff))+
	# geom_boxplot(aes(fill  = TYPE))+
	# scale_fill_viridis_d(option = "D")+
	# ylab("Δ Survival")+
	# labs(fill = "Functional group")+
	# facet_grid(BIOMASS~EXCL)+
	# xlab("Placement")+
	# theme_bw()


# ggplot(data = survival, aes(x = PACKET, y = diff, color = TYPE))+
	# geom_jitter(aes(fill = TYPE), color = "black", pch = 21, 
						 alpha = 0.3, size = 3.5,
						 position = position_jitter(width = .05, height = 0.5))+
	# scale_fill_viridis_d(option = "D")+
	# geom_boxplot(alpha = 0, colour = "black")+
	# labs(fill = "Functional group")+
	# ggtitle("Seed response to carrion")+
	# facet_grid(~TYPE)+
	# xlab("Placement")+
	# theme_bw()+
	# theme(strip.background = element_blank(),
  # strip.text.x = element_blank())

# Create a version with mean and 95% CIs ####
# rename levels of factor
levels(survival$PACKET)[levels(survival$PACKET)=="AA"] <- "Rain adjacent"
levels(survival$PACKET)[levels(survival$PACKET)=="AT"] <- "Rain proximal"
levels(survival$PACKET)[levels(survival$PACKET)=="BA"] <- "Bank adjacent"
levels(survival$PACKET)[levels(survival$PACKET)=="BU"] <- "Bank proximal"

levels(survival$TYPE)[levels(survival$TYPE)=="ND"] <- "No dormancy"
levels(survival$TYPE)[levels(survival$TYPE)=="PD"] <- "Physiological dormancy"
levels(survival$TYPE)[levels(survival$TYPE)=="PY"] <- "Physical dormancy"

# reorder levels of factor
survival$PACKET <- factor(survival$PACKET, levels = c("Bank proximal",
										"Bank adjacent", "Rain proximal", "Rain adjacent"))

# make the label take up two lines
levels(survival$PACKET) <- gsub(" ", "\n", levels(survival$PACKET))

# First write a function that compute the min, mean-1SEM, mean, mean+1SEM, and Max. Then map these 5 values onto a boxplot using stat_summary.
MinMeanSEMMax <- function(x) {
  v <- c(min(x), mean(x) - sd(x)/sqrt(length(x)), mean(x), mean(x) + sd(x)/sqrt(length(x)), max(x))
  names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
  v
}


# Boxplot ####
bank <- ggplot(data = survival, aes(x = PACKET, y = diff, color = TYPE))+
	stat_summary(fun.data = MinMeanSEMMax, geom = "boxplot", color = "black")+ 
	geom_jitter(aes(fill = TYPE), color = "black", pch = 21, 
						 alpha = 0.3, size = 4.5,
						 position = position_jitter(width = .05, height = 0.5))+
	scale_fill_viridis_d(option = "D")+
	labs(fill = "Functional group")+
	ggtitle("Seed response to carrion")+
	facet_grid(~TYPE)+
	xlab("Seed location")+
	ylab("Δ Survival")+
	theme_bw()+
	theme(strip.background = element_blank(),
  strip.text.x = element_blank())+
	theme(legend.position = "bottom")+
	theme(
	title = element_text(size = 18),
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 18))+
	theme(axis.title.x = element_text(margin = margin(t = 20)))+
	theme(plot.title = element_text(hjust = 0.5, size = 18))+
	theme(legend.text=element_text(size=20))+
	theme(legend.title=element_text(size=20))
