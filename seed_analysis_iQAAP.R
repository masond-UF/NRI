# Models for iQAAP ####
library(nlme)
library(lme4)
library(sjPlot)
library(sjmisc)
library(MuMIn)
# Bring in the data ####
raw <- read.csv("PACKETS_SPRING_2020.csv") # load in data
d <-  raw %>% 
	filter(PLOT != "REF") # separate out the reference plots
ref <-  raw %>% 
	filter(PLOT == "REF") # keep just the reference plots
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
# Get the difference
survival$diff <- survival$SURV - as.numeric(survival$REF)
survival <- survival %>% 
	separate(TREATMENT, c("BIOMASS", "EXCL"), sep = "_")

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

# centering data

# Make lme models #####



0.02249/(0.02249 + 0.12050)  # ~16 %
0.1283658/(0.1283658 + 0.3439184) # 27
hist(residuals(m7))
boxplot(residuals(m7))
shapiro.test(m7$residuals) # residuals normally distributed
plot.lme(m7, resid(., type = "p") ~ fitted(.) | TYPE+PACKET, grid = TRUE, 
				 abline = c(0,0))

(prelim_plot <- ggplot(survival, aes(x = TYPE, y = diff)) +
  geom_point() +
  geom_smooth(method = "lm"))

# Make lmer models ####
mod.list <- list()


# intercept only model + random effects
mod.list[[1]] <- lmer(diff~1 + (1|SITE), data = survival, REML = FALSE)
summary(m1)
AIC(m1) # 275.6523
# all fixed effects + random effects
mod.list[[2]] <- lmer(diff ~ TYPE + BIOMASS + EXCL + PACKET + (1|SITE), 
					 data = survival, REML = FALSE)
summary(m2) 
AIC(m2) # 260.5358
# everything except biomass
mod.list[[3]] <- lmer(diff ~ TYPE + EXCL + PACKET + (1|SITE),
					 data = survival,  REML = FALSE)
summary(m3) 
AIC(m3) # 255.2524
# everything except exclusion
mod.list[[4]] <- lmer(diff ~ TYPE + BIOMASS + PACKET + (1|SITE),
					data = survival,  REML = FALSE)
summary(mod.list[[4]]) 
AIC(m4) # 249.0216
# everything except packet
mod.list[[5]] <- lmer(diff ~ TYPE + BIOMASS + EXCL + (1|SITE),
					data = survival,  REML = FALSE)
summary(m5) 
AIC(m5) # 286.8341
# everything except type
mod.list[[6]] <- lmer(diff ~ PACKET + BIOMASS + EXCL + (1|SITE),
					data = survival,  REML = FALSE)
summary(m6) 
AIC(m6) # 268.5415
# only type and packet
mod.list[[7]] <- lmer(diff~ TYPE + PACKET + (1|SITE), REML = FALSE,
				data = survival,  REML = FALSE)
summary(m7) 
AIC(m7) # 243.6317
# only type and exclusion 
mod.list[[8]] <- lmer(diff ~ TYPE + EXCL + (1|SITE),
			data = survival, REML = FALSE)
summary(m8)
AIC(m8) # 281.5373
# only type and biomass 
mod.list[[9]] <- lmer(diff ~ TYPE + BIOMASS + (1|SITE),
				 data = survival,  REML = FALSE)
summary(m9)
AIC(m9) # 275.4966
# only packet and exclusion 
mod.list[[10]] <- lmer(diff ~ PACKET + EXCL + (1|SITE),
				 data = survival,  REML = FALSE)
summary(m10)
AIC(m10) # 263.2489
# only packet and biomass 
mod.list[[11]] <- lmer(diff ~ PACKET + BIOMASS + (1|SITE),
				data = survival,  REML = FALSE)
summary(m11)
AIC(m11) # 257.1039
# only exclusion and biomass 
mod.list[[12]] <- lmer(diff ~ EXCL + BIOMASS + (1|SITE),
				data = survival,  REML = FALSE)
summary(m12)
AIC(m12) # 292.3029
# only type
mod.list[[13]] <- lmer(diff ~ TYPE + (1|SITE),
				data = survival,  REML = FALSE)
summary(m13)
AIC(m13) # 270.1064
# only packet 
mod.list[[14]] <- lmer(diff ~ PACKET + (1|SITE),
				 data = survival,  REML = FALSE)
summary(m14)
AIC(m14) # 251.7109
# only biomass 
mod.list[[15]] <- lmer(diff ~ BIOMASS + (1|SITE),
				data = survival,  REML = FALSE)
summary(m15)
AIC(m15) # 281.0367
# only exclusion
mod.list[[16]] <- lmer(diff ~ EXCL + (1|SITE),
			 data = survival,  REML = FALSE)
summary(m16)
AIC(m16) # 287.0074


m7 <- lmer(diff~ TYPE + PACKET + (1|SITE), REML=FALSE, 
				data = survival,  REML = FALSE)
leveneTest(y = golf$Birdies, group = golf$Wins)


aictab(cand.set = mod.list)


m7 <- lmer(diff ~ TYPE + EXCL + (1|SITE),data = survival, REML = FALSE)

dev.off()
plot.lme(mod.list[[7]], main = "Model 7", ylab = "Residuals", 
		 xlab = "Fitted values", ylim = c(-1,1.5), xlim = c(-0.6,0.4))	
plot.lme(mod.list[[4]], main = "Model 4", ylab = "Residuals",
		 xlab = "Fitted values", ylim = c(-1,1.5), xlim = c(-0.6,0.4))
hist(residuals(mod.list[[7]]), main = "Model 7",
		 xlab = "Residuals", xlim = c(-1.5,1.5), breaks = 15, ylim = c(0,80))
hist(residuals(mod.list[[4]]), main = "Model 4",
		  xlab = "Residuals", xlim = c(-1.5,1.5), breaks = 15, ylim = c(0,80))


summary(mod.list[[7]]) 
0.01648/(0.01648+0.11828) # variance attributable to SITE = 12%
summary(mod.list[[4]])
0.01628/(0.01628+0.11780) # variance attributable to SITE = 12%
coef(m7)

anova(mod.list[[4]], mod.list[[7]], refit=FALSE)
anova(mod.list[[1]], mod.list[[7]], refit=FALSE)
anova(mod.list[[1]], mod.list[[4]], refit=FALSE)

library("AICcmodavg")

qplot(PACKET, diff, facets = . ~ SITE, 
      colour = TYPE, geom = "boxplot", data = survival)

d_bycond = na.omit(survival) %>%
  group_by(SITE, TYPE, PACKET) %>%
  summarise(mean_diff = mean(diff))

ggplot(d_bycond, aes(x=PACKET, y=mean_diff, 
              colour=SITE, group=SITE)) +
    geom_line(size=2) + geom_point(size=5, shape=21, fill="white")+
		facet_wrap(~TYPE)

aictab(cand.set = mod.list)
out.put <- model.sel(mod.list) 
sub <- subset(out.put, delta <2) 
MA.ests < -model.avg(sub, revised.var = TRUE) 
write.csv(out.put, "model selection.csv")


MA.ests<-model.avg(out.put, subset= delta < 2, revised.var = TRUE) 
write.csv(as.data.frame(MA.ests), "model average")
