# 18 March 2020 ####
library(lubridate)
library(devtools)
library(ggord)
library(vegan)
library(tidyverse)
# bring in all of the data
veg_all <- read.csv("OKMME_veg.csv") 
# convert the date using lubridate
veg_all$Date <- as_date(paste0(veg_all$Year,"-", 
															veg_all$Month,"-",
															veg_all$Day))
# subset the data by sampling period
march_all <- veg_all[which(veg_all$Month==3),]
april_all <- veg_all[which(veg_all$Month==4),]
may_all <- veg_all[which(veg_all$Month==5),]
june_all <- veg_all[which(veg_all$Month==6),]
july_all <- veg_all[which(veg_all$Month==7),]
# subset the data to get just vegetation
march_veg <- march_all[,9:97]
march_env <- march_all[,1:8]
april_veg <- april_all[,9:97]
may_veg <- may_all[,9:97]
june_veg <- june_all[,9:97]
july_veg <- july_all[,9:97]
# Create ordinations ####
march_ord <- metaMDS(march_veg)
# Using the scores function from vegan to extract 
# the site scores and convert to a data.frame
march_data_scores <- as.data.frame(scores(march_ord))  
# create a column of site names, from the rownames of data.scores
march_data_scores_env <- cbind(march_env, march_data_scores)



ggord(march_ord, grp_in = c(march_all$Carrion, march_all$Exclusion), ellipse = FALSE, arrow = NULL,
			alpha = 0.8, var_sub = FALSE)



april_ord <- metaMDS(april_veg)
ggord(april_ord, grp_in = april_all$Treatment, ellipse = FALSE, arrow = NULL,
			alpha = 0.8, var_sub = FALSE)

			