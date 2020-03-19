# 18 March 2020 ####
library(lubridate)
library(devtools)
library(ggord)
library(vegan)
library(tidyverse)
library(ggplot2)
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
# Create March ordination ####
march_ord <- metaMDS(march_veg)

# Using the scores function from vegan to extract 
# the site scores and convert to a data.frame
march_data_scores <- as.data.frame(scores(march_ord))  

# create a column of site names, from the rownames of data.scores
march_data_scores_env <- cbind(march_env, march_data_scores)

# Using the scores function from vegan to extract 
# the species scores and convert to a data.frame
march_species_scores <- as.data.frame(scores(march_ord, "species"))  

# create a column of species, from the rownames of species.scores
march_species_scores$Species <- rownames(march_species_scores)  
# Use ggplot2 to visualize the March ordination ####
ggplot() + 
	geom_text_repel(data=march_data_scores_env,
						aes(x=NMDS1,y=NMDS2,label=Site))+
	geom_point(data=march_data_scores_env,
						 aes(x=NMDS1,y=NMDS2,shape=Exclusion,colour=Carrion),size=4)+
	coord_equal()+
	theme_classic()+
	scale_color_manual(values = c("High" = "Blue", "Low" = "Orange"))

			