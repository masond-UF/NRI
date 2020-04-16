# 18 March 2020 ####
library(lubridate)
library(devtools)
library(ggord)
library(vegan)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(cowplot)
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
march_veg <- march_all[,9:97] # take just average cover
march_veg <- march_veg[, colSums(march_veg != 0) > 0] # get rid of empty columns
march_env <- march_all[,1:8] # take just environmental variables
april_veg <- april_all[,9:97]
april_veg <- april_veg[, colSums(april_veg != 0) > 0]
april_env <- april_all[,1:8]
may_veg <- may_all[,9:97]
may_veg <- may_veg[, colSums(may_veg != 0) > 0]
may_env <- may_all[,1:8]
june_veg <- june_all[,9:97]
june_veg <- june_veg[, colSums(june_veg != 0) > 0]
june_env <- june_all[,1:8]
july_veg <- july_all[,9:97]
july_veg <- july_veg[, colSums(july_veg != 0) > 0]
july_env <- july_all[,1:8]
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
	ggrepel::geom_text_repel(data=march_data_scores_env,
						aes(x=NMDS1,y=NMDS2,label=Site))+
	geom_point(data=march_data_scores_env,
						 aes(x=NMDS1,y=NMDS2,shape=Exclusion,colour=Carrion),size=3)+
	coord_equal()+
	theme_classic()+
	scale_color_manual(values = c("High" = "Blue", "Low" = "Orange"))
# Create April ordination ####
april_ord <- metaMDS(april_veg)

# Using the scores function from vegan to extract 
# the site scores and convert to a data.frame
april_data_scores <- as.data.frame(scores(april_ord))  

# create a column of site names, from the rownames of data.scores
april_data_scores_env <- cbind(april_env, april_data_scores)

# Using the scores function from vegan to extract 
# the species scores and convert to a data.frame
april_species_scores <- as.data.frame(scores(april_ord, "species"))  

# create a column of species, from the rownames of species.scores
april_species_scores$Species <- rownames(april_species_scores)  
# Use ggplot2 to visualize the April ordination ####
april <- ggplot() + 
	ggrepel::geom_text_repel(data=april_data_scores_env,
													 aes(x=NMDS1,y=NMDS2,label=Site))+
	geom_point(data=april_data_scores_env,
						 aes(x=NMDS1,y=NMDS2,shape=Exclusion,colour=Carrion),size=3)+
	coord_equal(xlim = c(-1, 1), ylim = c(-1,1))+
	ggtitle("April")+
	theme_classic()+
	scale_color_viridis_d(option = "C", alpha = 0.7)
# Create May ordination ####
may_ord <- metaMDS(may_veg)

# Using the scores function from vegan to extract 
# the site scores and convert to a data.frame
may_data_scores <- as.data.frame(scores(may_ord))  

# create a column of site names, from the rownames of data.scores
may_data_scores_env <- cbind(may_env, may_data_scores)

# Using the scores function from vegan to extract 
# the species scores and convert to a data.frame
may_species_scores <- as.data.frame(scores(may_ord, "species"))  

# create a column of species, from the rownames of species.scores
may_species_scores$Species <- rownames(may_species_scores)  

# Use ggplot2 to visualize the May ordination ####
may <- ggplot() + 
	ggrepel::geom_text_repel(data=may_data_scores_env,
													 aes(x=NMDS1,y=NMDS2,label=Site))+
	geom_point(data=may_data_scores_env,
						 aes(x=NMDS1,y=NMDS2,shape=Exclusion,colour=Carrion),size=3)+
	coord_equal(xlim = c(-1,1), ylim = c(-1,1))+
	ggtitle("May")+
	theme_classic()+
		scale_color_viridis_d(option = "C", alpha = 0.7)
# Create June ordination ####
june_ord <- metaMDS(june_veg)

# Using the scores function from vegan to extract 
# the site scores and convert to a data.frame
june_data_scores <- as.data.frame(scores(june_ord))  

# create a column of site names, from the rownames of data.scores
june_data_scores_env <- cbind(june_env, june_data_scores)

# Using the scores function from vegan to extract 
# the species scores and convert to a data.frame
june_species_scores <- as.data.frame(scores(june_ord, "species"))  

# create a column of species, from the rownames of species.scores
june_species_scores$Species <- rownames(june_species_scores)  





# Use ggplot2 to visualize the June ordination ####
june <- ggplot() + 
	ggrepel::geom_text_repel(data=june_data_scores_env,
													 aes(x=NMDS1,y=NMDS2,label=Site))+
	geom_point(data=june_data_scores_env,
						 aes(x=NMDS1,y=NMDS2,shape=Exclusion,colour=Carrion),size=3)+
	coord_equal(xlim = c(-1,1), ylim = c(-1,1))+
	ggtitle("June")+
	theme_classic()+
	scale_color_viridis_d(option = "C", alpha = 0.7)

# Create July ordination ####
july_ord <- metaMDS(july_veg)

# Using the scores function from vegan to extract 
# the site scores and convert to a data.frame
july_data_scores <- as.data.frame(scores(july_ord))  

# create a column of site names, from the rownames of data.scores
july_data_scores_env <- cbind(july_env, july_data_scores)

# Using the scores function from vegan to extract 
# the species scores and convert to a data.frame
july_species_scores <- as.data.frame(scores(july_ord, "species"))  

# create a column of species, from the rownames of species.scores
july_species_scores$Species <- rownames(july_species_scores)  
# Use ggplot2 to visualize the July ordination ####
july <- ggplot() + 
	ggrepel::geom_text_repel(data=july_data_scores_env,
													 aes(x=NMDS1,y=NMDS2,label=Site))+
	geom_point(data=july_data_scores_env,
						 aes(x=NMDS1,y=NMDS2,shape=Exclusion,colour=Carrion),size=3)+
	coord_equal(xlim = c(-1, 1), ylim = c(-1,1))+
	ggtitle("July")+
	theme_classic()+
	scale_color_viridis_d(option = "C", alpha = 0.7)

legend_b <- get_legend(
  april + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom"))

prow <- plot_grid(
  april + theme(legend.position="none"),
  may + theme(legend.position="none"),
  june + theme(legend.position="none"),
  july + theme(legend.position="none"),
  align = 'vh',
  hjust = -1,
  nrow = 2,
  labels = c('D.1', 'D.2', 'D.3', 'D.4'),
											 label_colour = "red")

prow

ord_final <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1))
