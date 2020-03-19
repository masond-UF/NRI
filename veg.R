# 18 March 2020 ####
library(lubridate)
# bring in all of the data
veg_all <- read.csv("OKMME_veg.csv") 
# convert the date using lubridate
veg_all$Date <- as_date(paste0(veg_all$Year,"-", 
															veg_all$Month,"-",
															veg_all$Day))
# subset the data by sampling period
march_veg <- veg_all[which(veg_all$Month==3),]
april_veg <- veg_all[which(veg_all$Month==4),]
may_veg <- veg_all[which(veg_all$Month==5),]
june_veg <- veg_all[which(veg_all$Month==6),]
july_veg <- veg_all[which(veg_all$Month==7),]
# Create ordinations ####