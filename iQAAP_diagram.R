# 20 April 2020—Conceptual Diagram for iQAAP
# Data and packages ####
library(ggplot2)
library(bezier)  # package gives functions to draw custom curves
library(grid)    # packages helps with arranging Grobs
library(jpeg)

decomp <- rasterGrob(readJPEG('decomp.jpg'))
PY <- rasterGrob(readJPEG('PY.jpg'))
PD <- rasterGrob(readJPEG('PD.jpg'))
ND <- rasterGrob(readJPEG('ND.jpg'))
base <- rasterGrob(readJPEG('base.jpg'))
plant1 <- rasterGrob(readJPEG('plant1.jpg'))
plant2 <- rasterGrob(readJPEG('plant2.jpg'))
plant3 <- rasterGrob(readJPEG('plant3.jpg'))
fact1 <- rasterGrob(readJPEG('factorial1.jpg'))
fact2 <- rasterGrob(readJPEG('factorial2.jpg'))
NRI <- rasterGrob(readJPEG('NRI2.jpg'))
# Data viz ####
diagram <- ggplot(data.frame(a=1)) + xlim(1, 31) + ylim(1, 32)+ 
	annotation_custom(decomp, xmin = 1.25, xmax = 6.25, ymin = 22, ymax = 28)+
	geom_label(label ="Decomposition", x = 2.5, y = 30, color = "red", size = 6)+
	geom_path(data = as.data.frame(bezier(t = 0:100/100, 
																				p = list(x = c(6.5, 7.5, 7.5),
																								 y = c(22.5, 22, 19)))),
						aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"),
																												 type = "closed"))+
	annotation_custom(fact1, xmin = 12, xmax = 19, ymin = 21, ymax = 28)+
	#annotate('text', x = 10, y = 30, label = 'Carrion design', fontface = 'bold')+
	annotate("text", x = 13.7, y = 29, label = "Low", size = 5)+
	annotate("text", x = 10.3, y = 29, label = "Exclusion", size = 5)+
	annotate("text", x = 17.2, y = 29, label = "High", size = 5)+
	annotate("text", x = 15.5, y = 30, label = "Biomass", size = 5)+
	annotate("text", x = 10.3, y = 27, label = "Herbivore", size = 5)+
	annotate("text", x = 10.2, y = 24.5, label = "Open", size = 5)+
	annotate("text", x = 10.2, y = 22, label = "Scavenger", size = 5)+
	annotation_custom(fact2, xmin = 20, xmax = 27, ymin = 22.5, ymax = 26.5)+
	#annotate('text', x = 26, y = 30, label = 'Seed design', fontface = 'bold')+
	geom_path(data = as.data.frame(bezier(t = 0:100/100, 
																				p = list(x = c(8, 9, 9, 9),
																								 y = c(13, 11, 10, 8)))),
						aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"),
																												 type = "closed"))+
	annotate("text", x = 28, y = 25.8, label = "Proximal", size = 5)+
	annotate("text", x = 28, y = 23, label = "Adjacent", size = 5)+
	annotate("text", x = 22, y = 27.4, label = "Bank", size = 5)+
	annotate("text", x = 25.2, y = 27.4, label = "Rain", size = 5)+
	annotate("text", x = 21.13, y = 25.544, label = "ND", size = 3)+
	annotate("text", x = 4, y = 13, label = "ND", size = 5)+
	annotate("text", x = 4, y = 2.5, label = "ND", size = 5)+
	annotate("text", x = 22, y = 25.544, label = "PD", size = 3)+
	annotate("text", x = 7, y = 13, label = "PD", size = 5)+
	annotate("text", x = 7.5, y = 1.5, label = "PD", size = 5)+
	annotate("text", x = 22.86, y = 25.544, label = "PY", size = 3)+
	annotate("text", x = 10, y = 13, label = "PY", size = 5)+
	annotate("text", x = 11, y = 1, label = "PY", size = 5)+
	annotate("text", x = 1, y = 21, label = "Figure 4", size = 5)+
	annotate("text", x = 1, y = 12, label = "Figure 6", size = 5)+
	annotate("text", x = 23, y = 18.6, label = "Figure 7", size = 5)+
	annotation_custom(PY, xmin = 3, xmax = 5, ymin = 13, ymax = 18)+
	annotation_custom(PD, xmin = 6, xmax = 8, ymin = 13, ymax = 18)+
	annotation_custom(ND, xmin = 9, xmax = 11, ymin = 13, ymax = 18)+
	geom_label(label ="Seed bank/seed rain", x = 3.5, y = 19, color = "Blue", size = 6)+
	geom_rect(aes(xmin = 8.7, xmax = 30, ymin = 20, ymax = 31),# seed bank/seed rain
		 fill = "transparent", color = "black", size = 0.5, linejoin = "round")+
	geom_rect(aes(xmin = 20.3, xmax = 26.6, ymin = 22.4, ymax = 26.6),# seed design
						fill = "transparent", color = "blue", size = 0.5, linejoin = "round")+
	geom_rect(aes(xmin = 15.6, xmax = 18.9, ymin = 26, ymax = 28),# mini blue top right
						fill = "transparent", color = "blue", size = 0.5, linejoin = "round")+
	geom_rect(aes(xmin = 12.1, xmax = 15.3, ymin = 26, ymax = 28),# mini blue top left
						fill = "transparent", color = "blue", size = 0.5, linejoin = "round")+
	geom_rect(aes(xmin = 15.6, xmax = 18.9, ymin = 23.5, ymax = 25.5),# mini blue middle right
						fill = "transparent", color = "blue", size = 0.5, linejoin = "round")+
	geom_rect(aes(xmin = 12.1, xmax = 15.3, ymin = 23.5, ymax = 25.5),# mini blue middle left
						fill = "transparent", color = "blue", size = 0.5, linejoin = "round")+
	geom_rect(aes(xmin = 15.6, xmax = 18.9, ymin = 21, ymax = 23),# mini blue bottom right
						fill = "transparent", color = "blue", size = 0.5, linejoin = "round")+
	geom_rect(aes(xmin = 12.1, xmax = 15.3, ymin = 21, ymax = 23),# mini blue bottom left
						fill = "transparent", color = "blue", size = 0.5, linejoin = "round")+
	geom_rect(aes(xmin = 12, xmax = 19, ymin = 20.9, ymax = 28.1),# carrion design
						fill = "transparent", color = "red", size = 0.8, linejoin = "round")+
		#annotation_custom(base, xmin = 4, xmax = 8, ymin = 3, ymax = 6)+
	annotation_custom(plant1, xmin = 2.5, xmax = 5.5, ymin = 3, ymax = 9)+
	annotation_custom(plant2, xmin = 6, xmax = 9, ymin = 2, ymax = 8)+
	annotation_custom(plant3, xmin = 9.5, xmax = 12.5, ymin = 2, ymax = 8)+
	geom_label(label ="Vegetation response", x = 3.5, y = 10, color = "red", size = 6)+
	annotation_custom(NRI, xmin = 16, xmax = 30, ymin = 0, ymax = 19)+
	geom_label(label ="Landscape heterogeniety", x = 26, y = 17, size = 6)+
	geom_path(data = as.data.frame(bezier(t = 0:100/100, 
																				p = list(x = c(13, 13.5, 14.5, 15.5),
																								 y = c(3, 2.5, 2, 3)))),
						aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"),
																												 type = "closed"))+
	annotate("text", x = 19, y = 32, label = "Experimental design", size = 5)+
	geom_segment(x = 18.87, xend = 20.3, y = 24.5, yend = 25.5, color = "blue")+
	geom_segment(x = 18.87, xend = 20.3, y = 24.5, yend = 23.5, color = "blue")+
	geom_segment(x = 19, xend = 12, y = 14.2, yend = 21, color = "red")+
	geom_segment(x = 19.3, xend = 19, y = 14.2, yend = 21, color = "red")+
	theme_void()

ggsave("iQAAP_diagram.png")

