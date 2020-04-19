# Saving the images for the storyboard
ggsave(plot = diagram, "diagram.png", width = 15, height = 10, 
			 units = "in", dpi = 300)
ggsave(plot = bank, "seeds.png", width = 15, height = 10, 
			 units = "in", dpi = 300)
ggsave(plot = panel_c, "panel_c", width = 15, height = 10, 
			 units = "in", dpi = 300, device = "png")
ggsave(plot = ord_final, "ord_final", width = 15, height = 10, 
			 units = "in", dpi = 300, device = "png")
