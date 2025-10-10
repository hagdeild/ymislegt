# Installing fonts

# library(extrafont)
# font_import(paths = "C:/Windows/Fonts", prompt = FALSE)
# loadfonts(device = "win")
# 


# install.packages("showtext")
# install.packages("systemfonts")
# 
# library(showtext)
# library(systemfonts)
# 
# font_add("Myriad Pro", "C:/Users/vidar/Downloads/myriad-pro/MYRIADPRO-REGULAR.OTF")
# 
# showtext_auto()


library(ggplot2)
library(showtext)

showtext_auto()

font_add("Myriad Pro", "C:/Users/vidar/Documents/Rwd/fonts/MYRIADPRO-REGULAR.OTF")

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  labs(title = "Plot") +
  theme(plot.title = element_text(family = "Myriad Pro"))
