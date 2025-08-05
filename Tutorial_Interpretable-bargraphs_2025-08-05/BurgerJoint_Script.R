library(tidyverse)

dat <- read_csv("Tutorial/Tutorial4-Bargraph_fastfood/BurgerJoint_data.csv")
dat$Restaurant <- factor(dat$Restaurant, levels = unique(dat$Restaurant))

# Bar graph comparing within item types
ggplot(dat, aes(x = ItemType, y = Calories, fill = Restaurant)) +
  geom_col(position = "dodge", color = "black", width = 0.7) +
  scale_fill_manual(values = c("#FFF10A", "#FF8733", "#DD1438")) +
  ggthemes::theme_fivethirtyeight() +
  ggtitle("Counting Calories")
ggsave("Tutorial/Tutorial4-Bargraph_fastfood/BurgerJoint_Graph_v1.jpg", device = "jpeg",
       width = 5, height = 4, dpi = 320)

# Swap grouping
ggplot(dat, aes(fill = ItemType, y = Calories, x = Restaurant)) +
  geom_col(position = "dodge", color = "black", width = 0.7) +
  scale_fill_manual(values = c("brown", "yellow", "green")) +
  ggthemes::theme_fivethirtyeight() +
  ggtitle("Counting Calories")
ggsave("Tutorial/Tutorial4-Bargraph_fastfood/BurgerJoint_Graph_v2.jpg", device = "jpeg",
       width = 5, height = 4, dpi = 320)

# stack
ggplot(dat, aes(fill = ItemType, y = Calories, x = Restaurant)) +
  geom_col(position = "stack", color = "black", width = 0.7) +
  scale_fill_manual(values = c("brown", "yellow", "green")) +
  ggthemes::theme_fivethirtyeight() +
  ggtitle("Counting Calories")
ggsave("Tutorial/Tutorial4-Bargraph_fastfood/BurgerJoint_Graph_v3.jpg", device = "jpeg",
       width = 5, height = 4, dpi = 320)

# Default colors
ggplot(dat, aes(x = ItemType, y = Calories, fill = Restaurant)) +
  geom_col(position = "dodge", color = "black", width = 0.7) +
  ggtitle("Counting Calories")

# Shadow effect
# https://stackoverflow.com/questions/28997809/add-shadow-effect-ggplot2-bars-barplot
ggplot(dat) +
  geom_col(position = "dodge", fill = "grey70", aes(x = as.integer(as.factor(ItemType))+0.1, y = Calories-10, group = Restaurant), width = 0.7) +
  geom_col(position = "dodge", color = "black", aes(x = as.integer(as.factor(ItemType)), y = Calories, fill = Restaurant), width = 0.7) +
  scale_x_continuous(breaks = 1:3,labels = unique(dat$ItemType)) +
  ylab("Calories") +
  ggtitle("Counting Calories")

# 3D !!!! Variables need to be factors
# https://stackoverflow.com/questions/26794236/ggplot2-3d-bar-plot
library(latticeExtra)

dat$ItemType <- factor(dat$ItemType, levels = c("Fries", "Salad", "Burger"))
cloud(Calories ~ Restaurant + ItemType, dat, 
      panel.3d.cloud=panel.3dbars, col.facet='grey', 
      xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1), 
      par.settings = list(axis.line = list(col = "transparent")))

