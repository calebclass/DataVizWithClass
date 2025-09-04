# Data from the United States Social Security Administration, downloaded
# from Kaggle (Ryan Burnsworth): 
# https://www.kaggle.com/datasets/ryanburnsworth/popular-names-by-birth-year-1880-2022

library(tidyverse)

getwd()
names_df <- read_csv("Tutorial/Tutorial3-Heatmaps/data/names_by_birth_year.csv")

head(names_df)

# Popularity of "Caleb" over time
names_df |> filter(Name == "Caleb" & Gender == "M") |>
  ggplot(aes(x = Year, y = Count)) +
  geom_line()

# Rank names each year
names_ranked <- names_df |> group_by(Year, Gender) |>
  mutate(Rank = rank(desc(Count)))
View(names_ranked)

# Top 10 from 2022
top10_2022 <- names_ranked |> filter(Year == 2022 & Rank <= 10) |>
  mutate(Name_Gender = str_c(Name, Gender, sep = "_"))
View(top10_2022)

# Filter original data table for top 10 boy & girl names.
# Our filter function says we only want rows from names_df if the Name_Gender
# is %in% the Name_Gender column of the 2022 Top 10 data frame.

names_top10 <- names_df |> 
  filter(str_c(Name, Gender, sep = "_") %in% top10_2022$Name_Gender) 


### 2 bonus lines, to improve theming
names_top10 |>
  ggplot(aes(x = Year, y = Name, fill = log10(Count))) +
  geom_tile() +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "red",
                       midpoint = 3) +
  theme_bw() +
  ylab("") + # remove y-axis title
  scale_x_continuous(expand = c(0,0))  # removes the padding

# tidyr to convert "long" to "wide" data frame
names_matrix <- names_top10 |> select(-Gender) |>
  pivot_wider(names_from = Name, values_from = Count,
              values_fill = 5) |> 
  arrange(Year)
View(names_matrix)

# If you want to go from a wide to long data frame
names_matrix |>
  pivot_longer(-Year, names_to = "Name", values_to = "Babies") |> 
  view()

# Make the heatmap using the wide one!
# We use t() to transpose (swap the rows & columns)
library(pheatmap)

pheatmap(t(select(names_matrix, -Year)),
         labels_col = names_matrix$Year)

pheatmap(log10(t(select(names_matrix, -Year))),
         cluster_cols = FALSE)

# We'll use this to make the colorbar for males/females
annotation <- data.frame(Gender = top10_2022$Gender,
                         row.names = top10_2022$Name)

# Add a colorbar!
pheatmap(log10(t(select(names_matrix, -Year))),
         cluster_cols = FALSE,
         annotation_row = annotation)



################### Bonus! ########################
# Change color scale
# I think the default color scale looks pretty good, but there are
# plenty of other options from RColorBrewer
# See the help file for details
?pheatmap

# Below, I change the color palette to Red-White-Blue instead of Red-Yellow-Blue
# You could remove the rev() function to reverse the scale (blue high, red low)
# Or try using a different palette: https://r-graph-gallery.com/38-rcolorbrewers-palettes.html 
pheatmap(log10(t(select(names_matrix, -Year))),
         cluster_cols = FALSE,
         annotation_row = annotation,
         color = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdBu")))(100))

# If that heatmap didn't work, try installing RColorBrewer
#install.packages("RColorBrewer")


### Bonus plot -- a bad one! ###
# 20 different colored lines on same graph

names_top10 |>
  ggplot(aes(x = Year, y = Count, color = Name)) +
  geom_line(size = 0.8) +
  theme_bw() +
  xlim(1980, 2022) +
  ylim(0,40000)

