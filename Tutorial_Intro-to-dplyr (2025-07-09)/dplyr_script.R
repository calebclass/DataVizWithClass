library(tidyverse)

# Load the data
# You can find and and load it directly from your "Files" window
Video_Games <- 
  read_csv("Tutorial/Tutorial2/Video_Games.csv")

# select chooses columns
select(Video_Games, Name, NA_Sales)
select(Video_Games, Name:NA_Sales)
select(Video_Games, -Name)

# The pipe!
vg_selected <- 
  Video_Games |> select(Name, NA_Sales)
# is the same as ...
vg_selected <- 
   select(Video_Games, Name, NA_Sales)

# Remove (omit) any rows with missing (NA) data
vg_filtered <- 
  Video_Games |> na.omit()

# A logical expression
# Main logical operators in R: > >= < <= == !=
8 > 3
8 < 3

# Retain games with Critic Score greater or equal to 90
vg_highscores <- Video_Games |>
  filter(Critic_Score >= 90)

# Retain games from PlayStation
vg_playstation <- Video_Games |>
  filter(Platform == "PS")

# Retain games with BOTH conditions (&)
vg_both <- Video_Games |>
  filter(Critic_Score >= 90 & Platform == "PS")

# Retain games that DO NOT (!) have a missing (NA) Critic_Score
vg_filtered2 <- Video_Games |>
  filter(!is.na(Critic_Score))

# Group by & summarize!
# This one counts how many from each platform.
vg_filtered2 |> group_by(Platform) |>
  count()

# This does summary calculations within each platform.
# n() allows you to count within the summarize function.
vg_filtered2 |> group_by(Platform) |>
  summarize(Total_Sales = sum(Global_Sales),
            Releases = n(),
            Critic_Average = mean(Critic_Score))

# Without grouping, it's pretty boring.
vg_filtered2 |> 
  summarize(Total_Sales = sum(Global_Sales),
            Releases = n(),
            Critic_Average = mean(Critic_Score))

# Use arrange to sort. desc makes it descending (high-to-low)
# Then pipe it straight into a graph! 
# (ggplot does not need a 'data' argument in this case -- it's piped in)
vg_filtered2 |> group_by(Platform) |>
  summarize(Total_Sales = sum(Global_Sales),
            Releases = n(),
            Critic_Average = mean(Critic_Score)) |>
  arrange(desc(Total_Sales)) |>
  ggplot(aes(x = reorder(Platform, desc(Total_Sales)), y = Total_Sales)) +
    geom_col()

# The ggrepel library makes your labels 'repel' each other, so they 
# don't overlap.
vg_filtered2 |> group_by(Platform) |>
  summarize(Total_Sales = sum(Global_Sales),
            Releases = n(),
            Critic_Average = mean(Critic_Score)) |>
  arrange(desc(Total_Sales)) |>
  ggplot(aes(x=Critic_Average, y=Total_Sales, label=Platform)) +
    geom_point() +
    ggrepel::geom_label_repel()

# Mutate adds columns to your data table
vg_mutated <- vg_filtered2 |> mutate(NAvJP_Sales = NA_Sales - JP_Sales) |>
  select(Name, NA_Sales, JP_Sales, NAvJP_Sales) |>
  arrange(NAvJP_Sales)

# Grouped mutate!
vg_mutated2 <- vg_filtered2 |> group_by(Platform) |>
  mutate(Critic_vs_Avg = Critic_Score - mean(Critic_Score),
         PlatformMean = mean(Critic_Score))

vg_mutated2 |> 
  arrange(desc(Critic_vs_Avg)) |>
  select(Name, Platform, Critic_Score, Critic_vs_Avg, PlatformMean)






