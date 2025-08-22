library(tidyverse)

# Unzip and load data
basic_df <- read_tsv(gzfile(
  "Improv/IMDB/title.basics.tsv.gz"
))

# The type of each entry. 
table(basic_df$titleType)

# We'll focus on the movies.
movie_df <- basic_df |>
  filter(titleType == "movie")

# We won't include adult films in this analysis.
table(movie_df$isAdult)
movie_df <- movie_df |>
  filter(!isAdult)

table(movie_df$isAdult) #yup, they're gone now

# Load in the AKA's dataset to see if we can identify which are originally
# English-language films. We failed, so I commented it out. 
# You can try if you want!

#akas_df <- read_tsv(gzfile(
#  "Improv/IMDB/title.akas.tsv.gz"
#))
#akas_df <- akas_df |>
#  filter(titleId %in% movie_df$tconst)

table(movie_df$genres)

# Convert Year to a numeric variable, and only include those released 
# through 2024.
movie_df <- movie_df |> 
  mutate(Year = as.numeric(startYear)) |>
  filter(!is.na(Year) & Year <= 2024)

# Count of movie releases over time
# Pipe your data table into ggplot for quick graphing!
movie_df |> group_by(Year) |>
  count() |>
  ggplot(aes(x = Year, y = n)) +  #'n' is the count for each year
  geom_line()

# Show the most recent years.
movie_df |> group_by(Year) |>
  count() |> arrange(desc(Year))

# How many films are listed as purely "Comedy".
sum(movie_df$genres == "Comedy")

# Include films that include "Comedy" in the genre listing
# Still include if they're also other genres.
# str_detect will detect all rows with "Comedy" in the genres listing,
#   and ignore_case=TRUE means that upper and lowercase both count.
movie_df <- movie_df |>
  mutate(isComedy = 
           str_detect(genres, 
                      fixed("Comedy", ignore_case = TRUE)))

# Area plot of comedy movies over time
movie_df |> filter(isComedy) |> group_by(Year) |>
  count() |>
  ggplot(aes(x = Year, y = n)) +
  geom_area() +
  ggtitle("Comedy movies over time") +
  ylab("Number of movies")

# Count of comedy *and other* movies over time
movie_df |> group_by(Year, isComedy) |>
  count() |>
  ggplot(aes(x = Year, y = n, fill = isComedy)) +
  geom_area() +
  ggtitle("Comedy movies over time") +
  ylab("Number of movies")


# Mean of logical expression shows what fraction are true
table(movie_df$isComedy)
mean(movie_df$isComedy)
mean(movie_df$Year > 1980)


# Fraction of comedy movies (out of total releases) each year
movie_df |> group_by(Year) |>
  summarize(fraction_comedy = mean(isComedy),
            count = n()) |>
  ggplot(aes(x = Year, y = fraction_comedy * 100)) +
  geom_area() +
  ggtitle("Comedy movies over time") +
  ylab("% Comedy")


# The big plot! 
movie_df |> group_by(Year) |>
  summarize(comedy = mean(isComedy)*100,
            other = mean(!isComedy)*100) |> # percentage of comedy/other films each year
  pivot_longer(-Year, names_to = "isComedy", 
               values_to = "percentage") |> # convert to friendly "long" format for ggplot
  mutate(isComedy = factor(isComedy, levels = c("other", "comedy"))) |>
  ggplot(aes(x = Year, y = percentage)) +
    geom_area(aes(fill = isComedy), alpha = 0.5, color = "black") +
    geom_text(data = data.frame(Year = c(1960, 1960),
                                percentage = c(10, 70),
                                Label = c("Comedy", "Other :(")), # Set coordinates and text for labels
              aes(label = Label)) +
  ggtitle("Proportion of comedy movies over time") +
  ylab("Percentage") +
  scale_fill_manual(values = c("grey", "purple")) +
  guides(fill = FALSE) +
  theme_bw()





### Bonus! ###
# Use the 'expand' option to add or remove space between the data & axes.
# In this case, I'll remove the space
# I also add a margin around the outside of the plot using 'plot.margin' in theme()
# And I changed text size.

movie_df |> group_by(Year) |>
  summarize(comedy = mean(isComedy)*100,
            other = mean(!isComedy)*100) |> # percentage of comedy/other films each year
  pivot_longer(-Year, names_to = "isComedy", 
               values_to = "percentage") |> # convert to friendly "long" format for ggplot
  mutate(isComedy = factor(isComedy, levels = c("other", "comedy"))) |>
  ggplot(aes(x = Year, y = percentage)) +
  geom_area(aes(fill = isComedy), alpha = 0.5, color = "black") +
  geom_text(data = data.frame(Year = c(1960, 1960),
                              percentage = c(10, 70),
                              Label = c("Comedy", "Not Funny :(")), # Set coordinates and text for labels
            aes(label = Label), size = 6) +
#  ggtitle("Proportion of comedy movies over time") +
  ylab("Percentage") +
  scale_fill_manual(values = c("grey", "purple")) +
  guides(fill = FALSE) +
  theme_bw() +
  theme(axis.title=element_text(size=20),
        axis.text=element_text(size=14),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1900, 2020, by=40)) +
  scale_y_continuous(expand = c(0,0))

ggsave("Comedy_AreaPlot.jpg", height = 4, width = 5)




