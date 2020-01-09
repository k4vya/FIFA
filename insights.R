library("dplyr")
library("ggplot2")

df <- read.csv("fifa_data.csv")

avg_age <- df %>%
  select("Age") %>%
  summarize(Age = mean(Age, na.rm = TRUE)) %>%
  pull(Age)

avg_age <- round(avg_age, 1)
  
best_gk <- df %>%
  group_by(Position) %>%
  filter(Position == "GK") %>%
  filter(Overall == max(Overall, na.rm = TRUE)) %>%
  select(Name, Overall)

worst_gk <- df %>%
  group_by(Position) %>%
  filter(Position == "GK") %>%
  filter(Overall == min(Overall, na.rm = TRUE)) %>%
  select(Name, Overall)

best_player <- df %>%
  select(Name, Nationality, Overall, Club) %>%
  filter(Overall == max(Overall, na.rm = TRUE)) %>%
  select(Name, Nationality, Overall, Club)

worst_player <- df %>%
  select(Name, Nationality, Overall, Club) %>%
  filter(Overall == min(Overall, na.rm = TRUE)) %>%
  select(Name, Nationality, Overall, Club)

country_most_players <- df %>%
  group_by(Nationality) %>%
  tally() %>%
  filter(n == max(n)) %>%
  select(Nationality, n)

country_least_players <- df %>%
  group_by(Nationality) %>%
  tally() %>%
  filter(n == min(n)) %>%
  select(Nationality, n)


player_most_potential <- df %>%
  select(Name, Nationality, Overall, Potential) %>%
  mutate(Most_Pot = (Potential-Overall)) %>%
  filter(Most_Pot == max(Most_Pot, na.rm = TRUE)) %>%
  select(Name, Nationality, Overall, Potential, Most_Pot)

sounders_avg <- df %>%
  filter(Club == "Seattle Sounders FC") %>%
  summarize(Overall = mean(Overall, na.rm = TRUE))

rmcf_avg <- df %>%
  filter(Club == "Real Madrid") %>%
  summarise(Overall = mean(Overall, na.rm = TRUE))

#what % of players are left footed?

perc_left_foot <- df %>%
  group_by(Foot) %>%
  filter(Foot == "Left") %>%
  tally() %>%
  mutate(n = as.numeric(n)) 

perc_left_foot <- 100*round((4211/18207), 2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#MAKE SOME GRAPHS!

#Is age correlated with overall score?
#Is age correlated with speed?

age_score_scat <- ggplot(data = df) +
  geom_point(mapping = aes(
    x = Age,
    y = Overall,
  ),
  size = 0.5, 
  color = "blue") + 
  geom_smooth(
    mapping = aes(
      x = Age,
      y = Overall
    ),
    color = "black"
  )


age_speed_scat <- ggplot(data = df) +
  geom_point(mapping = aes(
    x = Age,
    y = SprintSpeed,
  ),
  size = 0.5, 
  color = "red") + 
  geom_smooth(
    mapping = aes(
      x = Age,
      y = SprintSpeed
    ),
    color = "black"
  )

# Does having more players make your country better at soccer?
# Bar chart!
# Get top 10 countries with most players
# group by country
# tally num players
# get top 10
# make df w 10 countries and num players
# make bar chart

top_10 <- df %>%
  select(Nationality) %>%
  group_by(Nationality) %>%
  tally() %>%
  arrange(-n) %>%
  top_n(10)

#make bar chart
ten_bar <- ggplot(data = top_10) +
  geom_col(mapping = aes(
            x = Nationality,
            y = n
  ))
  
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#hello <- map_data("world")

#ggplot(hello) +
 
# geom_polygon(
 #   mapping = aes(x = long, y = lat, group = group),
  
# color = "white", # show state outlines
#size = .01        # thinly stroked
#  ) +
 # coord_map() # use a map-based coordinate system