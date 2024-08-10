library(tidyverse)
library(caret)

df <- read.csv("train.csv", na.strings = "")
df_test <- read.csv("test.csv", na.strings = "")
df_test <- df_test %>% mutate(Transported = NA)
df <- rbind(df, df_test)
summary(df)

# check NA
apply(df, 2, function(x) sum(is.na(x)))

df <- df %>%
  mutate(Group = unname(sapply(PassengerId, function(x) as.numeric(str_extract(x, "[0-9]{4}")))))

group <- df %>%
  group_by(Group) %>%
  summarize(N = n()) 

df <- df %>%
  merge(group, by = "Group")

df <- df %>%
   mutate(LastName = unname(sapply(Name, function(x) {
     names <- str_split(x, " ")[[1]]
     return (names[length(names)])
   })))
head(df)

df <- df %>%
   mutate(Deck = unname(sapply(Cabin, function(x) str_split(x,"/")[[1]][1])),
          Number = unname(sapply(Cabin, function(x) str_split(x,"/")[[1]][2])),
          Side = unname(sapply(Cabin, function(x) str_split(x,"/")[[1]][3])))
     
# categorical plot: HomePlanet, Destination, CryoSleep, VIP, Solo
ggplot(df) +
  geom_bar(aes(HomePlanet, fill = Transported), stat = "count", position = "dodge") +
  labs(title = "HomePlanet stats")

# VIP has minimal effect
ggplot(df %>% pivot_longer(cols = c("HomePlanet", "Destination", "CryoSleep", "VIP"),
                           names_to = "Variables", values_to = "Value")) +
  geom_bar(aes(x = Value, fill = Transported), stat = "count", position = "dodge") +
  facet_wrap(~ Variables, scales = "free_x") +
  labs(title = "Categorical Variable Stats", x = "Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Deck_T, Deck_A has minimal effect
ggplot(df) +
  geom_bar(aes(Deck, fill = Transported), stat = "count", position = "dodge") +
  theme_minimal()

ggplot(df) +
  geom_histogram(aes(as.numeric(Number), fill = Transported), color = "white") +
  theme_minimal()

ggplot(df) +
  geom_bar(aes(Side, fill = Transported), stat = "count", position = "dodge") +
  theme_minimal()

# Age ~ Normal  
ggplot(df) +
  geom_histogram(aes(Age, fill = Transported), color = "white") +
  theme_minimal()

# Service ~ Skewed left
ggplot(df %>% pivot_longer(cols = c("RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck"),
                           names_to = "Service", values_to = "Value")) +
  geom_boxplot(aes(Service, Value, color = VIP))

# install.packages("moments")
library(moments)
df %>% 
  select(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck) %>%
  summarize_all(~ skewness(., na.rm = TRUE)) %>%
  gather(variable, skewness)

# Overall, VIP spend more for Services
df %>%
  select(VIP, RoomService, FoodCourt, ShoppingMall, Spa, VRDeck) %>%
  group_by(VIP) %>%
  summarize_all( ~ mean(., na.rm = TRUE))


# a large portion are people within the same group and the same last name
# having the same HomePlanet
df %>%
  group_by(Group, LastName) %>%
  summarize(n_home = n_distinct(HomePlanet), 
            n_destination = n_distinct(Destination),
            n_deck = n_distinct(Deck),
            n_number = n_distinct(Number),
            n_side = n_distinct(Side)) %>%
  filter(n_home == n_destination & n_destination == n_deck & n_deck == n_number
         & n_number == n_side & n_side == 1)
df %>%
  group_by(Group, LastName) %>%
  summarize(n_deck = n_distinct(Deck),
            n_number = n_distinct(Number),
            n_side = n_distinct(Side)) %>%
  filter(n_deck == n_side & n_side == n_number)

str(df)
# install.packages("DescTools")
library(DescTools)
library(data.table)


imp_df <- df %>%
  group_by(LastName, Group) %>%
  fill(HomePlanet, Destination, Deck, Number, Side, VIP) %>%
  ungroup()

imp_df <- data.frame(imp_df)

imp_df <- imp_df %>%
  mutate_at(vars(HomePlanet, Destination, Deck, Number, Side), function(x) replace_na(x, Mode(x, na.rm = TRUE)))

apply(imp_df, 2, function(x) sum(is.na(x)))
str(imp_df)



imp_df <- imp_df %>%
  mutate_at(vars(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck), log1p) %>%
  mutate_at(vars(Number, Group), as.integer) %>%
  mutate_at(vars(colnames(df %>% select(where(is.character)))), as.factor) %>%
  select(-PassengerId, -Cabin, -Name, -LastName, -Transported)


library(mice)
apply(imp_df, 2, function(x) sum(is.na(x)))
blocks <- list(
  norm = c("Age", "RoomService", "FoodCourt", "ShoppingMall", "Spa", "VRDeck"),
  binary = c("CryoSleep", "VIP")
)
imp <- mice(imp_df, m = 1, seed = 1, blocks = blocks, method = c("norm", "lasso.logreg"))

imp_df <- complete(imp)
