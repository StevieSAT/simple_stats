library(dplyr)

## Load Data 
data <- read.csv("(path)", header = TRUE, stringAsFactor = FALSE)
head(data)
colnames(data)

### Changing data types for specific columns 
## 1st: change them individually
d_nu <- data
d_nu$Asian.policies.or.issues <- as.numeric(d_nu$Asian.policies.or.issues)
d_nu$Asian.voters.or.people <- as.numeric(d_nu$Asian.voters.or.people)
d_nu$Asian.culture <- as.numeric(d_nu$Asian.culture)
d_nu$Asian.origins <- as.numeric(d_nu$Asian.origins)

# 2nd: change them with the mutate function with a pacakge of codes. 
d2_nu <- data %>%
  mutate(
    Asian.policies.or.issues = as.numeric(Asian.policies.or.issues),
    Asian.voters.or.people = as.numeric(Asian.voters.or.people),
    Asian.culture = as.numeric(Asian.culture),
    Asian.origins = as.numeric(Asian.origins)
  )

## Summarize the Dummies 
counts <- d2_nu %>%
  na.omit() %>%
  summarize(
    n_policies = sum(Asian.policies.or.issues == 1),
    n_voters = sum(Asian.voters.or.people == 1),
    n_culture = sum(Asian.culture == 1),
    n_origins = sum(Asian.origins == 1)
  )

counts

## Plotting the Barplot 
library(ggplot2)
library(dplyr)
library(tidyr)
counts <- d2_nu %>%
  na.omit() %>%
  summarize(
    n_policies = sum(Asian.policies.or.issues == 1),
    n_voters = sum(Asian.voters.or.people == 1),
    n_culture = sum(Asian.culture == 1),
    n_origins = sum(Asian.origins == 1)) %>%
  pivot_longer(everything(), names_to = "category", values_to = "count")

# display the counts
counts
# create a barplot
ggplot(counts, aes(x = category, y = count, fill = category)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  coord_flip() +
  labs(x = "", y = "Count", fill = "") +
  ggtitle("Counts by category")

## Association of these four categories 
cor <- d2_nu %>%
  na.omit() %>%
  select(Asian.policies.or.issues,
         Asian.voters.or.people,
         Asian.culture,
         Asian.origins) %>% 
  cor()

cor

# Plot the association 
install.packages("ggcorrplot")
library(ggcorrplot)
cor_ma <- as.data.frame(cor)
cor_ma
ggcorrplot(cor_ma, type = "upper", hc.order = TRUE, lab = TRUE)

