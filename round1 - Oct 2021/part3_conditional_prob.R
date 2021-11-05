# libraries I'm using
library(dplyr)
library(ggplot2)

# find conditional probabilities - p(x|y)
hamptonese <- readRDS("hamptonese.Rds")

# how often does each character appear?
# using methodB to be conservative
count_a <- hamptonese %>%
  mutate(thisChar=methodB) %>%
  group_by(thisChar) %>%
  summarise(total_count=n())

# how often does each character follow the other?
# treat each "revelation" as a break (new line)
count_a_b_pairs <- hamptonese %>%
  mutate(thisChar=methodB,
         nextChar=lead(methodB),
         nextRev=lead(revelation)) %>%
  filter(revelation==nextRev) %>%
  group_by(thisChar,nextChar) %>%
  summarise(pair_count=n())

# conditional probability
prob_b_given_a <- count_a %>%
  left_join(count_a_b_pairs) %>%
  mutate(A=thisChar,
         B=nextChar,
         `p(B|A)`=pair_count/total_count) 


# take a look
ggplot(prob_b_given_a, aes(x = A, y = B)) + 
  geom_raster(aes(fill=`p(B|A)`)) + 
  scale_fill_gradient(low="grey90", high="red") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# is the sum of conditional prob just going to mirror most frequent characters?
# more or less- not very insightful
check <- prob_b_given_a %>%
  group_by(B) %>%
  summarise(total=sum(`p(B|A)`))


# which character is most likely to follow each character?
check <- prob_b_given_a %>%
  group_by(A) %>%
  arrange(desc(`p(B|A)`)) %>%
  summarise(most_likely_B=first(B)) %>%
  filter(A != "*") %>%
  filter(most_likely_B != "*") %>%
  group_by(most_likely_B) %>%
  summarize(count=n())

# we find "vv" and "Y3" are the most likely characters to follow something else






