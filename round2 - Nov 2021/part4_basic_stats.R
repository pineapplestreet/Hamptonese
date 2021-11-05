# libraries I'm using
library(dplyr)
library(ggplot2)
library(scales)

# grab our data
hamptonese_clean <- readRDS("round2 - Nov 2021/hamptonese_clean.Rds")


##########################################################################
# character frequency
char_freq <- hamptonese_clean %>%
  group_by(char) %>%
  summarise(n=n())
ggplot(char_freq, aes(x=reorder(char,n), y=n, fill=n)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=comma) +
  coord_flip() +
  ggtitle("Frequency of Hampton Symbols") +
  labs(x="Hampton Symbol",y="Appearances in Corpus") +
  theme_bw()

# character ratio
total_symbols <- sum(char_freq$n)
char_freq <- char_freq %>%
  mutate(total=total_symbols,
         ratio=n/total_symbols)
ggplot(char_freq, aes(x=reorder(char,ratio), y=ratio, fill=ratio)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=percent) +
  coord_flip() +
  ggtitle("Ratio of Hampton Symbols") +
  labs(x="Hampton Symbol",y="Percent of Corpus") +
  theme_bw()


##########################################################################
# how rare are these symbols? - per revelation
total_revelations <- length(unique(hamptonese_clean$revelation))
symbols_per_revelation <- hamptonese_clean %>%
  group_by(char) %>%
  summarise(rev_appearing_in=length(unique(revelation))) %>%
  mutate(percent=rev_appearing_in/total_revelations)
ggplot(symbols_per_revelation, aes(x=reorder(char,percent), y=percent, fill=percent)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=percent) +
  coord_flip() +
  ggtitle("Hamptonese per Revelation") +
  labs(x="Hampton Symbol",y="Percent of Revelations Appearing In") +
  theme_bw()

check <- symbols_per_revelation %>% filter(percent < 0.5)



##########################################################################
# how rare are these symbols? - per revelation
total_lines <- length(unique(hamptonese_clean$line_id))
symbols_per_line <- hamptonese_clean %>%
  group_by(char) %>%
  summarise(lines_appearing_in=length(unique(line_id))) %>%
  mutate(percent=lines_appearing_in/total_lines)
ggplot(symbols_per_line, aes(x=reorder(char,percent), y=percent, fill=percent)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=percent) +
  coord_flip() +
  ggtitle("Hamptonese per Line") +
  labs(x="Hampton Symbol",y="Percent of Lines Appearing In") +
  theme_bw()
