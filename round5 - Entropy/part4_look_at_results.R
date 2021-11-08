##################################################################
# libraries I'm using
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

# our results
results <- readRDS("round5 - Entropy/results.Rds") 

# making the table to compare with Stamp
table <- results %>% filter(n <= 3)

# output all results to a table too
table <- results %>% arrange(item, n)
write.csv(table,"round5 - Entropy/results.csv")

# pretty plot time
p1 <- ggplot(results %>% filter(n <=15), aes(x=n, y=entropy, color=item)) +
  geom_point() +
  geom_line() +
  labs(x="Number of Tokens", y="Entropy") +
  ggtitle("Entropy - Linear Scale") +
  scale_colour_manual(name=" ",
                      values = c("Hamptonese (Method A)"="#ff0000",
                                 "Hamptonese (Method B)"="#ffae00",
                                 "Book of Psalms (characters)"="#179132",
                                 "Book of Psalms (words)"="#76b884",
                                 "Book of Nonsense (characters)"="#6068d6",
                                 "Book of Nonsense (words)"="#8e93d1")) +
  theme_bw()

p2 <- ggplot(results %>% filter(n <=15) %>% mutate(entropy=log(entropy)), aes(x=n, y=entropy, color=item)) +
  geom_point() +
  geom_line() +
  #scale_y_continuous(trans='log') +
  #scale_x_continuous(trans='log') + 
  ggtitle("Entropy - Log Transform") +
  labs(x="Number of Tokens", y="log(Entropy)") +
  scale_colour_manual(name=" ",
                      values = c("Hamptonese (Method A)"="#ff0000",
                                 "Hamptonese (Method B)"="#ffae00",
                                 "Book of Psalms (characters)"="#179132",
                                 "Book of Psalms (words)"="#76b884",
                                 "Book of Nonsense (characters)"="#6068d6",
                                 "Book of Nonsense (words)"="#8e93d1")) +
  theme_bw()
#grid.arrange(p1, p2, nrow=1)



# looking deeper into word  and letter distributions

ggplot(results %>% filter(n <=50) %>% 
               mutate(entropy=log(entropy)) %>%
               filter(item %in% c("Book of Psalms (characters)","Book of Psalms (words)")), aes(x=n, y=entropy, color=item)) +
  geom_point() +
  geom_line() +
  #scale_y_continuous(trans='log') +
  #scale_x_continuous(trans='log') + 
  ggtitle("Entropy - Log Transform") +
  labs(x="Number of Tokens", y="log(Entropy)") +
  scale_colour_manual(name=" ",
                      values = c("Hamptonese (Method A)"="#ff0000",
                                 "Hamptonese (Method B)"="#ffae00",
                                 "Book of Psalms (characters)"="#179132",
                                 "Book of Psalms (words)"="#76b884",
                                 "Book of Nonsense (characters)"="#6068d6",
                                 "Book of Nonsense (words)"="#8e93d1")) +
  theme_bw()




# how many words are there again?
words <- readRDS("round4 - Zipf/data_psalms_words.Rds") %>%
  group_by(char) %>%
  summarise(count=n())

# avg psalm length?
lines <- readRDS("round4 - Zipf/data_psalms_words.Rds")  %>%
  group_by(line_id) %>%
  summarise(words=n())


##################################################################
# function to collapse all tokens into a single line
# (so I can use tidytext to simplify the n-grams)
collapse_into_line <- function(data) {
  data <- data %>%
    group_by(line_id) %>%
    summarise(line=paste(char, collapse= " ")) %>%
    # hacky cleanup to play nice with tidytext tokenizer
    mutate(line=str_replace_all(line,"_","S"),
           line=str_replace_all(line,"\\*","UNKNOWN"),
           line=str_replace_all(line,"-","DASH"))
  return(data)
}
text_psalms_chars <- collapse_into_line(readRDS("round4 - Zipf/data_psalms_characters.Rds")) %>%
  unnest_tokens(ngram, line, token="ngrams", n=50, to_lower=FALSE) %>%
  group_by(ngram) %>%
  summarise(count=n()) %>%
  filter(count==1)



# book of nonsense

ggplot(results %>% filter(n <=50) %>% 
         mutate(entropy=log(entropy)) %>%
         filter(item %in% c("Book of Psalms (characters)","Book of Psalms (words)","Book of Nonsense (characters)","Book of Nonsense (words)")), aes(x=n, y=entropy, color=item)) +
  geom_point() +
  geom_line() +
  #scale_y_continuous(trans='log') +
  #scale_x_continuous(trans='log') + 
  ggtitle("Entropy - Log Transform") +
  labs(x="Number of Tokens", y="log(Entropy)") +
  scale_colour_manual(name=" ",
                      values = c("Hamptonese (Method A)"="#ff0000",
                                 "Hamptonese (Method B)"="#ffae00",
                                 "Book of Psalms (characters)"="#179132",
                                 "Book of Psalms (words)"="#76b884",
                                 "Book of Nonsense (characters)"="#6068d6",
                                 "Book of Nonsense (words)"="#8e93d1")) +
  theme_bw()



# avg nonsense length?
lines <- readRDS("round4 - Zipf/data_nonsense_words.Rds")  %>%
  group_by(line_id) %>%
  summarise(words=n())


# A v B

ggplot(results %>% filter(n <=11) %>% 
         mutate(entropy=log(entropy)) %>%
         filter(item %in% c("Hamptonese (Method A)","Hamptonese (Method B)")), aes(x=n, y=entropy, color=item)) +
  geom_point() +
  geom_line() +
  #scale_y_continuous(trans='log') +
  #scale_x_continuous(trans='log') + 
  ggtitle("Entropy - Log Transform") +
  labs(x="Number of Tokens", y="log(Entropy)") +
  scale_colour_manual(name=" ",
                      values = c("Hamptonese (Method A)"="#ff0000",
                                 "Hamptonese (Method B)"="#ffae00",
                                 "Book of Psalms (characters)"="#179132",
                                 "Book of Psalms (words)"="#76b884",
                                 "Book of Nonsense (characters)"="#6068d6",
                                 "Book of Nonsense (words)"="#8e93d1")) +
  theme_bw()


