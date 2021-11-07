# libraries I'm using
library(dplyr)
library(tidytext)
library(stringr)

# we finally have nice clean data to compare everything against!
data_psalms_words <- readRDS("round4 - Zipf/data_psalms_words.Rds") 
data_psalms_characters <- readRDS("round4 - Zipf/data_psalms_characters.Rds") 
data_nonsense_words <- readRDS("round4 - Zipf/data_nonsense_words.Rds") 
data_nonsense_characters <- readRDS("round4 - Zipf/data_nonsense_characters.Rds") 
data_hamptonese_methodA <- readRDS("round4 - Zipf/data_hamptonese_methodA.Rds") %>% mutate(line_id=paste0(revelation_no,'_',line_no))
data_hamptonese_methodB <- readRDS("round4 - Zipf/data_hamptonese_methodB.Rds") %>% mutate(line_id=paste0(revelation_no,'_',line_no))

check <- data_psalms_words %>%
  group_by(char) %>%
  summarise(count=n()) %>%
  mutate(p=count/nrow(data_psalms_words))

# calculate Shannon's entropy base 2
calculate_shanon_entropy <- function(df) {
  probs <- df %>%
    group_by(char) %>%
    summarise(count=n()) %>%
    mutate(p=count/nrow(df),
           log2p = log2(p),
           entropy = p*log2p) 
  return(sum(probs$entropy)*-1)
}
entropy_psalms_words <- calculate_shanon_entropy(data_psalms_words)
entropy_psalms_chars <- calculate_shanon_entropy(data_psalms_characters)
entropy_nonsense_words <- calculate_shanon_entropy(data_nonsense_words)
entropy_nonsense_chars <- calculate_shanon_entropy(data_nonsense_characters)
entropy_hamptonese_methodA <- calculate_shanon_entropy(data_hamptonese_methodA)
entropy_hamptonese_methodB <- calculate_shanon_entropy(data_hamptonese_methodB)



########################################################################
# just to demonstrate
calculate_conditional_entropy_words_explained <- function(data, depth) {
  #example <- "the fool hath said in his heart there is no god"
  #example <- "therefore the ungodly shall not stand in the judgment"
  example <- "Ki Ki PL N vv 95 96 P"
  match = word(example, 1, depth)
  
  check <- data %>%
    group_by(line_id) %>%
    summarise(line=paste(char, collapse= " ")) %>%
    unnest_tokens(ngram, line, token="ngrams", n=depth+1, to_lower = FALSE) %>%
    mutate(A = word(ngram, 1, depth),
           B = word(ngram,-1),
           match=match)  %>%
    filter(A==match) 
  freqA <- check %>%
    group_by(A) %>%
    summarise(freqA=n()) 
  check <- check %>% 
    left_join(freqA) %>%
    group_by(A,B) %>%
    summarise(freqA=max(freqA),
              freqAB=n(),
              pBA=freqAB/freqA) %>%
    ungroup() %>%
    group_by(A) %>%
    summarise(choices=length(unique(B)),
              avgP=mean(pBA)) %>%
    mutate(string=A) %>%
    select(string, choices, avgP)
  return(check)
  check <- check %>%
    group_by(A) %>%
    summarise(sum_entropyA =sum(entropy)*-1)
  return(mean(check$sum_entropyA))
}


example <- data.frame()
for(i in 1:8) {
  words <- calculate_conditional_entropy_words_explained(data_hamptonese_methodA, i)  
  example <- rbind(example,words)
}


next_word <- calculate_conditional_entropy_words_explained(data_psalms_words, 8) 


# the fool hath said in his heart there is no god  <- for an example
