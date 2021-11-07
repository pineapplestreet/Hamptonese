# libraries I'm using
library(dplyr)
library(tidytext)
library(stringr)

# hamptonese can be treated same as words

# we finally have nice clean data to compare everything against!
data_psalms_words <- readRDS("round4 - Zipf/data_psalms_words.Rds") 
data_nonsense_words <- readRDS("round4 - Zipf/data_nonsense_words.Rds") 
data_hamptonese_methodA <- readRDS("round4 - Zipf/data_hamptonese_methodA.Rds") %>% mutate(line_id=paste0(revelation_no,'_',line_no))
data_hamptonese_methodB <- readRDS("round4 - Zipf/data_hamptonese_methodB.Rds") %>% mutate(line_id=paste0(revelation_no,'_',line_no))

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
entropy_nonsense_words <- calculate_shanon_entropy(data_nonsense_words)
entropy_hamptonese_methodA <- calculate_shanon_entropy(data_hamptonese_methodA)
entropy_hamptonese_methodB <- calculate_shanon_entropy(data_hamptonese_methodB)


########################################################################
# conditional probability - words
calculate_conditional_entropy_words <- function(data, depth) {
  check <- data %>%
    group_by(line_id) %>%
    summarise(line=paste(char, collapse= " ")) %>%
    unnest_tokens(ngram, line, token="ngrams", n=depth+1, to_lower = FALSE) %>%
    mutate(A = word(ngram, 1, depth),
           B = word(ngram,-1))
  freqA <- check %>%
    group_by(A) %>%
    summarise(freqA=n()) 
  check <- check %>% 
    left_join(freqA) %>%
    group_by(A,B) %>%
    summarise(freqA=max(freqA),
              freqAB=n(),
              pAB=freqAB/freqA,
              log2pAB = log2(pAB),
              entropy = pAB*log2pAB)
  #return(check)
  check <- check %>%
    group_by(A) %>%
    summarise(sum_entropyA =sum(entropy)*-1)
  return(mean(check$sum_entropyA))
}



words1 <- calculate_conditional_entropy_words(data_hamptonese_methodA, 7)
words2 <- calculate_conditional_entropy_words(data_hamptonese_methodA, 8)

words3 <- calculate_conditional_entropy_words(data_hamptonese_methodB, 7)
words4 <- calculate_conditional_entropy_words(data_hamptonese_methodB, 8)




