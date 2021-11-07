##################################################################
# libraries I'm using
library(dplyr)
library(tidytext)
library(stringr)

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

##################################################################
# load up all our data
text_psalms_words <- collapse_into_line(readRDS("round4 - Zipf/data_psalms_words.Rds"))
text_psalms_chars <- collapse_into_line(readRDS("round4 - Zipf/data_psalms_characters.Rds"))
text_nonsense_words <- collapse_into_line(readRDS("round4 - Zipf/data_nonsense_words.Rds"))
text_nonsense_chars <- collapse_into_line(readRDS("round4 - Zipf/data_nonsense_characters.Rds"))
text_hamptoneseA <- collapse_into_line(readRDS("round4 - Zipf/data_hamptonese_methodA.Rds") %>% mutate(line_id=paste0(revelation_no,'_',line_no)))
text_hamptoneseB <- collapse_into_line(readRDS("round4 - Zipf/data_hamptonese_methodB.Rds") %>% mutate(line_id=paste0(revelation_no,'_',line_no)))

##################################################################
# calculate Shanon's entropy (n=1) or conditional entropy (n>1)
calculate_entropy <- function(df, item, n) {
  
  # break into n-grams of length n
  # we want to preserve any remaining punctuation (spaces [_]) and caps (Hamptonese) 
  df <- df %>% unnest_tokens(ngram, line, token="ngrams", n=n, to_lower=FALSE) 
  
  # if n = 1 we use Shanon's Entropy
  if(n==1) {
    probs <- df %>%
      group_by(ngram) %>%
      summarise(count=n()) %>%
      mutate(p=count/nrow(df),
             log2p = log2(p),
             entropy = p*log2p) 
    entropy <- sum(probs$entropy)*-1
    return(data.frame(item=item, n=n, entropy=entropy, avg_choices=length(unique(probs$ngram)), note="Shanon's Entropy"))
  }
  
  # if n > 1 we need Conditional Entropy
  else if(n > 1) {
    # Stamp gives us the the formula
    # P(abc) log(P(c | ab))
    # example: for n = 2 we want P(ab)*log(P(b|a)) => p_ngram * log2(p_b_given_a)
    # example: for n = 3 we want P(abc)*log(P(c|ab)) => p_ngram * log2(p_b_given_a)
    # example: for n = 5 we want P(abcde)*log(P(e|abcd)) => p_ngram * log2(p_b_given_a)
    
    # for simple code we just use p(B|A) regardless of length
    tmp <- df %>%
    mutate(A = word(ngram, 1, (n-1)),
           B = word(ngram,-1))
    
    # if n is greater than the available length of a line, remove it
    tmp <- tmp %>%
      filter(!is.na(ngram))
    
    # if nothing exists with the given length, we have no entropy
    if(nrow(tmp)==0) {return(data.frame(item=item, n=n, entropy=0, note="Doesn't Exist"))}
    
    # get freq(B|A)
    freq_ab <- tmp %>%
      group_by(A,B) %>%
      summarise(freq_AB=n())
    tmp <- tmp %>% left_join(freq_ab, by=c("A", "B"))
    
    # find p(ngram)
    p_ngram <- tmp %>%
      group_by(ngram) %>%
      summarise(freq=n()) %>%
      mutate(total=nrow(tmp),
             p_ngram=freq/total) %>%
      select(ngram, p_ngram)
    tmp <- tmp %>% left_join(p_ngram)
    
    # find freq(A)
    freq_A <- tmp %>%
      group_by(A) %>%
      summarise(freq_A=n(),
                choices=length(unique(B))) %>%
      select(A, freq_A, choices)
    tmp <- tmp %>% left_join(freq_A)  
    
    # find p(b_given_a) and entropy
    tmp <- tmp %>%
      mutate(p_b_given_a=freq_AB/freq_A) %>%
      mutate(log2p = log2(p_b_given_a)) %>%
      mutate(entropy=p_ngram*log2p*-1)
    
    # de-dupe so we have one row per ngram
    tmp <- tmp %>%
      group_by(ngram) %>%
      summarise(choices=first(choices),
                entropy=first(entropy))
    return(data.frame(item=item, n=n, entropy=mean(tmp$entropy), avg_choices=mean(tmp$choices), note="Conditional Entropy"))    
  }
}
check2 <- calculate_entropy(text_hamptoneseB,"Hamptonese (Method B)", 2)


check1 <- calculate_entropy(text_psalms_chars,"Book of Psalms (characters)", 1)
check2 <- calculate_entropy(text_psalms_words,"Book of Psalms (words)", 1)
check3 <- calculate_entropy(text_nonsense_chars,"Book of Nonsense (characters)", 1)
check4 <- calculate_entropy(text_nonsense_words,"Book of Nonsense (words)", 1)
check5 <- calculate_entropy(text_hamptoneseA,"Hamptonese (Method A)", 1)
check6 <- calculate_entropy(text_hamptoneseB,"Hamptonese (Method B)", 1)






