# libraries I'm using
library(dplyr)
library(ggplot2)
library(scales)

# we finally have nice clean data to compare everything against!
data_hamptonese_clean <- readRDS("round4 - Zipf/data_hamptonese_clean.Rds")
data_psalms_characters <- readRDS("round4 - Zipf/data_psalms_characters.Rds")
data_nonsense_characters <- readRDS("round4 - Zipf/data_nonsense_characters.Rds")
data_psalms_words <- readRDS("round4 - Zipf/data_psalms_words.Rds")
data_nonsense_words <- readRDS("round4 - Zipf/data_nonsense_words.Rds")

# min max normalize
minmax_norm <- function(vector) {
  min = min(vector)
  max = max(vector)  
  denominator = max-min
  norm <- vector-min
  norm <- norm/denominator
  return(norm)
}

# nromalize and use the "super basic" zipf law estimate of 1/n based on min-max normalization
super_basic_zipf_analysis <- function(df) {
  # character frequency in psalms
  output <- df %>%
    group_by(char) %>%
    summarise(freq=n()) %>%
    arrange(desc(freq)) %>%
    mutate(rank=row_number(),
           norm=minmax_norm(freq),
           zipf=1/rank,
           abs_error=abs(norm-zipf))
  return(output)
}

# compare token frequencies in Hamptonese v. Psalms v. Nonsense
freq_hamptonese <- super_basic_zipf_analysis(data_hamptonese_clean)
freq_psalms_characters <- super_basic_zipf_analysis(data_psalms_characters)
freq_psalms_words <- super_basic_zipf_analysis(data_psalms_words)
freq_nonsense_characters <- super_basic_zipf_analysis(data_nonsense_characters)
freq_nonsense_words <- super_basic_zipf_analysis(data_nonsense_words)


freq_hamptonese <- super_basic_zipf_analysis(hamptonese_clean)

# holy shit - Hamptonese is following Zipf's law

# measuring fit - just compare total absolute error
# this is garbage because we have way more samples (errors) in the word analysis
sum(freq_hamptonese$abs_error)
sum(freq_psalms_characters$abs_error)
sum(freq_psalms_words$abs_error)
sum(freq_nonsense_characters$abs_error)
sum(freq_nonsense_words$abs_error)

# combine everything into a nice long dataframe to play nice with ggplot
freq_results <- data.frame(Item="Zip's Law",
                           Rank=freq_psalms_words$rank,
                           Value=freq_psalms_words$zipf,
                           abs_error=0)
freq_results <- rbind(freq_results,data.frame(Item="Hamptonese",
                                              Rank=freq_hamptonese$rank,
                                              Value=freq_hamptonese$norm,
                                              abs_error=freq_hamptonese$abs_error))
freq_results <- rbind(freq_results,data.frame(Item="Psalms (Characters)",
                                              Rank=freq_psalms_characters$rank,
                                              Value=freq_psalms_characters$norm,
                                              abs_error=freq_psalms_characters$abs_error))
freq_results <- rbind(freq_results,data.frame(Item="Psalms (Words)",
                                              Rank=freq_psalms_words$rank,
                                              Value=freq_psalms_words$norm,
                                              abs_error=freq_psalms_words$abs_error))
freq_results <- rbind(freq_results,data.frame(Item="Nonsense (Characters)",
                                              Rank=freq_nonsense_characters$rank,
                                              Value=freq_nonsense_characters$norm,
                                              abs_error=freq_nonsense_characters$abs_error))
freq_results <- rbind(freq_results,data.frame(Item="Nonsense (Words)",
                                              Rank=freq_nonsense_words$rank,
                                              Value=freq_nonsense_words$norm,
                                              abs_error=freq_nonsense_words$abs_error))



# look at the first 100 words only
zipf1 <- freq_results %>% 
  filter(Rank <= 100) %>%
  filter(!(Item %in% c('Nonsense (Characters)',
                       'Psalms (Characters)')))

# look at the first 25 characters only
zipf1 <- freq_results %>% 
  filter(Rank <= 25) %>%
  filter(!(Item %in% c('Nonsense (Words)',
                       'Psalms (Words)')))

quantify_it <- zipf1 %>%
  group_by(Item) %>%
  summarise(total_absolute_error=sum(abs_error))


ggplot(zipf1,aes(x=Rank,y=Value,color=Item)) +
  geom_point() + 
  geom_line() +
  labs(x="Rank",y="Min/Max Normalized Frequency") +
  ggtitle("Linear Scale: 25 Most Common Tokens")

ggplot(zipf1,aes(x=Rank,y=Value,color=Item)) +
  geom_point() + 
  geom_line() +
  scale_y_continuous(trans='log') +
  scale_x_continuous(trans='log') +  
  labs(x="log(Rank)",y="log(Min/Max Normalized Frequency)") +
  ggtitle("Log/Log Scale: 25 Most Common Tokens")


# this is very exciting - we have something worth an article

# compare to - psalms as words
# compare to - some sort of nonsense language - e.g. "markov chain generated text"
saveRDS(psalms,"round2 - Nov 2021/psalms_characters.Rds")