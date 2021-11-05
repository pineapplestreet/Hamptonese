# libraries I'm using
library(dplyr)
library(ggplot2)
library(scales)

# grab our data
hamptonese_clean <- readRDS("round2 - Nov 2021/hamptonese_clean.Rds")
psalms <- read.csv("round2 - Nov 2021/python_output_psalms.csv")

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

# run on Hamptonese v. Psalms
freq_psalms <- super_basic_zipf_analysis(psalms)
freq_hamptonese <- super_basic_zipf_analysis(hamptonese_clean)

# holy shit - Hamptonese is following Zipf's law

# measuring fit - just compare total absolute error
sum(freq_psalms$abs_error)
sum(freq_hamptonese$abs_error)

# plot the first 25 items
ggdata <- data.frame(Item="Zip's Law",
                     Rank=seq(1,25,1),
                     Value=(freq_psalms %>% filter(rank <= 25))$zipf)
ggdata <- rbind(data.frame(Item="Psalms (Letters)",
                           Rank=seq(1,25,1),
                           Value=(freq_psalms %>% filter(rank <= 25))$norm),
                ggdata)
ggdata <- rbind(data.frame(Item="Hamptonese",
                           Rank=seq(1,25,1),
                           Value=(freq_hamptonese %>% filter(rank <= 25))$norm),
                ggdata)


ggplot(ggdata,aes(x=Rank,y=Value,color=Item)) +
  geom_point() + 
  geom_line() +
  labs(x="Rank",y="Min/Max Normalized Frequency") +
  ggtitle("Linear Scale: 25 Most Common Tokens")

ggplot(ggdata,aes(x=Rank,y=Value,color=Item)) +
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