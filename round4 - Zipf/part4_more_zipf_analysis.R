# libraries I'm using
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)
library(gridExtra)

# we finally have nice clean data to compare everything against!
data_stamp_freq <- read.csv("round4 - Zipf/data_stamp_freq.csv") 
data_hamptonese_methodA <- readRDS("round4 - Zipf/data_hamptonese_methodA.Rds") %>% filter(char!="*") 
data_hamptonese_methodB <- readRDS("round4 - Zipf/data_hamptonese_methodB.Rds") %>% filter(char!="*") 

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

# deal with Stamp
data_stamp_freq2 <- data_stamp_freq %>%
  mutate(char=key,
         freq=count,
         norm=minmax_norm(freq)) %>%
  arrange(desc(freq)) %>%
  mutate(rank=row_number()) %>%  
  mutate(Item="Stamp & Le",
         Rank=rank,
         Value=norm) %>%
  select(Item,Rank,Value)

# compare token frequencies in Hamptonese v. Psalms v. Nonsense
freq_hamptonese_methodA <- super_basic_zipf_analysis(data_hamptonese_methodA)
freq_hamptonese_methodB <- super_basic_zipf_analysis(data_hamptonese_methodB)

# combine everything into a nice long dataframe to play nice with ggplot
freq_results <- data.frame(Item="Zipf's Law",
                           Rank=freq_hamptonese_methodA$rank,
                           Value=freq_hamptonese_methodA$zipf)
freq_results <- rbind(freq_results,data.frame(Item="Method A",
                                              Rank=freq_hamptonese_methodA$rank,
                                              Value=freq_hamptonese_methodA$norm))
freq_results <- rbind(freq_results,data.frame(Item="Method B",
                                              Rank=freq_hamptonese_methodB$rank,
                                              Value=freq_hamptonese_methodB$norm))
freq_results <- rbind(freq_results,data_stamp_freq2)



# look at the first 42 words only
zipf1 <- freq_results %>% 
  filter(Rank <= 21)
zipf2 <- freq_results %>% 
  filter(Rank >= 22 & Rank <= 42)


p1 <- ggplot(zipf1,aes(x=Rank,y=Value,color=Item)) +
  geom_point() + 
  #geom_line() +
  scale_y_continuous(trans='log') +
  scale_x_continuous(trans='log') + 
  labs(x="Log Transform Rank",y=" ") +
  ggtitle("Log/Log Transform - Rank 1 to 21") +
  scale_colour_manual(name=" ",
                      values = c("Stamp & Le"="#000000",
                                 "Method A"="#ff0000",
                                 "Method B"="#ffae00",
                                 "Zipf's Law"="#0330fc")) +
  theme_bw() +
  theme(legend.position="bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


p2 <- ggplot(zipf2,aes(x=Rank,y=Value,color=Item)) +
  geom_point() + 
  #geom_line() +
  scale_y_continuous(trans='log') +
  scale_x_continuous(trans='log') + 
  labs(x="Log Transform Rank",y=" ") +
  ggtitle("Log/Log Transform - Rank 22 to 42") +
  scale_colour_manual(name=" ",
                      values = c("Stamp & Le"="#000000",
                                 "Method A"="#ff0000",
                                 "Method B"="#ffae00",
                                 "Zipf's Law"="#0330fc")) +
  theme_bw() +
  theme(legend.position="bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
grid.arrange(p1, p2, nrow=1)
