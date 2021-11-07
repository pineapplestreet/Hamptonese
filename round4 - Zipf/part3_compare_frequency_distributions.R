# libraries I'm using
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)
library(gridExtra)

# how does this compare to what Stamp found?
data_hamptonese_methodA <- readRDS("round4 - Zipf/data_hamptonese_methodA.Rds") %>% filter(char!="*") 
data_hamptonese_methodA <- data_hamptonese_methodA %>%
  group_by(char) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  mutate(rank=row_number(),
         method='Method A',
         total=nrow(data_hamptonese_methodA),
         rel_freq=count/total)  %>%
  select(method,char,rank,rel_freq,count)
  
data_hamptonese_methodB <- readRDS("round4 - Zipf/data_hamptonese_methodB.Rds") %>% filter(char!="*") 
data_hamptonese_methodB <- data_hamptonese_methodB %>%
  group_by(char) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  mutate(rank=row_number(),
         method='Method B',
         total=nrow(data_hamptonese_methodB),
         rel_freq=count/total)  %>%
  select(method,char,rank,rel_freq,count)

data_stamp_freq <- read.csv("round4 - Zipf/data_stamp_freq.csv") %>%
  arrange(desc(count)) %>%
  mutate(char=key,
         rank=row_number(),
         method='Stamp') %>%
  select(method,char,rank,rel_freq,count)

freq <- rbind(data_stamp_freq,data_hamptonese_methodA,data_hamptonese_methodB) %>%
  filter(char %in% data_stamp_freq$char) 

# relative frequency
p1 <- ggplot(freq, aes(x=reorder(char,rank),y=rel_freq, color=method)) +
  geom_point() +
  scale_colour_manual(name=" ",
                      values = c("Stamp & Le"="#000000",
                                 "Method A"="#ff0000",
                                 "Method B"="#ffae00")) +
  theme_bw() +
  #scale_y_continuous(trans='log') +
  scale_y_continuous(labels=percent) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  labs(x="Hamptonese Token",y="Relative Frequency") +
  ggtitle("Linear Scale")

# relative frequency
p2 <- ggplot(freq, aes(x=reorder(char,rank),y=rel_freq, color=method)) +
  geom_point() +
  scale_colour_manual(name=" ",
                      values = c("Stamp & Le"="#000000",
                                 "Method A"="#ff0000",
                                 "Method B"="#ffae00")) +
  theme_bw() +
  scale_y_continuous(trans='log') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(x="Hamptonese Token",y="") +
  ggtitle("Log Transform")

library(gridExtra)
grid.arrange(p1, p2, nrow=1)

