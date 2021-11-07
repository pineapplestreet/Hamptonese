# libraries I'm using
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)

# how does this compare to what Stamp found?
data_hamptonese_clean <- readRDS("round4 - Zipf/data_hamptonese_clean.Rds")
data_stamp_freq <- read.csv("round4 - Zipf/data_stamp_freq.csv")

# ignored by stamp
ignored <- data_hamptonese_clean %>%
  filter(!(char %in% data_stamp_freq$key)) %>%
  group_by(char) %>%
  summarise(count=n())

# we find 14 characters in our "methodB" data that is missing/ignored by stamp
# this includes all the English tokens and the (*) unknown character
data_hamptonese_methodB_freq <- data_hamptonese_clean %>%
  filter((char %in% data_stamp_freq$key)) %>%
  group_by(char) %>%
  summarise(methodB_count=n()) %>%
  left_join(data_stamp_freq, by=c("char"="key"))

# we are under-counting with method B relative to Stamp
print(ignored)

# look at method A - how does that compare?
data_hamptonese <- readRDS("round4 - Zipf/data_hamptonese.Rds")
data_hamptonese_methodA_freq <- data_hamptonese %>%
  mutate(char=methodA) %>%
  group_by(char) %>%
  summarise(methodA_count=n()) %>%
  left_join(data_stamp_freq, by=c("char"="key"))

# ignored by stamp
ignoredB <- data_hamptonese %>%
  mutate(char=methodB) %>%
  filter(!(char %in% data_stamp_freq$key)) %>%
  group_by(char) %>%
  summarise(Bcount=n())

# ignored by stamp
ignoredA <- data_hamptonese %>%
  mutate(char=methodB) %>%
  filter(!(char %in% data_stamp_freq$key)) %>%
  group_by(char) %>%
  summarise(Acount=n())


#####################################################################
# do we have any other missing characters?
check_hamptonese <- read.csv("round1 - Oct 2021/HampTrans.csv")
check_key <- read.csv("source_materials/HampKey.csv")
ignored_tokens <- check_hamptonese %>%
  mutate(char=str_replace(char,"#","")) %>%
  group_by(char) %>%
  summarise(count=n()) %>%
  filter(!(char %in% data_stamp_freq$key)) %>%
  mutate(in_dictionary=ifelse(char %in% check_key$char,"Yes","No"))


#####################################################################
# compare these relative frequencies - how big of a difference?
stamp <- data_stamp_freq %>%
  mutate(char=key) %>%
  select(char, count, rel_freq) %>%
  arrange(desc(rel_freq)) %>% 
  mutate(rank=row_number(),
         method="Stamp")
  
sumA <- sum(data_hamptonese_methodA_freq$methodA_count)
pauseA <- data_hamptonese_methodA_freq %>%
  select(-count) %>%
  mutate(count=methodA_count,
         rel_freq=methodA_count/sumA) %>%
  select(char, count, rel_freq) %>%
  arrange(desc(rel_freq)) %>% 
  mutate(rank=row_number(),
         method="Pause A")


sumB <- sum(data_hamptonese_methodA_freq$methodA_count)
pauseB <- data_hamptonese_methodB_freq %>%
  select(-count) %>%
  mutate(count=methodB_count,
         rel_freq=methodB_count/sumB) %>%
  select(char, count, rel_freq) %>%
  arrange(desc(rel_freq)) %>% 
  mutate(rank=row_number(),
         method="Pause B")


# combine into a wide table for display...
combined_wide <- stamp %>%
  mutate(stamp_count=count,
         stamp_rank=rank) %>%
  select(char, stamp_count, stamp_rank)

tmp <- pauseA %>%
  mutate(methodA_count=count,
         methodA_rank=rank) %>%
  select(char, methodA_count, methodA_rank)
combined_wide <- combined_wide %>% left_join(tmp)

tmp <- pauseB %>%
  mutate(methodB_count=count,
         methodB_rank=rank) %>%
  select(char, methodB_count, methodB_rank)
combined_wide <- combined_wide %>% left_join(tmp) %>%
  
  
combined_wide <- combined_wide %>% mutate(avg_rank=(stamp_rank+methodA_rank+methodB_rank)/3)


#####################################################################
# we have more precisely defined Method A and Method B now
# let's regenerate data frames with these clear definitions

# start with the raw data
data_hamptonese_raw <- read.csv("round1 - Oct 2021/HampTrans.csv")
key_hamptonese <- read.csv("source_materials/HampKey.csv")
data_stamp_freq <- read.csv("round4 - Zipf/data_stamp_freq.csv") %>%
  mutate(char=key)

# get what we need to define Method A and B
data_hamptonese_method <- data_hamptonese_raw %>%
  mutate(unsure=ifelse(str_detect(char, "#"),'Y','N'),
         unsure_clean=str_replace(char, "#","")) %>%
  left_join(key_hamptonese) %>%
  left_join(data_stamp_freq) %>%
  mutate(in_dictionary=ifelse(!is.na(weight),'Y','N'),
         in_stamps_42=ifelse(!is.na(rel_freq),'Y','N'),
         orig=char)

# method A is more generous
# include items we are unsure about and items not in the dictionary
data_hamptonese_methodA <- data_hamptonese_method %>%
  mutate(char=unsure_clean,
         revelation_no=revelation) %>%
  select(page_no, hampton_page_no, line_no, char_no, revelation_no, char)
saveRDS(data_hamptonese_methodA,"round4 - Zipf/data_hamptonese_methodA.Rds")


# method B is conservative
# ignore items we are unsure about
# ignore items not in the dictionary
data_hamptonese_methodB <- data_hamptonese_method %>%
  mutate(char=ifelse(unsure=='Y','*',char),
         char=ifelse(in_dictionary=='N','*',char),
         char=ifelse(in_stamps_42=='N','*',char),         
         revelation_no=revelation) %>%
  select(page_no, hampton_page_no, line_no, char_no, revelation_no, char)
saveRDS(data_hamptonese_methodB,"round4 - Zipf/data_hamptonese_methodB.Rds")


# method B is conservative
# ignore items we are unsure about
# ignore items not in the dictionary
data_hamptonese_methodC <- data_hamptonese_method %>%
  mutate(char=unsure_clean,
         #char=ifelse(in_stamps_42=='N','*',char), 
         revelation_no=revelation) %>%  
  select(page_no, hampton_page_no, line_no, char_no, revelation_no, char)
#saveRDS(data_hamptonese_methodB,"round4 - Zipf/data_hamptonese_methodB.Rds")



#####################################################################
# compare these relative frequencies again with defined A and B- how big of a difference?
stamp <- data_stamp_freq %>%
  mutate(char=key) %>%
  select(char, count, rel_freq) %>%
  arrange(desc(rel_freq)) %>% 
  mutate(rank=row_number(),
         method="Stamp")

# remove all unknown (*)
totalA <- nrow(data_hamptonese_methodA %>% filter(char!="*"))
methodA <- data_hamptonese_methodA %>%
  filter(char!="*") %>%
  group_by(char) %>%
  summarise(count=n()) %>%
  mutate(rel_freq=count/totalA) %>%
  arrange(desc(rel_freq)) %>% 
  mutate(rank=row_number(),
         method="MethodA")

# remove all unknown (*)
totalB <- nrow(data_hamptonese_methodB %>% filter(char!="*"))
methodB <- data_hamptonese_methodB %>%
  filter(char!="*") %>%
  group_by(char) %>%
  summarise(count=n()) %>%
  mutate(rel_freq=count/totalB) %>%
  arrange(desc(rel_freq)) %>% 
  mutate(rank=row_number(),
         method="MethodB")


# remove all unknown (*)
totalC <- nrow(data_hamptonese_methodC %>% filter(char!="*"))
methodC <- data_hamptonese_methodC %>%
  filter(char!="*") %>%
  group_by(char) %>%
  summarise(count=n()) %>%
  mutate(rel_freq=count/totalC) %>%
  arrange(desc(rel_freq)) %>% 
  mutate(rank=row_number(),
         method="MethodC*")

# combine into a long table
long <- stamp %>%
  mutate(val=count,
         key="Stamp_Count") %>%
  select(char, key, val)
long <- rbind(long,
              stamp %>%
              mutate(val=rank,
                     key="Stamp_Rank") %>%
              select(char, key, val))
long <- rbind(long,
              stamp %>%
                mutate(val=rel_freq,
                       key="Stamp_Rel_Freq") %>%
                select(char, key, val))
long <- rbind(long,
              methodA %>%
                mutate(val=count,
                       key="MethodA_Count") %>%
                select(char, key, val))
long <- rbind(long,
              methodA %>%
                mutate(val=rank,
                       key="MethodA_Rank") %>%
                select(char, key, val))
long <- rbind(long,
              methodA %>%
                mutate(val=rel_freq,
                       key="methodA_Rel_Freq") %>%
                select(char, key, val))
long <- rbind(long,
              methodB %>%
                mutate(val=count,
                       key="MethodB_Count") %>%
                select(char, key, val))
long <- rbind(long,
              methodB %>%
                mutate(val=rank,
                       key="MethodB_Rank") %>%
                select(char, key, val))
long <- rbind(long,
              methodB %>%
                mutate(val=rel_freq,
                       key="methodB_Rel_Freq") %>%
                select(char, key, val))

wide <- long %>%
  spread(key,val,fill=0) %>%
  mutate(agree=ifelse(MethodA_Rank==MethodB_Rank & MethodA_Rank==Stamp_Rank,'Y','N')) %>%
  arrange(MethodA_Rank)

long2 <- rbind(stamp,methodA,methodB) %>%
  filter(rank <= 42)


#####################################################################
# how well do these methods fit Stamp?

# plot freq v rank for first 42 items
ggplot(long2, aes(x=log(rank),y=log(rel_freq), color=method)) +
  geom_point()

ggplot(long2, aes(x=rank,y=rel_freq, color=method)) +
  geom_point() +
  scale_y_continuous(trans='log') +
  scale_x_continuous(trans='log') 


# look at percent change from Stamp to our method
percent_change <- function(a,b) {
  return((b-a)/abs(a))
}

check <- wide %>% filter(MethodB_Count>Stamp_Count)

# measure the fit
methodA_fit <- wide %>%
  filter(Stamp_Count > 0) %>%
  mutate(error=percent_change(Stamp_Count, MethodA_Count),
         missing=Stamp_Count-MethodA_Count) %>%
  mutate(item="Method A") %>%
  select(char, item, error, missing)

# measure the fit
methodB_fit <- wide %>%
  filter(Stamp_Count > 0) %>%
  mutate(error=percent_change(Stamp_Count, MethodB_Count),
         missing=Stamp_Count-MethodB_Count) %>%
  mutate(item="Method B") %>%
  select(char, item, error, missing)

stamp_fit <- wide %>%
  filter(Stamp_Count > 0) %>%
  mutate(error=0,
         missing=0,
         item="Stamp & Le") %>%
  select(char, item, error, missing)

fit <- rbind(methodA_fit, methodB_fit, stamp_fit) 
total_missing <- fit %>%
  group_by(char) %>%
  summarise(total_missing=sum(missing))
fit <- fit %>% left_join(total_missing)

# plot how well things fit
ggplot(fit, aes(x=reorder(char,total_missing),y=missing, color=item, group=item)) +
  geom_point() +
  coord_flip() +
  scale_colour_manual(name=" ",
                    values = c("Stamp & Le"="#000000",
                               "Method A"="#ff0000",
                               "Method B"="#ffae00")) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  labs(x="Hamptonese Token", y="Missing")

method_missing <- fit %>%
  group_by(item) %>%
  summarise(missing=sum(missing))

# Method A missing 237, but includes 256 tokens missing from Stamp (493) 
# Method B missing 499 

# can I match with Method C?

# no, I think Stamp's table contains errors



# ok 

