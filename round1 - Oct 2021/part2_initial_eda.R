# libraries I'm using
library(dplyr)
library(stringr)

# the full transcription by Stamp & Le
# one character per row, with various indexes for where it appears
hamptonese <- read.csv("HampTrans.csv")

# most common characters (starting with 126 distinct tokens)
alphabet <- hamptonese %>%
  group_by(char) %>%
  summarise(count=n())

# includes a few fragments of English e.g. [Jesus] and [Christ]
# all English is encapsulated with square brackets
hamptonese <- hamptonese %>%
  mutate(english=ifelse(str_detect(char, "\\["),'Y','N'))

# the * represents an indecipherable character

# Le was unsure of anything preceded by pound sign (#) 
hamptonese <- hamptonese %>%
  mutate(unsure=ifelse(str_detect(char, "#"),'Y','N'))

# We can treat these as their best guess (Method A) or as unknown (Method B)
hamptonese <- hamptonese %>%
  mutate(methodA=ifelse(unsure=='Y',str_replace(char,"#",""),char)) %>%
  mutate(methodB=ifelse(unsure=='Y',"*",char))

# compare the transcription to the official key
# http://www.cs.sjsu.edu/faculty/stamp/Hampton/Hamptonese/Hamptonese.html
hamp_key <- read.csv("HampKey.csv")
unknown_characters <- hamptonese %>%
  left_join(hamp_key, by=c("methodA"="char")) %>%
  filter(is.na(weight)) %>%
  group_by(methodA) %>%
  summarise(count=n()) %>%
  filter(methodA!="*") %>%
  filter(!str_detect(methodA, "\\["))

# the transcription contains characters not defined in the official key
# treat these as unknown in both methodA and methodB
hamptonese <- hamptonese %>%
  mutate(methodA=ifelse(methodA %in% unknown_characters$methodA,"*",methodA)) %>%
  mutate(methodB=ifelse(methodB %in% unknown_characters$methodA,"*",methodB))  


# let's look at distribution of our characters, methodA & methodB
alphabetA <- hamptonese %>%
  group_by(methodA) %>%
  summarise(count=n())
alphabetB <- hamptonese %>%
  group_by(methodB) %>%
  summarise(count=n())

# we are left with 56 unique characters, including * and the english words

# save as RDS for future exploration
saveRDS(hamptonese,"hamptonese.Rds")

