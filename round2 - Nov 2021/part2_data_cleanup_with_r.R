# libraries I'm using
library(dplyr)
library(stringr)

# start with the output from the Python script
hamptonese <- read.csv("round2 - Nov 2021/python_output.csv")

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
hamp_key <- read.csv("source_materials/HampKey.csv")
unknown_characters <- hamptonese %>%
  left_join(hamp_key, by=c("methodA"="char")) %>%
  filter(is.na(weight)) %>%
  group_by(methodA) %>%
  summarise(count=n()) %>%
  filter(methodA!="*") %>%
  filter(!str_detect(methodA, "\\["))

# the transcription contains characters not defined in the official key
# treat these as unknown in methodB
hamptonese <- hamptonese %>%
  mutate(methodB=ifelse(methodB %in% unknown_characters$methodA,"*",methodB))  

# for the sake of being conservative I am going with method B
# ignoring any characters not in the official key
# ignoring any characters preceded by "#" (where Le was unsure)

# save the cleaned-up dataframe as an Rds for easy work in R
hamptonese_clean <- hamptonese %>%
  mutate(char=methodB) %>%
  mutate(line_id=paste0(revelation,'_',line_no)) %>%
  select(page_no,
         hampton_page_no,
         line_no,
         line_id,
         char_no,
         revelation,
         char) 

# save as RDS for future exploration
saveRDS(hamptonese_clean,"round2 - Nov 2021/hamptonese_clean.Rds")

