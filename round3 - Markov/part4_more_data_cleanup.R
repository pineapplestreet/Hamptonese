# now that we have all our needed data, clean it up so we can go back to analysis

# libraries I'm using
library(dplyr)
library(tidyr)
library(stringr)

# grab our psalms broken into words
psalms_words <- read.csv("round3 - Markov/python_output_psalms_words.csv")
saveRDS(psalms_words,"round3 - Markov/data_psalms_words.Rds")

# break nonsense up by word
book_of_nonsense <- readRDS("round3 - Markov/Book_of_Nonsense.Rds")
data_nonsense_words <- book_of_nonsense %>%
  mutate(char = strsplit(as.character(new_line), " ")) %>% 
  unnest(char) %>%
  select(-new_line)
saveRDS(data_nonsense_words,"round3 - Markov/data_nonsense_words.Rds")


# break nonsense by character
data_nonsense_characters <- book_of_nonsense %>%
  mutate(new_line=str_replace_all(new_line," ","_")) %>%
  mutate(char = strsplit(as.character(new_line), "")) %>% 
  unnest(char) %>%
  select(-new_line)
saveRDS(data_nonsense_words,"round3 - Markov/data_nonsense_characters.Rds")