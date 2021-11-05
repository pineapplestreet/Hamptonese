# libraries I'm using
library(dplyr)

# grab our psalms broken into words
psalms_words <- read.csv("round3 - Markov/python_output_psalms_words.csv")

# my goal here is to create a "religious text" that is objectively nonsense
# I will do that by generating a corpus the same size as psalms using a simplified markov model

#################################################################################
# step 1 - find p(B|A)

# get counts for each A
word_freq <- psalms_words %>% 
  mutate(A=char) %>%
  group_by(A) %>% 
  summarise(total_freq=n())

# get the next word, but only when they appear in the same line
next_word <- psalms_words %>%
  mutate(next_char=lead(char),
         next_line_id=lead(line_id)) %>%
  filter(line_id==next_line_id)         

# find p(B|A)
prob_b_given_a <- next_word %>%
  mutate(A=char,
         B=next_char) %>%
  group_by(A,B) %>%
  summarise(pair_freq=n()) %>%
  left_join(word_freq) %>%
  mutate(prob_b_given_a=pair_freq/total_freq) %>%
  select(A,B,prob_b_given_a)

#################################################################################
# step 2 - sanity check

# sanity check
check <- prob_b_given_a %>%
  filter(A=="a")
sum(check$prob_b_given_a)
check <- prob_b_given_a %>%
  filter(A=="psalm")
sum(check$prob_b_given_a)

# the word "psalm" only has a total p of 0.5 and only appears with "and"?
check <- next_word %>%
  filter(char=="psalm")

# only one appearance?
check <- psalms_words %>%
  filter(char=="psalm")

# appear twice, once at the end of a line. Hence 0.5 p. 

# what is the total p per A word?
check <- prob_b_given_a %>%
  group_by(A) %>%
  summarise(choices=n(),
            total_p=sum(prob_b_given_a))
nrow(check)
nrow(check %>% filter(total_p==1))

#################################################################################
# step 3 - defining Markov logic

# nearly half of all words only have one choice
# this will break the Markov and result in identical strings of text
# to avoid this in cases where there is only 1 choice, will will select from all

# starting with the simple probability of each word
markov_logic <- word_freq %>%
  mutate(p=total_freq/sum(word_freq$total_freq))
sum(markov_logic$p)
  
# mark items that have no choices
# method p(A) - when we have no choices to pick from, pick word based on p(A)
# method p(B|A) - when we have 2+ choices to pick from, pick word based on p(B|A)
no_choices <- prob_b_given_a %>%
  group_by(A) %>%
  summarise(choices=n()) %>%
  mutate(method=ifelse(choices>1,'p(B|A)','p(A)')) %>%
  select(A, method)  
markov_logic <- markov_logic %>% left_join(no_choices) %>%
  mutate(method=ifelse(is.na(method),'p(A)',method)) # to handle words missing from prob_b_given_a 

# normalize prob_b_given_a based on total p for each word
total_p <- prob_b_given_a %>%
  group_by(A) %>%
  summarise(total_p=sum(prob_b_given_a))
norm_prob_b_given_a <- prob_b_given_a %>%
  left_join(total_p) %>%
  mutate(norm_p_b_given_a=prob_b_given_a/total_p)

# check all good? some rounding errors, no big deal...
check <- norm_prob_b_given_a %>%
  group_by(A) %>%
  summarise(total_norm_p=sum(norm_p_b_given_a)) %>%
  filter(total_norm_p>1)



######################################################################################
# step 4 - constructing the Book of Nonsense
# note: char_no is based on the lines in the transcription, so were not accurate here

# We want to generate the same number of psalms with the same number of lines and the
# same number of words per line. The "Book of Nonsense" will have the same "shape" 
# as the book of Psalms, but hopefully, will be meaningless

# We will start each line with the same word used in Psalms; this serves as our 
# not-quite random seed 

template <- psalms_words %>%
  group_by(psalm_no, line_no, line_id) %>%
  summarise(first_word=first(char),
            word_count=n()) %>%
  ungroup()


it=0
book_of_nonsense <- data.frame()
# lazy loopy code written quick and dirty
for(row in 1:nrow(template)) {
  it = it+1
  
  # get our template
  my_template <- template[row,]
  starting_word <- my_template$first_word
  desired_word_count <- my_template$word_count  
  
  # start our new line
  word_count = 1
  new_line = starting_word
  selected_word = starting_word
  
  # build a string with the desired number of words
  while(word_count < desired_word_count) {
    
    # get details about the currently selected word - which method do we use?
    word_count = word_count +1
    word_details = markov_logic %>% filter(A==selected_word)
    method = word_details$method

    # method p(A)
    if(method=="p(A)") {
      #print("method p(A)")
      next_word <- (sample_n(markov_logic, 1, replace = FALSE, weight = markov_logic$p))$A
      #print(next_word)
    }
    # method p(B|A) 
    else if(method=="p(B|A)") {
      #print("method p(B|A)")
      my_norm_prob_b_given_a <- norm_prob_b_given_a %>% filter(A==selected_word)
      next_word <- (sample_n(my_norm_prob_b_given_a, 1, replace = FALSE, weight = my_norm_prob_b_given_a$norm_prob_b_given_a))$B
      #print(next_word)      
    }
    
    # add the word to our line
    new_line <- paste0(new_line,' ',next_word)
    selected_word = next_word
  }
  
  # create our new row
  row <- data.frame(nonsense_no=my_template$psalm_no,
                    line_no=my_template$line_no,
                    line_id=my_template$line_id,
                    new_line=new_line)
  book_of_nonsense <- rbind(book_of_nonsense,row)  
  print(my_template)
  print(new_line)
}

# save the precious gospel for future generations
saveRDS(book_of_nonsense,"round3 - Markov/Book_of_Nonsense.Rds")
