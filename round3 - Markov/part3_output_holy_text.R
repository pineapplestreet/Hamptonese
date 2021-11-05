# libraries I'm using
library(dplyr)
library(stringr)

book_of_nonsense <- readRDS("round3 - Markov/Book_of_Nonsense.Rds")
#write.csv(book_of_nonsense,"round3 - Markov/Book_of_Nonsense.csv")

book_of_nonsense <- book_of_nonsense %>%
  mutate(pretty=str_replace(line_id,"_",":"))

nonsense_no = 0
output_text = ""
for(i in 1:nrow(book_of_nonsense)) {
  line = book_of_nonsense[i,]
  if(line$nonsense_no!=nonsense_no) {
    output_text = paste0(output_text, "\n\nNonsense #",line$nonsense_no)
    nonsense_no=line$nonsense_no
  }
  output_text = paste0(output_text,"\n",line$pretty," ",line$new_line)
  print(paste0(line$pretty," ",line$new_line))
}

# save as txt
fileConn<-file("round3 - Markov/Book_of_Nonsense.txt")
writeLines(output_text, fileConn)
close(fileConn)
