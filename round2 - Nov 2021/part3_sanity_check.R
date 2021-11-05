# libraries I'm using
library(dplyr)
library(ggplot2)

# grab our data
hamptonese_clean <- readRDS("round2 - Nov 2021/hamptonese_clean.Rds")

# sanity checking myself...

# every revelation exists within one page, good.
check_pages <- hamptonese_clean %>%
  group_by(revelation) %>%
  summarise(page_nos=length(unique(page_no)))

# we see 2 pages with multiple revelations...
check_pages <- hamptonese_clean %>%
  group_by(page_no) %>%
  summarise(revelations=length(unique(revelation)))

# here are the two we saw before...
# https://www.cs.sjsu.edu/faculty/stamp/Hampton/jpegFiles/p158.jpg
#https://www.cs.sjsu.edu/faculty/stamp/Hampton/jpegFiles/p166.jpg

# This looks right to me. 



##########################################################################
# how many revelations do we have?
total_revelations <- length(unique(hamptonese_clean$revelation))
check <- data.frame(expected_revelations=seq(1,102,1))
check2 <- data.frame(found_revelations=unique(hamptonese_clean$revelation))
check %>% filter(!(expected_revelations %in% check2$found_revelations))

review <- hamptonese_clean %>%
  group_by(page_no) %>%
  summarise(revelation=max(revelation))

length(unique(hamptonese_clean$page_no))

# We have 100 pages
# We have 102 revelations includes the "double revelation" on pgs 158 & 166
