# libraries I'm using
library(dplyr)
library(ggplot2)

# grab our data
hamptonese <- readRDS("hamptonese.Rds")

# the page numbering does not agree
check_pages <- hamptonese %>%
  group_by(hampton_page_no) %>%
  summarise(page_nos=length(unique(page_no)))

# do hampton pages 2, 12, 13, 14 appear on more than 1 scanned page?
dupe_pages <- hamptonese %>%
  filter(hampton_page_no %in% c(2,12,13,14)) %>%
  group_by(hampton_page_no,page_no) %>%
  summarise(records=n())


# Hampton used page number 2, and 16, 2 causing the confusion
# although both are hampton_page_no==2 they are clearly different
# http://www.cs.sjsu.edu/faculty/stamp/Hampton/jpegFiles/p65.jpg
# http://www.cs.sjsu.edu/faculty/stamp/Hampton/jpegFiles/p75.jpg

# Hampon had two pages he numbered 12
# although both are hampton_page_no==12 they are clearly different
#https://www.cs.sjsu.edu/faculty/stamp/Hampton/jpegFiles/p69.jpg
#https://www.cs.sjsu.edu/faculty/stamp/Hampton/jpegFiles/p78.jpg

# Hampon had two pages he numbered 13
# although both are hampton_page_no==13 they are clearly different
#https://www.cs.sjsu.edu/faculty/stamp/Hampton/jpegFiles/p68.jpg
#https://www.cs.sjsu.edu/faculty/stamp/Hampton/jpegFiles/p79.jpg

# Hampon had two pages he numbered 14
# although both are hampton_page_no==14 they are clearly different
# they appear slightly more similar - the "2OOO" pattern appears in both
#https://www.cs.sjsu.edu/faculty/stamp/Hampton/jpegFiles/p67.jpg
#https://www.cs.sjsu.edu/faculty/stamp/Hampton/jpegFiles/p80.jpg

# sanity checking myself...

# every revelation exists on one page
check_pages <- hamptonese %>%
  group_by(revelation) %>%
  summarise(page_nos=length(unique(page_no)))

# we see 4 pages with multiple revelations...
check_pages <- hamptonese %>%
  group_by(page_no) %>%
  summarise(revelations=length(unique(revelation)))

# here are the two we saw before...
# https://www.cs.sjsu.edu/faculty/stamp/Hampton/jpegFiles/p158.jpg
#https://www.cs.sjsu.edu/faculty/stamp/Hampton/jpegFiles/p166.jpg

# page_no 136 and 152 do not appear to have extra revelations
# checked original TXT transcript by Stamp & Le to confirm
# the python must have messed up
sanity_check <- hamptonese %>%
  filter(page_no==136)

# nope, the Python was fine, found 2 errors in the TXT transcripts which I fixed