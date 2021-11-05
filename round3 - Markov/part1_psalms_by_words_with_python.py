#!/usr/bin/env python3# -*- coding: utf-8 -*-"""@author: joshpauseFound the Book of Psalms via Gutenburg:     https://www.gutenberg.org/cache/epub/8019/pg8019.txt    Massage into a useable dataframeSame as what I did in Round 2, but this time I'm breaking into words, not charactersKeeping the column naming convention the same so things fit into the same functions later"""# libraries usedimport reimport pandas as pd# start with modified text# based on https://www.gutenberg.org/cache/epub/8019/pg8019.txt# Includes some formatting fixes I added to make this part easiermy_file = open("/Users/joshpause/Desktop/Experiments/Hamptonese/source_materials/Psalms.txt", "r")content_list = my_file.readlines()# forgive some ugly code, but it gets 'er doneiteration = 0clean_data = pd.DataFrame()for row in content_list:    iteration += 1        # deal with book and line numbering    if '<<' in row:                # get the psalm and line        x = re.search('<<psalm-([0-9]{3})-([0-9]{3})>> ', row)        if x:            psalm_no = x.group(1)            psalm_line_no = x.group(2)             line_id = str(int(psalm_no))+"_"+str(int(psalm_line_no))    # cleanup each line    if len(row) > 1:        row = re.sub('<<.*?>>', '', row)        row = row.strip()        row = re.sub(r'[^\w\s]', '', row)        row = row.lower()                 char_no = 0        print(row)                                # cycle through all characters in this row        chars = row.split(' ')                          for char in chars:            print(char)            char_no += 1            row = pd.DataFrame({'psalm_no':[int(psalm_no)],                                'line_no':[int(psalm_line_no)],                                                                'line_id':[line_id],                                'char_no':[int(char_no)],                                'char':[char]})               clean_data = clean_data.append(row)                                                                   # save as a CSV for easy sharingclean_data.to_csv("/Users/joshpause/Desktop/Experiments/Hamptonese/round3 - Markov/python_output_psalms_words.csv",index=False)                