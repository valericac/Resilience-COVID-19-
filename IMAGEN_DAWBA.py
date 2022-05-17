#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Sep 24 12:40:09 2021
@author: valerie_hofmann
"""

##Import Packages 
import pandas as pd

dawba_bl = pd.read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_CFA.csv", dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')
dawba_fu1 = pd.read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_FU1_CFA.csv", dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')
dawba_fu2 = pd.read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_FU2_CFA.csv", dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')
dawba_fu3 = pd.read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_FU3_CFA.csv", dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')

dawba = pd.concat([dawba_bl, dawba_fu1, dawba_fu2, dawba_fu3], axis=1, join='inner', ignore_index=False).reset_index(drop = False) 
len(dawba)

dawba.to_csv(path_or_buf = 'C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_BL_to_FU3.csv', index=False)
