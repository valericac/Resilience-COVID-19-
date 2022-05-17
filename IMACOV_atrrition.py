# -*- coding: utf-8 -*-
"""
Created on Tue Feb 22 11:30:23 2022

@author: hofma
"""

#packages
import pandas as pd

#Check where the missings are coming from 
dawbaleq = pd.read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_LEQ_res.txt", dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')

datbl = pd.read_csv('/Users/hofma/Desktop/LGC/02_data/Crisis_BL.csv', dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')
datfu = pd.read_csv('/Users/hofma/Desktop/LGC/02_data/Crisis_FU.csv', dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')
datfu2 = pd.read_csv('/Users/hofma/Desktop/LGC/02_data/Crisis_FU2.csv', dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')

crisis_m = pd.concat([dawbaleq, datbl], axis=1, join='inner', ignore_index=False) 
crisis_m_fu = pd.concat([crisis_m, datfu], axis=1, join='inner', ignore_index=False) 
crisis_m_fu2 = pd.concat([crisis_m, datfu2], axis=1, join='inner', ignore_index=False) 

#
datbl.columns.tolist()
demo = datbl[['Language', 'age', 'BG1', 'BG2', 'BG3', 'BG6', 'BG11']]
demo.columns = [""]