# -*- coding: utf-8 -*-
"""
Created on Tue Apr  5 14:11:59 2022

@author: hofma
"""
import pandas as pd

#load
leq = pd.read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/LEQ_FU1_to_FU3_overall_percentile.csv", dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')
dawba = pd.read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_BL_to_FU3.csv", dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')
dl = pd.concat([dawba, leq], axis=1, join='inner', ignore_index=False).reset_index(drop = False) 

#Get Data 
datbl = pd.read_csv('/Users/hofma/Desktop/LGC/02_data/Crisis_BL.csv', dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')
datfu = pd.read_csv('/Users/hofma/Desktop/LGC/02_data/Crisis_FU.csv', dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')
datfu2 = pd.read_csv('/Users/hofma/Desktop/LGC/02_data/Crisis_FU2.csv', dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')
demo = pd.read_csv("/Users/hofma/Desktop/LGC/02_data/Crisis_bl_demo.txt", sep=",", names=["ID", "T_T1", "T_T2", "T_T3", "M_T1", "M_T2", "M_T3", "W_T1", "W_T2", "W_T3", "D31D", "RESIL", "Origin", "Age", "Employment", "Urbanicity", "Household", "Phy_Health", "Psy_Health", "gender", "Mood_M3"]).set_index("ID")

#merge
ab = dl.merge(datbl, left_on = ['ID'], right_on = ['ID'],  how='left', indicator=True).set_index('ID')
bc = datbl.merge(datfu, left_on = ['ID'], right_on = ['ID'],  how='left', indicator=True)
cd = datfu.merge(datfu2, left_on = ['ID'], right_on = ['ID'],  how='left', indicator=True)

a = ab[["_merge"]]
b = bc[["_merge"]]
c = cd[["_merge"]]

#recode for anaylsis 
def x_recode(_merge):
    if _merge == "left_only":
        return 1
    else:
        return 0

a['Dummy_A'] = a['_merge'].apply(x_recode)
b['Dummy_A'] = b['_merge'].apply(x_recode)
c['Dummy_A'] = c['_merge'].apply(x_recode)

#check
a['Dummy_A'].head()
b['Dummy_A'].head()
c['Dummy_A'].head()

#save to R for Mulivariate Attrition Analysis 
a.to_csv(path_or_buf = '/Users/hofma/Desktop/LGC/02_data/FU3COVBL.csv', index = True, sep=';')
b.to_csv(path_or_buf = '/Users/hofma/Desktop/LGC/02_data/COVBLFU.csv', index = True, sep=';')
c.to_csv(path_or_buf = '/Users/hofma/Desktop/LGC/02_data/COVFUFU2.csv', index = True, sep=';')
