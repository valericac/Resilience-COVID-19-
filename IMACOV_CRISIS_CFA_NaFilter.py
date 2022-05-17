# -*- coding: utf-8 -*-
"""
Created on Thu Feb 24 11:14:54 2022

@author: hofma
"""
#packages
import pandas as pd

datbl = pd.read_csv('/Users/hofma/Desktop/LGC/02_data/Crisis_BL.csv', dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')
datfu = pd.read_csv('/Users/hofma/Desktop/LGC/02_data/Crisis_FU.csv', dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')
datfu2 = pd.read_csv('/Users/hofma/Desktop/LGC/02_data/Crisis_FU2.csv', dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')

#########################
#####75% dataframe 
#For the sake of checking how many datasets remain if only 75 of all data is present 
crisis_n = pd.concat([datbl, datfu, datfu2], axis=1, join='outer', ignore_index=False)
crisis_n.columns.tolist()
perc = 75.0 
crisis_n.columns = ['L_T1', 'T_T1', 'Y_T1', 'A1_T1','A2_T1','geoLoc_search','BG1_T1','BG2_T1','BG3_T1','BG6_T1','BG11_T1','M1_T1','M10_T1','M11_T1', 'M2_T1', 'M3_T1', 'M4_T1', 'M5_T1', 'M6_T1', 'M7_T1', 'M8_T1', 'M9_T1','W1_T1', 'W2_T1', 'W3_T1', 'W4_T1', 'W5_T1', 'W6_T1', 'L2_T1', 'L3_T1', 'L4_T1', 'L5_T1', 'L6_T1', 'L7_T1', 'L8_T1', 'L9_T1', 'L10_T1', 'L11_T1', 'L12_T1', 'L13_T1', 'L14_T1', 'L_T2', 'T_T2', 'Y_T2', 'A1_T2', 'A2_T2', 'BG1_T2', 'M1_T2', 'M10_T2', 'M11_T2', 'M2_T2', 'M3_T2', 'M4_T2', 'M5_T2', 'M6_T2', 'M7_T2', 'M8_T2', 'M9_T2', 'W1_T2', 'W2_T2', 'W3_T2', 'W4_T2', 'W5_T2', 'W6_T2', 'LC2_T2', 'LC3_T2', 'LC4_T2', 'LC5_T2', 'LC6_T2', 'LC7_T2', 'LC8_T2', 'LC9_T2', 'LC10_T2', 'LC11_T2', 'LC12_T2', 'LC13_T2', 'LC14_T2', 'L_T3', 'T_T3', 'Y_T3', 'A1_T3', 'A2_T3', 'BG1_T3', 'M1_T3', 'M10_T3', 'M11_T3', 'M2_T3', 'M3_T3', 'M4_T3', 'M5_T3', 'M6_T3', 'M7_T3', 'M8_T3', 'M9_T3', 'W1_T3', 'W2_T3', 'W3_T3', 'W4_T3', 'W5_T3', 'W6_T3', 'L2_T3', 'L3_T3', 'L4_T3', 'L5_T3', 'L6_T3', 'L7_T3', 'L8_T3', 'L9_T3', 'L10_T3', 'L11_T3', 'L12_T3', 'L13_T3', 'L14_T3']
min_count =  int(((100-perc)/100)*crisis_n.shape[1] + 1) 
#
crisis_75 = crisis_n.dropna(axis=0,thresh=min_count, subset =['M1_T1','M10_T1','M11_T1', 'M2_T1', 'M3_T1', 'M4_T1', 'M5_T1', 'M6_T1', 'M7_T1', 'M8_T1', 'M9_T1', 'M1_T2', 'M10_T2', 'M11_T2', 'M2_T2', 'M3_T2', 'M4_T2', 'M5_T2', 'M6_T2', 'M7_T2', 'M8_T2', 'M9_T2', 'M1_T3', 'M10_T3', 'M11_T3', 'M2_T3', 'M3_T3', 'M4_T3', 'M5_T3', 'M6_T3', 'M7_T3', 'M8_T3', 'M9_T3']) #75% non missing
len(crisis_75) 
crisis_75.mean()

###########################
# Overlap to Res Score
###########################
#Check for overlap of Data from FU3 and 75% Data 
leq = pd.read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/LEQ_FU1_to_FU3_overall_percentile.csv", dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')
dawba = pd.read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_BL_to_FU3.csv", dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')

crisis_dawba_leq = pd.concat([crisis_75, dawba, leq], axis=1, join='inner', ignore_index=False).reset_index(drop = False) 
len(crisis_dawba_leq)


#Write CSV of 75% 
crisis_75 = crisis_75.reset_index(drop=False)
crisis_75['ID'] = crisis_75['ID'].astype(str)
print(crisis_75.head())

#csv
crisis_75.to_csv(path_or_buf = '/Users/hofma/Desktop/LGC/02_data/Crisis_75.csv', index = False)
#safe with different sep for R 
crisis_75.to_csv(path_or_buf = '/Users/hofma/Desktop/LGC/02_data/Crisis_75_sep.csv', index = False, sep=';')

