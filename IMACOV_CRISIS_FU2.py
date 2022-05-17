
# -*- coding: utf-8 -*-
"""
Created on Mon Jan 31 15:33:04 2022

@author: hofma
"""

##################
#crisis FU2
##################

# crisis IMACOV BL + FU + FU2  

#packages
import os
import pandas as pd
import seaborn as sns
from matplotlib import cm
vidris = cm.get_cmap("vidris")

#Set wd
os.chdir("/Users/hofma/Desktop/LGC/01_raw")

#crisis 
crisis_fu2 = pd.read_csv("IMACOV19-EXTRA_FU2.csv", dtype={"User code":"string",
                                                          'string_col': 'float16',
                                                          'int_col': 'float16'})
crisis_fu2.rename(columns={'User code': 'ID'}, inplace=True)
crisis_fu2.set_index('ID')
crisis_fu2_ID = crisis_fu2["ID"]
crisis_fu2_Language = crisis_fu2["Language"]

#Colnames
crisis_fu2.columns.tolist()

#Converting the Timestamp to exact age, timestamp is the numer of days 
crisis_fu2['age'] = crisis_fu2[["Completed Timestamp"]]/365

###########################################
# Check for Analysis 
###########################################
#Demographics 
demo_col = ['ID', 'BG1'] #Profession_Status (Sind Sie derzeit berufstätig?) 
demo = pd.DataFrame(crisis_fu2.loc[:,demo_col])
demo = demo.dropna(axis = 0)
len(demo)

#Mood Sates
mood_col = ['ID', 'Language', 'EW2W1', 'EW2W10', 'EW2W11', 'EW2W2', 'EW2W3', 'EW2W4', 'EW2W5', 'EW2W6', 'EW2W7', 'EW2W8', 'EW2W9']
mood = pd.DataFrame(crisis_fu2.loc[:,mood_col])
mood = mood.dropna(axis = 0)
len(mood)
mw = sns.heatmap(mood[['EW2W1', 'EW2W2', 'EW2W3', 'EW2W4', 'EW2W5', 'EW2W6', 'EW2W7', 'EW2W8', 'EW2W9', 'EW2W10', 'EW2W11']].corr(), annot=True)
mw
# recode EW2W2 and EW2W3 reverse items 

# Worries
worries_col = ['ID', 'Language', 'ES5_1', 'ES5_2', 'ES5_3', 'ES5_4', 'ES6', 'ES7']
worries = pd.DataFrame(crisis_fu2.loc[:,worries_col])
worries = worries.dropna(axis = 0)
len(mood)
w = sns.heatmap(worries[['ES5_1', 'ES5_2', 'ES5_3', 'ES5_4', 'ES6', 'ES7']].corr(), annot=True)
w
# recode ES7 reverse Item 

#Lifestyle Changes 
life_col = ['ID', 'Language', 'LC2', 'LC3', 'LC4', 'LC5', 'LC6', 'LC7', 'LC8', 'LC9', 'LC10', 'LC11', 'LC12', 'LC13', 'LC14']
life = pd.DataFrame(crisis_fu2.loc[:,life_col])
life = life.dropna(axis = 0)
len(life)
l = sns.heatmap(life[['LC2', 'LC3', 'LC4', 'LC5', 'LC6', 'LC7', 'LC8', 'LC9', 'LC10', 'LC11', 'LC12', 'LC13', 'LC14']].corr(), annot=True)
l 

########
# Recode 
########
def reverseScoring(df, high, cols):
    df[cols] = high - df[cols]
    return df
#The Columns to be reversed
cols_m = ['EW2W2', 'EW2W3']
crisis_fu2 = reverseScoring(crisis_fu2, 4, cols_m)
cols_w = ['ES7']
crisis_fu2 = reverseScoring(crisis_fu2, 3, cols_w)


mw = sns.heatmap(crisis_fu2[['EW2W1', 'EW2W2', 'EW2W3', 'EW2W4', 'EW2W5', 'EW2W6', 'EW2W7', 'EW2W8', 'EW2W9', 'EW2W10', 'EW2W11']].corr(), annot=True, cmap=vidris)
mw
w = sns.heatmap(crisis_fu2[['ES5_1', 'ES5_2', 'ES5_3', 'ES5_4', 'ES6', 'ES7']].corr(), annot=True,  cmap=vidris)
w
######################################
#Final Set of Data 
#######################################
# NO Geodata 

#crisis_fu2
dat_fu2_colnames = ['ID', 'Language', "Completed Timestamp", 'age', 'ACC1', 'ACC2', 'BG1', 'EW2W1', 'EW2W10', 'EW2W11', 'EW2W2', 'EW2W3', 'EW2W4', 'EW2W5', 'EW2W6', 'EW2W7', 'EW2W8', 'EW2W9', 'ES5_1', 'ES5_2', 'ES5_3', 'ES5_4', 'ES6', 'ES7', 'LC2', 'LC3', 'LC4', 'LC5', 'LC6', 'LC7', 'LC8', 'LC9', 'LC10', 'LC11', 'LC12', 'LC13', 'LC14']
dat_fu2 = pd.DataFrame(crisis_fu2.loc[:,dat_fu2_colnames])
len(dat_fu2)

dat_fu2.to_csv(path_or_buf = '/Users/hofma/Desktop/LGC/02_data/Crisis_FU2.csv', index=False)


