#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Sep 24 12:40:09 2021
@author: valerie_hofmann
"""

##Import Packages 
import os
import pandas as pd
import numpy as np 
import seaborn as sns

#Set wd
os.chdir("C:/Users/hofma/Desktop/FRESHMO/01_raw")

#Load Dataset
dat = pd.read_csv("IMAGEN_dawba_FU3.tsv", sep="\t",  index_col = False,               
                  dtype={"PSC2":"string",
                             'string_col': 'float16',
                             'int_col': 'float16'})
dat.rename(columns={'PSC2': 'ID'}, inplace=True)
dat.set_index('ID')
DAWBA_ID = dat["ID"]

#dat.columns.tolist()

###DAWBA

#specific fear 
sf_colnames = ["sb1a", "sb1b", "sb1c", "sb1d", "sb1e", "sb1f", "sb1g", "sb1h", "sb1i", "sb1j", "sb1k", "sb1l", "sb1m"] 
dat["specific_fear_SUM"] = dat.loc[:,sf_colnames].sum(axis=1)  
len(sf_colnames)    
#range 0, 1, 2
specific_fear_max = len(sf_colnames)*2
dat["DAWBA_Specific_Fear_PERCENTILE"] = dat["specific_fear_SUM"] / specific_fear_max

#specific phobia
sp_colnames = ['sb2', 'sb3',  'sb4', 'sb5', 'sb6', 'sb7', 'sb8', 'sb9', 'sb10',
# 'sb11'
 ]
len(sp_colnames)
#sb11 exclude as the item is concearned with other peoples burden
dat["specific_phobia_SUM"] = dat.loc[:,sp_colnames].sum(axis=1) 
#range 0-2, sb11, sb6 = 0-3
specific_phobia_max = (8*2)+(3)
dat["DAWBA_Specific_Phobia_PERCENTILE"] = dat["specific_phobia_SUM"] / specific_phobia_max
#check 
dat["DAWBA_Specific_Phobia_PERCENTILE"].describe()


#social_anxiety 
#sc3, 4, and 5 excluded as they address children, talking about key adults 
sa_colnames = ['sc1', 'sc2a', 'sc2b', 'sc2c', 'sc2d', 'sc2e', 'sc2f', 'sc6', 'sc7', 'sc8',
# 'sc9', 
'sc10a', 'sc10b', 'sc10c', 'sc11', 'sc12', 'sc13', 'sc14', 'sc15', 'sc16',
# 'sc17'
]

len(sa_colnames)
#take out sc17 as it refers to the burden of others 
dat["Social_Anxiety_SUM"] = dat.loc[:,sa_colnames].sum(axis=1)
dat.loc[:,sa_colnames].max(axis=0)
dat['sc7'].describe() #conditional 
#Why is the max of sc12 3, documentation said it's 2, how long
dat['sc9'].describe() 
#"How old were you when" open- sc9 take out, no direct symptom
#ranges
#sc12 = 3 10a,b,c = 0-1 
Social_Anxiety_max = 38
#NaN never equals itself so replace by random value 
dat['sc7'] = dat['sc7'].replace(np.nan, 15)
#Create the percentage score in accordance with the number of items answered
dat.loc[dat['sc7'] == 15, 'DAWBA_Social_Anxiety_PERCENTILE'] =  (dat["Social_Anxiety_SUM"] / (Social_Anxiety_max-1))
dat.loc[dat['sc7'] < 15, 'DAWBA_Social_Anxiety_PERCENTILE'] =  (dat["Social_Anxiety_SUM"] / (Social_Anxiety_max))
#check 
dat["DAWBA_Social_Anxiety_PERCENTILE"].describe()
#replace np.na by 0 for CFA 
dat['sc7'] = dat['sc7'].replace(15, 0)
dat['sc7'].describe()

#Panic Disorder includes Agoraphobia (sd4b, sd4c, sd4d, sd5)
#no conditional questions
pd_colnames = ['sd1', 'sd2a', 'sd2b', 'sd2c', 'sd3a', 'sd3b', 'sd3c', 'sd3d', 'sd3e', 'sd3f', 'sd3g', 'sd3h', 'sd3i', 'sd3j', 'sd3k', 'sd3l', 'sd3m', 'sd3n', 'sd4a', 'sd4b', 'sd4c', 'sd4d', 'sd5', 'sd6', 'sd7a', 'sd7b', 'sd7c', 'sd7d',
# 'sd8'
 ]
len(pd_colnames)
#Exclude sd8 = burden of others 
dat["Panic_Disorder_SUM"] = dat.loc[:,pd_colnames].sum(axis=1)
dat.loc[:,pd_colnames].max(axis=0) 
Panic_Disorder_max = ((len(pd_colnames)-5) + 5*3) 
dat["DAWBA_Panic_Disorder_PERCENTILE"] = dat["Panic_Disorder_SUM"] / Panic_Disorder_max
#check 
dat["DAWBA_Panic_Disorder_PERCENTILE"].describe()

#PTSD
#no conditional questions
ptsd_colnames = ['se1', 'se2a', 'se2b', 'se2c', 'se2d', 'se2e', 'se2f', 'se2g', 'se2h', 'se2i', 'se2j', 'se2k', 'se3', 'se3a', 'se4a', 'se4b', 'se4c', 'se4d', 'se4e', 'se4f', 'se4g', 'se4h', 'se4i', 'se4j', 'se4k', 'se4l', 'se4m', 'se4n', 'se4o', 'se5', 'se6', 'se7', 'se8a', 'se8b', 'se8c', 'se8d',
# 'se9'
 ]
dat["PTSD_SUM"] = dat.loc[:,ptsd_colnames].sum(axis=1)
dat.loc[:,ptsd_colnames].max(axis=0)
len(ptsd_colnames)
#range 0-1 bis se3a, 0-2 bis se4o, 0-1 se5, 0-3 se8d
#se9 out burden of others
PTSD_max = 14 + 17*2 + 5*3 
dat["DAWBA_PTSD_PERCENTILE"] = dat["PTSD_SUM"] / PTSD_max
#check 
dat["DAWBA_PTSD_PERCENTILE"].describe()

#OCD
#no conditional questions 
ocd_colnames = ['sf1', 'sf2a', 'sf2b', 'sf2c', 'sf2d', 'sf2e', 'sf2f', 'sf2g', 'sf3', 'sf4',
# 'sf6',
 'sf7', 'sf8', 'sf9', 'sf10', 'sf11', 'sf12a', 'sf12b', 'sf12c', 'sf12d',
# 'sf13'
]
dat["OCD_SUM"] = dat.loc[:,ocd_colnames].sum(axis=1)
dat.loc[:,ocd_colnames].max(axis=0)
len(ocd_colnames)
#sf6 hat NaN als max inspect - Is this obsession about something terrible happening to yourself or to others just one part of a general concern about being separated from your key attachment figures, or is it a problem in its own right?
dat['sf6'].describe() #empty exclude
#range: sf1 0-1, 0-2 sf2a-sf6, 0-1 sf7-sf8 (but range goes to 2, so working with it, sme applies to sf1)
#sf10 too here should a 2 be the highest value wording "do you resist the rituals or obsessions"
#sf13 too "Have the rituals or obessions made it harder for those around you..." similar logic as PTSD exclude
OCD_max = 12*2 + 6*3 
dat["DAWBA_OCD_PERCENTILE"] = dat["OCD_SUM"] / OCD_max
#check 
dat["DAWBA_OCD_PERCENTILE"].describe()

#generalized anxiety 
#no conditional questions 

ga_colnames = ['sg2', 'sg2a', 'sg3', 'sg4a', 'sg4b', 'sg4c', 'sg4d', 'sg4e', 'sg4f', 'sg4g', 'sg4h', 'sg4i', 'sg4j', 'sg4k', 'sg6', 'sg7', 'sg8a', 'sg8b', 'sg8c', 'sg8d', 'sg8e', 'sg8f', 'sg9', 'sg10a', 'sg10b', 'sg10c', 'sg10d',
 #'sg11'
 ]
len(ga_colnames)
dat["Generalized_Anxiety_SUM"] = dat.loc[:,ga_colnames].sum(axis=1)
dat.loc[:,ga_colnames].max(axis=0)
#exclude sg11 other peoples burden
Gerneralized_Anxiety_max = 16*2 + 6*3 + 2*1
dat["DAWBA_Generalized_Anxiety_PERCENTILE"] = dat["Generalized_Anxiety_SUM"] / Gerneralized_Anxiety_max
#check 
dat["DAWBA_Generalized_Anxiety_PERCENTILE"].describe()

#Depression 
#conditional questions
MDD_colnames = ['sh1', 'sh2', 'sh3', 'sh4', 'sh5',
# 'sh7',
# 'sh8',
# 'sh9',
# 'sh10',
# 'sh11',
 'sh13', 'sh14', 'sh15', 'sh16', 'sh17', 'sh18a', 'sh18b', 'sh18c', 'sh18d', 'sh18e', 'sh18f', 'sh18g',  'sh18h', 'sh18i', 'sh18j', 'sh18k', 'sh18l', 'sh19', 'sh20a', 'sh20b', 'sh20c', 'sh20d',
  #'sh21'
 ]
#Exclude sh21 burden of others 
len(MDD_colnames)
dat["MDD_SUM"] = dat.loc[:,MDD_colnames].sum(axis=1)
dat.loc[:,MDD_colnames].max(axis=0)
dat.loc[:,MDD_colnames].max(axis=0).value_counts()
len(MDD_colnames)
mdd_scores = (1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3)
MDD_max = sum(mdd_scores)
#Skip rules: sh7 = 0 (3 questions skipped), sh13 = 0 (four questions skipped)
#Seems like sh7 does not contain data, check for sh8, sh9, sh10 same pattern exclude 
#dat['sh7'].describe()
#dat['sh8'].describe()
#dat['sh9'].describe()
#dat['sh10'].describe()
#dat['sh11'].describe()

#check sh13, data exists remains 
#dat['sh13'].describe()
#dat['na'] = dat['sh13'].isna() 
#dat['na'].describe()
#dat['sh13'] = dat['sh13'].replace(np.nan, 15) #NaN never equals itself, convert to 0 NaN and 0 both trigger the skipp

#Create the percentage score in accordance with the number of items answered
dat.loc[dat['sh13'] == 15, 'DAWBA_MDD_PERCENTILE'] =  (dat['MDD_SUM'] / (MDD_max-4))
dat.loc[dat['sh13'] < 15, 'DAWBA_MDD_PERCENTILE'] =  (dat['MDD_SUM'] / (MDD_max))
#check
dat['DAWBA_MDD_PERCENTILE'].describe()




#Eating Disorder 
#Skip Rules 
ed_colnames = ['sp1a', 'sp1b', 'sp1c', 'sp1d', 'sp1e',
# 'sp2a',
# 'sp2aexact',
# 'sp2b',
# 'sp2bexact',
# 'sp2c',
# 'sp2cexact',
# 'sp2d',
# 'sp2dexact',
# 'sp3',
# 'sp4',
# 'sp5',
# 'sp6',
# 'sp7',
 'sp8', 'sp9', 'sp10', 'sp11', 'sp12', 'sp13', 'sp14', 'sp15', 'sp16', 'sp17', 'sp18a', 'sp18b', 'sp18c', 'sp18d', 'sp18e', 'sp18f', 'sp18g', 'sp18h', 'sp19', 'sp20', 'sp21', 'sp22', 'sp26', 'sp27a', 'sp27b', 'sp27c', 'sp27d',
# 'sp28'
 ]
#SUM
dat["Eating_Disorder_SUM"] = dat.loc[:,ed_colnames].sum(axis=1)
len(ed_colnames)
#Max
#sp28 exclude, it's other peoples burden 
#exclude sp2a to sp7, concearned with assessment of weight and hight
dat.loc[:,ed_colnames].max(axis=0)
ed_score = (1,1,1,1,1,2,1,2,2,3,1,2,1,3,1,3,3,3,3,3,3,3,3,1,1,1,1,3,3,3,3,3)
ed_max = sum(ed_score)
len(ed_score)
#Different max for Skip rule

#NaN replacement for loop 
dat['sp8'] = dat['sp8'].replace(np.nan, .5)
dat['sp20'] = dat['sp20'].replace(np.nan, 15)
dat['sp26'] = dat['sp26'].replace(np.nan, 15)

def maxima(dat): 
    if (dat['sp8'] >= 1) and (dat['sp20'] < 15) and (dat['sp26'] < 15):
        return (ed_max-2)
    elif (dat['sp8'] >= 1) and (dat['sp20'] == 15) and (dat['sp26'] < 15): 
        return (ed_max-6)
    elif (dat['sp8'] >= 1) and (dat['sp20'] == 15) and (dat['sp26'] == 15): 
        return (ed_max-21)
    elif (dat['sp8'] >= 1) and (dat['sp20'] < 15) and (dat['sp26'] == 15):
        return (ed_max-17)
    elif (dat['sp8'] <= 2) and (dat['sp20'] == 15) and (dat['sp26'] < 15): 
        return (ed_max-3) 
    elif (dat['sp8'] <= 2) and (dat['sp20'] < 15) and (dat['sp26'] == 15):
        return (ed_max-15) 
    else: 
        return ed_max
    
dat['Eating_Disorder_max'] = dat.apply(maxima, axis = 1)
#check
#dat['Eating_Disorder_max'].describe()
dat['DAWBA_Eating_Disorder_PERCENTILE'] = (dat['Eating_Disorder_SUM'] / dat['Eating_Disorder_max'])
#check
dat['DAWBA_Eating_Disorder_PERCENTILE'].describe()

##check
#DAWBA_col= [col for col in dat.columns if '_PERCENTILE' in col]
#print(DAWBA_col)
 
## create final csv with relevant info; .4 
## create final dataframe only relevant
DAWBA_PERC = pd.DataFrame(dat[['DAWBA_Specific_Fear_PERCENTILE', 'DAWBA_Specific_Phobia_PERCENTILE', 'DAWBA_Social_Anxiety_PERCENTILE', 'DAWBA_Panic_Disorder_PERCENTILE', 'DAWBA_PTSD_PERCENTILE', 'DAWBA_OCD_PERCENTILE', 'DAWBA_Generalized_Anxiety_PERCENTILE', 'DAWBA_MDD_PERCENTILE','DAWBA_Eating_Disorder_PERCENTILE']])
DAWBA = pd.concat([DAWBA_ID, DAWBA_PERC], ignore_index = False, join = "inner", axis=1) 
#datX = dat.iloc[: , 44:]
#DAWBA_I = pd.concat([DAWBA_ID, datX], ignore_index = False, join = "inner", axis=1) 
## write csv
DAWBA.to_csv(path_or_buf = 'C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_FU3_percentile.csv', index=False)
#DAWBA_I.to_csv(path_or_buf = 'C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_items_percentile.csv', index=False)
sns.heatmap(DAWBA_PERC.corr(), annot=True);

#####################################################################
######Sum Score that just Sums all Items according to C. Wackerhagen 
int_colnames = ['sb1a', 'sb1b', 'sb1c', 'sb1d', 'sb1e', 'sb1f', 'sb1g', 'sb1h', 'sb1i', 'sb1j', 'sb1k', 'sb1l', 'sb1m', 'sb2', 'sb3', 'sb4', 'sb5', 'sb6', 'sb7', 'sb8', 'sb9', 'sb10', 'sc1', 'sc2a', 'sc2b', 'sc2c', 'sc2d', 'sc2e', 'sc2f', 'sc6', 'sc7', 'sc8', 'sd1', 'sd2a', 'sd2b', 'sd2c', 'sd3a', 'sd3b', 'sd3c', 'sd3d', 'sd3e', 'sd3f', 'sd3g', 'sd3h', 'sd3i', 'sd3j', 'sd3k', 'sd3l', 'sd3m', 'sd3n', 'sd4a', 'sd4b', 'sd4c', 'sd4d', 'sd5', 'sd6', 'sd7a', 'sd7b', 'sd7c', 'sd7d', 'se1', 'se2a', 'se2b', 'se2c', 'se2d', 'se2e', 'se2f', 'se2g', 'se2h', 'se2i', 'se2j', 'se2k', 'se3', 'se3a', 'se4a', 'se4b', 'se4c', 'se4d', 'se4e', 'se4f', 'se4g', 'se4h', 'se4i', 'se4j', 'se4k', 'se4l', 'se4m', 'se4n', 'se4o', 'se5', 'se6', 'se7', 'se8a', 'se8b', 'se8c', 'se8d', 'sf1', 'sf2a', 'sf2b', 'sf2c', 'sf2d', 'sf2e', 'sf2f', 'sf2g', 'sf3', 'sf4', 'sf7', 'sf8', 'sf9', 'sf10', 'sf11', 'sf12a', 'sf12b', 'sf12c', 'sf12d', 'sg2', 'sg2a', 'sg3', 'sg4a', 'sg4b', 'sg4c', 'sg4d', 'sg4e', 'sg4f', 'sg4g', 'sg4h', 'sg4i', 'sg4j', 'sg4k', 'sg6', 'sg7', 'sg8a', 'sg8b', 'sg8c', 'sg8d', 'sg8e', 'sg8f', 'sg9', 'sg10a', 'sg10b', 'sg10c', 'sg10d', 'sp1a', 'sp1b', 'sp1c', 'sp1d', 'sp1e', 'sp8', 'sp9', 'sp10', 'sp11', 'sp12', 'sp13', 'sp14', 'sp15', 'sp16', 'sp17', 'sp18a', 'sp18b', 'sp18c', 'sp18d', 'sp18e', 'sp18f', 'sp18g', 'sp18h', 'sp19', 'sp20', 'sp21', 'sp22', 'sp26', 'sp27a', 'sp27b', 'sp27c', 'sp27d'] 
intframe = dat[int_colnames]
int_max = [specific_fear_max + specific_phobia_max + Social_Anxiety_max + Panic_Disorder_max + OCD_max + Gerneralized_Anxiety_max + MDD_max ]
#Due to the Skip rules in the eating disorder section the maximum varies indidually - resulting sum has to be divided by indivdual maximum 
dat['DAWBA_INT_ITEMS_MAX'] = dat['Eating_Disorder_max'] +  int_max
dat["DAWBA_INTERNALIZING_SUM"] = dat.loc[:,int_colnames].sum(axis=1)
dat["DAWBA_INT_ITEMS_PERCENTILE"] = dat["DAWBA_INTERNALIZING_SUM"] / dat['DAWBA_INT_ITEMS_MAX']
#Create the percentage score in accordance with the number of items answered
dat.loc[dat['sh13'] == 15, 'DAWBA_INT_ITEMS_PERCENTILE'] =  (dat['DAWBA_INTERNALIZING_SUM'] / (dat['DAWBA_INT_ITEMS_MAX']-4))
dat.loc[dat['sh13'] < 15, 'DAWBA_INT_ITEMS_PERCENTILE'] =  (dat['DAWBA_INTERNALIZING_SUM'] / dat['DAWBA_INT_ITEMS_MAX'])
dat["DAWBA_INT_ITEMS_PERCENTILE"].describe()
#####.vs summing all subscale percentiles
dat["DAWBA_INT_SUBSCALES_PERCENTILE"] = (dat['DAWBA_Specific_Fear_PERCENTILE'] +  dat['DAWBA_Specific_Phobia_PERCENTILE'] + dat['DAWBA_Social_Anxiety_PERCENTILE'] +  dat['DAWBA_Panic_Disorder_PERCENTILE'] + dat['DAWBA_PTSD_PERCENTILE'] + dat['DAWBA_OCD_PERCENTILE'] + dat['DAWBA_Generalized_Anxiety_PERCENTILE'] + dat['DAWBA_MDD_PERCENTILE'] + dat['DAWBA_Eating_Disorder_PERCENTILE']) / 8
dat["DAWBA_INT_SUBSCALES_PERCENTILE"].describe()
###### All Percentages 
######Write .csv 
DAWBAX_PERC = pd.DataFrame(dat[['DAWBA_Specific_Fear_PERCENTILE', 'DAWBA_Specific_Phobia_PERCENTILE', 'DAWBA_Social_Anxiety_PERCENTILE', 'DAWBA_Panic_Disorder_PERCENTILE', 'DAWBA_PTSD_PERCENTILE', 'DAWBA_OCD_PERCENTILE', 'DAWBA_Generalized_Anxiety_PERCENTILE', 'DAWBA_MDD_PERCENTILE', 'DAWBA_Eating_Disorder_PERCENTILE', "DAWBA_INT_ITEMS_PERCENTILE", 'DAWBA_INT_ITEMS_MAX', "DAWBA_INTERNALIZING_SUM", "DAWBA_INT_SUBSCALES_PERCENTILE"]])
DAWBAX = pd.concat([DAWBA_ID, intframe, DAWBAX_PERC], ignore_index = False, join = "inner", axis=1) 
DAWBAX.to_csv(path_or_buf = 'C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_FU3_items_percentile.csv', index=False)


######
#Write a dataframe containing only those for CFA 
# Specific Phobia, Social Phobia, Panic Disorder,  Post-traumatic Stress Disorder,  Genralized Anxiety Disorder, MDD
######
CFA_colnames = pd.DataFrame(dat[['sb1a' , 'sb1b' , 'sb1c' , 'sb1d' , 'sb1e' , 'sb1f' ,'sb1g' , 'sb1h' , 'sb1i' , 'sb1j' , 'sb1k' , 'sb1l' , 'sb1m', 'sb2' , 'sb3' , 'sb4', 'sb5' , 'sb6' , 'sb7' , 'sb8' , 'sb9' , 'sb10', 'sc1', 'sc2a', 'sc2b', 'sc2c', 'sc2d', 'sc2e', 'sc2f', 'sc6', 'sc7', 'sc8', 'sc10a', 'sc10b', 'sc10c', 'sc11', 'sc12', 'sc13', 'sc14', 'sc15', 'sc16', 'sd1', 'sd2a', 'sd2b', 'sd2c', 'sd3a', 'sd3b', 'sd3c', 'sd3d', 'sd3e', 'sd3f', 'sd3g', 'sd3h', 'sd3i', 'sd3j', 'sd3k', 'sd3l', 'sd3m', 'sd3n', 'sd4a', 'sd4b', 'sd4c', 'sd4d', 'sd5', 'sd6', 'sd7a', 'sd7b', 'sd7c', 'sd7d', 'sh1', 'sh2', 'sh3', 'sh4', 'sh5', 'sh13', 'sh14', 'sh15', 'sh16', 'sh17', 'sh18a', 'sh18b', 'sh18c', 'sh18d', 'sh18e', 'sh18f', 'sh18g',  'sh18h', 'sh18i', 'sh18j', 'sh18k', 'sh18l', 'sh19', 'sh20a', 'sh20b', 'sh20c', 'sh20d', 'sg2', 'sg2a', 'sg3', 'sg4a', 'sg4b', 'sg4c', 'sg4d', 'sg4e', 'sg4f', 'sg4g', 'sg4h', 'sg4i', 'sg4j', 'sg4k', 'sg6', 'sg7', 'sg8a', 'sg8b', 'sg8c', 'sg8d', 'sg8e', 'sg8f', 'sg9', 'sg10a', 'sg10b', 'sg10c', 'sg10d','se1', 'se2a', 'se2b', 'se2c', 'se2d', 'se2e', 'se2f', 'se2g', 'se2h', 'se2i', 'se2j', 'se2k', 'se3', 'se3a', 'se4a', 'se4b', 'se4c', 'se4d', 'se4e', 'se4f', 'se4g', 'se4h', 'se4i', 'se4j', 'se4k', 'se4l', 'se4m', 'se4n', 'se4o', 'se5', 'se6', 'se7', 'se8a', 'se8b', 'se8c', 'se8d']])
CFA_frame = DAWBAX = pd.concat([DAWBA_ID, CFA_colnames], ignore_index = False, join = "inner", axis=1) 
## Write CSV
CFA_frame.to_csv(path_or_buf = 'C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_FU3_CFA.csv', index=False)

#####
#Check missings 
# count the number of missing values for each column
miss = CFA_frame.isnull().sum()
#especially high amounts of missing values in PTSD

######Extract all computer calculated predictions
#grab band pattern in columns 
predict_cols = [col for col in dat.columns if 'band' in col]
print(predict_cols)
#make new dataframe with ids and relevant to internalizing predictions only 
cols = ['sspphband', 'ssophband', 'spanband', 'sagoband', 'sptsdband', 'socdband', 'sgenaband', 'sdepband', 'seatband']
predictions = dat.loc[:,cols]
predictions.columns = ["Specific_Phobia", "Social_Phobia", "Panic_Disorder", "Agoraphobia", "PTSD", "OCD", "Generalized_Anxiety", "Depression", "Eating_Disorder"]
predictions = pd.concat([DAWBA_ID, predictions], ignore_index = False, join = "inner", axis=1) 
sns.heatmap(predictions.corr(), annot=True);

crisis = pd.read_csv('/Users/hofma/Desktop/LGC/02_data/Crisis_75.csv', dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')
d = pd.read_csv('C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_BL_Computer_Predictions.csv', dtype={"ID":"string", 'string_col': 'float16','int_col': 'float16'}).set_index('ID')
#short check 
predictions_crisis = pd.concat([d, crisis], axis=1, join='inner', ignore_index=False).reset_index(drop = False) 
predictions_crisis.columns.tolist()
pred_t1 = predictions_crisis[[
 'Generalized_Anxiety',
 'Depression',
 'Social_Phobia',
 'Agoraphobia',
 'Panic_Disorder',
 'M1_T1',
 'M10_T1',
 'M11_T1',
 'M2_T1',
 'M3_T1',
 'M4_T1',
 'M5_T1',
 'M6_T1',
 'M7_T1',
 'M8_T1',
 'M9_T1']]

pred_t1.columns = ['GAD',
 'DEP',
 'SOC',
 'AGORA',
 'PAN',
 'M1_T1',
 'M10_T1',
 'M11_T1',
 'M2_T1',
 'M3_T1',
 'M4_T1',
 'M5_T1',
 'M6_T1',
 'M7_T1',
 'M8_T1',
 'M9_T1']


corr = pred_t1.corr()
# Generate a mask for the upper triangle
mask = np.triu(np.ones_like(corr, dtype=bool))
# Set up the matplotlib figure
f, ax = plt.subplots(figsize=(11, 9))
# Generate a custom diverging colormap
#cmap = sns.diverging_palette(230, 20, as_cmap=True)
# Draw the heatmap with the mask and correct aspect ratio
sns.heatmap(corr, mask=mask, vmax=.3, center=0,
            square=True, linewidths=.5, cbar_kws={"shrink": .5})


#write csv
predictions.to_csv(path_or_buf = 'C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_FU3_Computer_Predictions.csv', index=False)

