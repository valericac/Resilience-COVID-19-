import numpy as np
import pandas as pd
import seaborn as sns
from sklearn.ensemble import IsolationForest
import matplotlib.pyplot as plt
from itertools import cycle


# load the crisisset
crisis = pd.read_csv("/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D.txt", sep=",")
crisis.columns.tolist()
crisis.head(10)

#check mood
sns.boxplot(crisis.M_T1)
sns.boxplot(crisis.M_T2)
sns.boxplot(crisis.M_T3)
sns.violinplot(x=crisis['M_T1'])
sns.violinplot(x=crisis['M_T2'])
sns.violinplot(x=crisis['M_T3'])

#check worries
sns.boxplot(crisis.W_T1)
sns.boxplot(crisis.W_T2)
sns.boxplot(crisis.W_T3)
sns.violinplot(x=crisis['W_T1'])
sns.violinplot(x=crisis['W_T2'])
sns.violinplot(x=crisis['W_T3'])


##############
#Outlier Worries 
##############
#T1
random_state = np.random.RandomState(42)
model=IsolationForest(n_estimators=100,max_samples='auto',contamination=float(0.1),random_state=random_state)
model.fit(crisis[['M_T1']])
print(model.get_params())

crisis['scores_T1'] = model.decision_function(crisis[['M_T1']])
crisis['anomaly_score_T1'] = model.predict(crisis[['M_T1']])
crisis[crisis['anomaly_score_T1']==-1].head()

#T2
model.fit(crisis[['M_T2']])
print(model.get_params())

crisis['scores_T2'] = model.decision_function(crisis[['M_T2']])
crisis['anomaly_score_T2'] = model.predict(crisis[['M_T2']])
crisis[crisis['anomaly_score_T2']==-1].head()

#T3
model.fit(crisis[['M_T3']])
print(model.get_params())

crisis['scores_T3'] = model.decision_function(crisis[['M_T3']])
crisis['anomaly_score_T3'] = model.predict(crisis[['M_T3']])
crisis[crisis['anomaly_score_T3']==-1].head()

#########
#identification of two outliers that are anomal at every time point
# 6210895  
# 6471914

#########
#Outlier Worries
#########
#T1
random_state = np.random.RandomState(42)
model=IsolationForest(n_estimators=100,max_samples='auto',contamination=float(0.1),random_state=random_state)
model.fit(crisis[['W_T1']])
print(model.get_params())

crisis['scores_WT1'] = model.decision_function(crisis[['W_T1']])
crisis['anomaly_score_WT1'] = model.predict(crisis[['W_T1']])
crisis[crisis['anomaly_score_WT1']==-1].head()

#T2
model.fit(crisis[['W_T2']])
print(model.get_params())

crisis['scores_WT2'] = model.decision_function(crisis[['W_T2']])
crisis['anomaly_score_WT2'] = model.predict(crisis[['W_T2']])
crisis[crisis['anomaly_score_WT2']==-1].head()

#T3
model.fit(crisis[['W_T3']])
print(model.get_params())

crisis['scores_WT3'] = model.decision_function(crisis[['W_T3']])
crisis['anomaly_score_WT3'] = model.predict(crisis[['W_T3']])
crisis[crisis['anomaly_score_WT3']==-1].head()

##no outliers that are anormal across all timetpoints 

out = crisis[["ID", "M_T1", "M_T2", "M_T3"]]
out = pd.melt(out, id_vars=['ID'])
out
Times = cycle(['T2','T3','T4'])
out['Time'] = [next(Times) for Time in range(len(out))]
out.head()

out2 = out[out["ID"].isin([1121275, 2437251, 6210895, 7062184, 10974785])]
out2.head(30)

out3 = out[out["ID"].isin([240546, 1121275, 16376434,  16908046, 19912904])]
out3.head(30)

out4 = out[out["ID"].isin([1938036, 5055175, 6471914, 16275727, 18934340])]
out4.head(30)

