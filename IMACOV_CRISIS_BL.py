# -*- coding: utf-8 -*-
"""
Created on Mon Jan 31 15:33:04 2022

@author: hofma
"""

##################
#crisis BL
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
crisis_bl = pd.read_csv("IMACOV19-EXTRA_BL.csv", dtype={"User code":"string", 'string_col': 'float16', 'int_col': 'float16'})
crisis_bl.rename(columns={'User code': 'ID'}, inplace=True)
crisis_bl.set_index('ID')
crisis_BL_ID = crisis_bl["ID"]
crisis_BL_Language = crisis_bl["Language"]

#Colnames
crisis_bl.columns.tolist()

#Converting the Timestamp to exact age, timestamp is the numer of days 
crisis_bl['age'] = crisis_bl[["Completed Timestamp"]]/365

#Create an Overview 

#################################
# Background and Demographic Characteristics
#################################

#Used 
#BG1 = Profession_Status (Sind Sie derzeit berufstätig?) 
#BG2 = Urban_Rual (Was beschreibt am besten die Gegend, in der Sie leben?)
# BG3 = Size_of_household (Wie viele Personen leben derzeit in Ihrem Haushalt (Sie eingeschlossen)?
# BG6 = Phys_Health_percieved - Wie würden Sie Ihre allgemeine körperliche Gesundheit einschätzen?
# BG11 = Psych_Health_percieved - Wie würden Sie Ihre allgemeine psychische/emotionale Gesundheit vor der Coronavirus/COVID-19-Krise in Ihrer Region einschätzen? 5 = Schelcht

#Not used
# BG4 = State_of_health_insurance - Sind Sie krankenversichert?
# BG5 = State_funding - Haben Sie Anspruch auf staatliche Hilfeleistungen?
# BG7 = Phys_Health_professional_ever - Hat Ihnen eine Gesundheitsfachkraft jemals mitgeteilt, dass Sie eine körperliche Erkrankung haben?
# BG8 = Psych_Health_professional_ever - Hat Ihnen eine psychiatrische Fachkraft jemals gesagt, Sie hätten: 1 = Psych, 2 = ALkohol/Drugs 
# BG9 = height
# BG10 = weight
# BG10units = weight_units

####################################
# Corona Mood States - Prior to Pandemic Onset
####################################
# 'EW3M1', - worried_in_general ... wie besorgt waren Sie im Allgemeinen? 0 = Üerhaupt nich, 4 sehr
# 'EW3M10', - sad_happy... wie glücklich oder traurig waren Sie?  0 = sehr trauig, 4 = sehr glücklich 
# 'EW3M11', enjoy_activities ... wie sehr konnten Sie Ihre üblichen Aktivitäten genießen? #0 Überhaupt nicht, #4 sehr 
# 'EW3M2', relaxed_worried ... wie entspannt oder ängstlich waren Sie? #0=sehr entspannt und ruigh, 4 = sehr nervös
# 'EW3M3', rumminat ... wie zappelig oder unruhig waren Sie? #0=überhaupt nicht zappelig, 4= sehr zappelig 
# 'EW3M4', exhausted ... wie erschöpft oder müde waren Sie? #0 überhaupt nicht erschöpft #4 sehr erschöpft
# 'EW3M5', concentration... wie gut konnten Sie sich konzentrieren oder fokussieren? #0=sehr fokussiert #4=sehr unfokussiert
# 'EW3M6', irritable ... wie irritierbar oder reizbar sind Sie gewesen? #0=Überhaupt nicht, 4 = sehr 
# 'EW3M7', lonely ... wie einsam waren Sie? # 0 = Überhaupt nicht, 4 = sehr 
# 'EW3M8', neg_toughts... inwieweit haben Sie negative Gedanken gehabt und über unangenehme Erfahrungen oder Dinge nachgedacht, die Ihnen ein schlechtes Gefühl bereitet haben? #0 = Überhaupt nicht, #4 die meiste Zeit
# 'EW3M9', self_harm ... haben Sie absichtlich versucht sich selbst zu verletzen oder weh zu tun? #0 = überhaupt nicht, 4 = Die meiste Zeit 

####################################
# Corona Mood States - last two weeks
#####################################
# EW2W1 - worried_in_general... wie besorgt waren Sie im Allgemeinen? 0 = Üerhaupt nich, 4 sehr
# EW2W2 - sad_happy... wie glücklich oder traurig waren Sie? 0 = sehr trauig, 4 = sehr glücklich 
# EW2W3 - enjoy_activities... wie sehr konnten Sie Ihre üblichen Aktivitäten genießen? #0 Überhaupt nicht, #4 sehr 
# EW2W4 -  relaxed_worried ... wie entspannt oder ängstlich waren Sie? #0=sehr entspannt und ruigh, 4 = sehr nervös
# EW2W5 - rumminat ... wie zappelig oder unruhig waren Sie? #0=überhaupt nicht zappelig, 4= sehr zappelig 
# EW2W6 - exhausted ... wie erschöpft oder müde waren Sie? #0 überhaupt nicht erschöpft #4 sehr erschöpft
# EW2W7 - concentration... wie gut konnten Sie sich konzentrieren oder fokussieren? #0=sehr fokussiert #4=sehr unfokussiert
# EW2W8 - irritable ... wie irritierbar oder reizbar sind Sie gewesen? #0=Überhaupt nicht, 4 = sehr 
# EW2W9 - lonely ... wie einsam waren Sie? # 0 = Überhaupt nicht, 4 = sehr 
# EW2W10 - neg_toughts... inwieweit haben Sie negative Gedanken gehabt und über unangenehme Erfahrungen oder Dinge nachgedacht, die Ihnen ein schlechtes Gefühl bereitet haben? #0 = Überhaupt nicht, #4 die meiste Zeit
# EW2W11 - self_harm ... haben Sie absichtlich versucht sich selbst zu verletzen oder weh zu tun? #0 = überhaupt nicht, 4 = Die meiste Zeit 


##################################
# Corona Worries - last two weeks 
###################################
#'ES5_1' - ... infiziert zu sein oder zu werden?
#'ES5_2' - ... dass Freunde oder Familie infiziert werden? 
#'ES5_3' - … dass ihre <i>körperliche Gesundheit</i> durch eine Coronavirus-Infektion/COVID-19 beeinflusst wird?
#'ES5_4' - … dass ihre <i>psychische/emotionale Gesundheit</i> durch eine Coronavirus-Infektion/COVID-19 beeinflusst wird?
# ES6 - Wie viel lesen oder sprechen Sie über das Coronavirus/COVID-19?
# ES7 - Hat die COVID-19-Situation in Ihrer Region zu positiven Veränderungen in Ihrem Leben geführt? 0 = keine 4 = Einige 

##################
# Lifestyle Changes 
##################
# LC2 = other_humans … mit wie vielen Personen, außerhalb Ihres Haushalts, haben Sie ein persönliches Gespräch geführt? 0 – 4 Not at all; Extremly
# LC3 = outside ...… wie viel Zeit haben Sie außerhalb des Hauses verbracht (z.B. in Geschäften, Parks usw.)? 0-4 Not at all; Extremly 
# LC4 = strain_lockdown … wie belastend waren für Sie die Ausgangsbeschränkungen? not at all = 0, 4 sehr 
# LC5 = change_relationships … haben sich Ihre Kontakte zu Menschen außerhalb Ihres Hauses im Vergleich zu vor der Coronavirus/COVID-19-Krise in Ihrer Region verändert? 0-4-
# LC6 = contact_rules … wie schwierig war es für Sie, die Empfehlungen zur Vermeidung von engem Kontakt mit anderen Menschen zu folgen? 0-4 None; A great amount
# LC7 = qual_relat_fam_change… hat sich die Qualität der Beziehungen zwischen Ihnen und Ihrer Familie verändert? 0 = no change 2 =gleich  4 = viel besser
# LC8 = qual_relat_fam_strain … wie belastend waren diese Veränderungen der familiären Kontakte für Sie? 0-4 Not at all; Extremly
# LC9 = qual_relat_friends_change … hat sich die Qualität der Beziehungen zu Ihren Freunden verändert? 0-4 A lot worse; A lot better Reverse
# LC10 = qual_relat_friends_strain … wie belastend waren diese Veränderungen bei den sozialen Kontakten für Sie? 0-4 Not at all; Extremly 
# LC11 = fiance_fam_you … inwieweit haben Änderungen im Zusammenhang mit der Coronavirus/COVID-19- Krise in Ihrer Region zu finanziellen Problemen für Sie oder Ihre Familie geführt? 0-4 Not at all; Extremly  
# LC12 = stabilty_concern … inwieweit sind Sie über die Stabilität Ihrer Lebenssituation besorgt? 0-4 Not at all; Extremly  
# LC13 = food_worries … haben Sie sich Sorgen gemacht, ob Ihnen das Essen ausgeht?
# LC14 = hopefull_end ...… wie zuversichtlich sind Sie, dass die Coronavirus/COVID-19-Krise in Ihrer Region bald ein Ende findet?

#########################################
# Enriching Information
##########################################
#Not used 
# ACC1 = worries_covid_impact Beschreiben Sie hier bitte alles, was Sie über die Auswirkungen von Coronavirus/COVID-19 auf Sie, Ihre Freunde oder Ihre Familie beunruhigt.
# ACC2 = q_comment Bitte notieren Sie hier alle Kommentare zu dieser Umfrage und/oder verwandten Themen, die Sie gerne angeben möchten.

##########################################
#Corona - Daily Behaviour - last two weeks
##########################################
#Not used 
# DB3M1 - sleep_duration… wie viele Stunden haben Sie im Durchschnitt pro Nacht geschlafen? 0 - Weniger als 6 Stunden #1- 6-8 Stunden #2 - 8-10 Stunden #3 - Mehr als 10 Stunden
# DB3M2 - 30_min_training… an wie vielen Tagen pro Woche haben Sie mindestens 30 Minuten lang trainiert (z.B. erhöhte Herzfrequenz, Atmung)? #0- Keine #1- 1-2 Tage #2- 3-4 Tage #3- 5-6 Tage #4- Täglich
# DB3M3 - outside … an wie vielen Tagen pro Woche haben Sie sich im Freien aufgehalten? #0- Keine  #1- 1-2 Tage #2- 3-4 Tage #3 5-6 Tage #4- Täglich
# Corona - Eat - last two week 
# EB2W1 - … eat_daily wie viel haben Sie im Durchschnitt pro Tag gegessen? 0 sehr wenig to 4 sehr viel
# EB2W3 - … eat_amount haben Sie mehr/weniger Nahrung oder Snacks gegessen als üblich? 1 = ja, mehr : 2 = ja, weniger

#############################
#Psychische Gesundheit 
#############################
#Not used 
#MHC1 - Psych_Diag_ever Wurde bei Ihnen jemals eines dieser psychischen Probleme (Alkoholkonsumstörung, depressive Störung, Schizophrenie, Essstörung) diagnostiziert?
#MHC2 - Psych_Diag_ever_recent Falls ja, haben Sie in den letzten zwei Wochen eine Veränderung der Symptome festgestellt? 
#Depression
#MHCD1_1 - loss_of_interest ... haben Sie wenig Interesse oder Freude daran, Dinge zu tun?
#MHCD1_2 - depressive_feeling... fühlen Sie sich niedergeschlagen, depressiv oder hoffnungslos? 
#MHCD1_3 - sleep_disturbance... haben Sie Probleme beim Einschlafen/Durchschlafen oder schlafen zu viel?
#MHCD1_4 - tired... fühlen Sie sich müde oder haben wenig Energie? 
#MHCD1_5 - no_apetite_overeat ... haben Sie einen mangelnden Appetit oder essen übermäßig?
#MHCD1_6 - feel_bad_failure ... fühlen Sie sich schlecht - oder dass Sie ein Versager sind, der sich selbst oder Ihre Familie im Stich gelassen hat?
#MHCD1_7 - concentration_problems ... haben Sie Schwierigkeiten, sich auf Dinge zu konzentrieren, wie z.B. die Zeitung zu lesen oder fernzusehen?
#MHCD1_8 - move_speak_slow... haben Sie sich so langsam bewegt oder gesprochen, dass andere Leute es hätten bemerken können? Oder das Gegenteil – waren so unruhig oder zappelig, dass Sie sich viel mehr als sonst bewegt haben?
#MHCD1_9 - rahter_be_dead... hatten Sie Gedanken, dass es besser wäre tot zu sein oder dachten daran, sich selbst zu verletzen? All above 0 = No, 4 = Every Day 
#FOOD
#MHCE2_1 - unplaned_snacks ... an wie vielen Tagen haben Sie außerhalb der geplanten Mahlzeiten und Snacks wiederholt (mehr als zweimal in der gleichen Tageszeit) an Essen geknabbert und genascht? 
#MHCE2_2 - food_control ... an wie vielen Tagen haben Sie absichtlich versucht, die Menge an Nahrungsmitteln, die Sie essen, zu begrenzen, Lebensmittel, die Sie gerne essen, zu streichen oder bestimmte Regeln bezüglich Ihrer Ernährung zu befolgen, um Ihre Form oder Ihr Gewicht zu beeinflussen (unabhängig davon, ob Ihnen dies gelungen ist oder nicht)?
#MHCE2_3 - food_thoughts_rule_adherence ... an wie vielen Tagen hat das Nachdenken über Ihr Gewicht, Ihre Form, Ihr Essen, Ihre Ernährung oder Ihre Kalorien es Ihnen schwergemacht, sich auf Dinge zu konzentrieren, die Sie interessieren (z.B. arbeiten, einem Gespräch folgen oder lesen)?
#MHCE2_4 - binge_eat ... an wie vielen Tagen haben Sie "Binge Eating" betrieben, d.h. Sie haben ungewöhnlich viel gegessen und ein Gefühl des Kontrollverlusts erlebt?
#MHCE2_5 - purge ... an wie vielen Tagen haben Sie sich absichtlich erbrochen, um Ihre Form oder Ihr Gewicht zu kontrollieren?
#MHCE2_6 - diet_pills ... an wie vielen Tagen haben Sie Abführmittel, Diuretika oder Diätpillen eingenommen, um Ihre Form oder Ihr Gewicht zu kontrollieren? 0=nie, 4=every day
#MHCE3_1 - self_esteem_food... hat Ihr Gewicht, Ihre Form oder Ihre Ernährung Einfluss darauf, wie Sie über sich als Person denken (urteilen)?
#MHCE3_1 - food_behaviour_worries ... wie sehr haben Ihr Essverhalten, die Maßnahmen zur Gewichtskontrolle oder Bedenken bezüglich Ihres Gewichts und Ihrer Form Sie beunruhigt? 0= gar nicht, 4 = a lot
#Resilience
# 'RESILIENCE_1' = Ich neige dazu, mich nach schwierigen Zeiten schnell zu erholen. 
# 'RESILIENCE_2'= Ich denke, die Situation hat auch ihre positiven Seiten. 
# 'RESILIENCE_3' = Ich denke, dass es gar nicht so schlimm war, im Vergleich zu anderen Dingen. 
# 'RESILIENCE_4' = Ich denke, dass ich die Situation akzeptieren muss. 
# 'RESILIENCE_5' = Ich versuche mir von anderen Menschen Rat oder Hilfe darüber einzuholen, was zu tun ist.
# 'RESILIENCE_6' = Ich denke immer wieder darüber nach, wie schrecklich es ist, was ich erlebe. 0 = stimme gar nicht zu, 4 = stimme voll zu

###########################################
# Check for Analysis 
###########################################
#Demographics 
demo_col = ['ID', 'BG1', 'BG2', 'BG3', 'BG6', 'BG11']
demo = pd.DataFrame(crisis_bl.loc[:,demo_col])
demo = demo.dropna(axis = 0)
len(demo)

#Mood 3 Months prior 
mood_prior_col = ['ID', 'Language', 'EW3M1', 'EW3M10', 'EW3M11', 'EW3M2', 'EW3M3', 'EW3M4', 'EW3M5', 'EW3M6', 'EW3M7', 'EW3M8', 'EW3M9']
mood_prior = pd.DataFrame(crisis_bl.loc[:,mood_prior_col])
mood_prior = mood_prior.dropna(axis = 0)
len(mood_prior)
m = sns.heatmap(mood_prior[['EW3M1', 'EW3M10', 'EW3M11', 'EW3M2', 'EW3M3', 'EW3M4', 'EW3M5', 'EW3M6', 'EW3M7', 'EW3M8', 'EW3M9']].corr(), annot=True)
m
# recode EW3M2 and EW3M3 reverse items 

#Mood Sates
mood_col = ['ID', 'Language', 'EW2W1', 'EW2W2', 'EW2W3', 'EW2W4', 'EW2W5', 'EW2W6', 'EW2W7', 'EW2W8', 'EW2W9', 'EW2W10', 'EW2W11']
mood = pd.DataFrame(crisis_bl.loc[:,mood_col])
mood = mood.dropna(axis = 0)
len(mood)
mood.mean()
mw = sns.heatmap(mood[['EW2W1', 'EW2W2', 'EW2W3', 'EW2W4', 'EW2W5', 'EW2W6', 'EW2W7', 'EW2W8', 'EW2W9', 'EW2W10', 'EW2W11',]].corr(), annot=True)
mw
# recode EW2W2 and EW2W3 reverse items 

# Worries
worries_col = ['ID', 'Language', 'ES5_1', 'ES5_2', 'ES5_3', 'ES5_4', 'ES6', 'ES7']
worries = pd.DataFrame(crisis_bl.loc[:,worries_col])
worries = worries.dropna(axis = 0)
mood.mean()
len(mood)
w = sns.heatmap(worries[['ES5_1', 'ES5_2', 'ES5_3', 'ES5_4', 'ES6', 'ES7']].corr(), annot=True)
w
# recode ES7 reverse Item 

#Lifestyle Changes 
life_col = ['ID', 'Language', 'LC2', 'LC3', 'LC4', 'LC5', 'LC6', 'LC7', 'LC8', 'LC9', 'LC10', 'LC11', 'LC12', 'LC13', 'LC14']
life = pd.DataFrame(crisis_bl.loc[:,life_col])
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
cols_m = ['EW3M11', 'EW3M11', 'EW2W2', 'EW2W3']
crisis_bl = reverseScoring(crisis_bl, 4, cols_m)
cols_w = ['ES7']
crisis_bl = reverseScoring(crisis_bl, 3, cols_w)

#check
mw = sns.heatmap(crisis_bl[['EW2W1', 'EW2W2', 'EW2W3', 'EW2W4', 'EW2W5', 'EW2W6', 'EW2W7', 'EW2W8', 'EW2W9', 'EW2W10', 'EW2W11',]].corr(), annot=True, cmap=vidris)
mw

w = sns.heatmap(crisis_bl[['ES5_1', 'ES5_2', 'ES5_3', 'ES5_4', 'ES6', 'ES7']].corr(), annot=True, cmap=vidris)
w
######################################
#Final Set of Data 
#######################################
#crisis_Bl 
dat_bl_colnames = ['ID', 'Language', "Completed Timestamp", "age", 'ACC1', 'ACC2', 'geoLoc_search', 'BG1', 'BG2', 'BG3', 'BG6', 'BG11', 'EW2W1', 'EW2W2', 'EW2W3', 'EW2W4', 'EW2W5', 'EW2W6', 'EW2W7', 'EW2W8', 'EW2W9', 'EW2W10', 'EW2W11', 'ES5_1', 'ES5_2', 'ES5_3', 'ES5_4', 'ES6', 'ES7', 'LC2', 'LC3', 'LC4', 'LC5', 'LC6', 'LC7', 'LC8', 'LC9', 'LC10', 'LC11', 'LC12', 'LC13', 'LC14']
dat_bl = pd.DataFrame(crisis_bl.loc[:,dat_bl_colnames])
len(dat_bl)
dat_bl.to_csv(path_or_buf = '/Users/hofma/Desktop/LGC/02_data/Crisis_BL.csv', index=False)

mood_prior.columns = ['ID', 'Language','M1_M3','M10_M3','M11_M3', 'M2_M3', 'M3_M3', 'M4_M3', 'M5_M3', 'M6_M3', 'M7_M3', 'M8_M3', 'M9_M3']
mood_prior.head(10)
m3 = pd.DataFrame(mood_prior)
m3.to_csv(path_or_buf = '/Users/hofma/Desktop/LGC/02_data/Crisis_BL_3M.csv', index=False, sep=';')

#one for r 
dat_bl.to_csv(path_or_buf = '/Users/hofma/Desktop/LGC/02_data/Crisis_BL_sep.csv', index=False, sep=";")
