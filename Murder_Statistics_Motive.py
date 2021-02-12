# -*- coding: utf-8 -*-
"""
IST 652 Final Project
Author: Kathryn Egan
DUE: 9/4/2020
"""

import numpy as np 
import pandas as pd 
from pandas import Series,DataFrame
import matplotlib 
import scipy.stats
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.collections import PatchCollection
import statsmodels.api as sm


## Read in the homicide dataset using pandas
df = pd.read_csv('C:\\Users\\katva\\Documents\homicide_db.csv')
## Remove first 3 columns
df = df.drop(["Record_ID","Agency_Code","Agency_Name"], axis=1)
df.head

## basic exploration what's in my database
df.shape ## How many columns and rows
df.dtypes ## I may need to change some types later
df.describe()##look at min & max, mean, quartiles of victim and perp counts
df.isnull().values.any() # checking for missing values, there aren't any


### What is the Avg Number of Homicides each year
years = pd.DataFrame(df, columns = ['Year']) 
count_years = years.stack().value_counts()#This is out of order
homicides = count_years.sort_index(axis=0, ascending=False)#put in order
homicides.describe
min(homicides) #14331
max(homicides) #24335
Avg_Annual_Homicides = sum(homicides)/len(homicides)## Average Number of Homicides per Year = 18241.54

## Visualize number of murders over the years
plt.figure(figsize=((15, 5)))
ax = sns.countplot(
    x='Year',
    data=df)
ax.set_xticklabels(
    ax.get_xticklabels(),
    rotation=45,
    horizontalalignment='right'
);


## create new df for homicides by year to add to merge with unemployment later
df1 = pd.DataFrame(homicides, columns=['Avg_Number_of_Homicides']) ## create new df for homicides by year to add to merge with unemployment later
df1
df1['Year'] = df1.index



## Which state has the highest homicide count 
state = pd.DataFrame(df, columns = ['State'])
count_states = state.stack().value_counts()
homicide_by_state = count_states.sort_index(axis=0,ascending=False)
homicide_by_state
homicide_by_state.columns = ['State','Murder_Count']
homicide_by_state.to_csv('State_Homicide_Counts.csv')

plt.figure(figsize=((20.27, 4.27)))
ax = sns.countplot(
    x='State',
    data=df)
ax.set_xticklabels(
    ax.get_xticklabels(),
    rotation=45,
    horizontalalignment='right'
);



### What are most common weapons used based on gender and relationship of victim to perp?
murder_by_gender = df.groupby(['Victim_Sex', 'Perpetrator_Sex', 'Weapon', 'Relationship']).size()
genderdf = murder_by_gender.to_frame(name='Count')
genderdf
genderdf.to_csv('Murder_by_Gender.csv')



## How do demographics correlate whether or not a homicide has been solved

unsolved = df.groupby(['Crime_Solved','Victim_Age', 'Victim_Race', 'State']).size()
unsolveddf = unsolved.to_frame(name='Count')
unsolveddf
unsolveddf.to_csv('Solved_vs_Unsolved.csv')



#### Create dictionaries from the data to best run correlation analysis and create heatmap
### Definition of a small function to construct a dictionnary
def make_dict(x,col):
    if x not in col :
        col[x]=len(col)
    return col
#build dictionaries and transform the data

states = {}
for state in df['State']:
    (make_dict(state,states))
df['State']=df['State'].map(states)
    
years = {}
for year in df['Year']:
    (make_dict(year,years))
df['Year']=df['Year'].map(years)   
    
months = {}
for month in df['Month']:
    (make_dict(month,months))
df['Month']=df['Month'].map(months)    
           
was_it_solved= {}
for solution in df['Crime_Solved']:
    (make_dict(solution,was_it_solved))
df['Crime_Solved']= df['Crime_Solved'].map(was_it_solved)    
    
vicSex = {}
for sex in df['Victim_Sex']:
    (make_dict(sex,vicSex))
df['Victim_Sex']=df['Victim_Sex'].map(vicSex)

vicAge = {}
for age in df['Victim_Age']:
    (make_dict(age,vicAge))
df['Victim_Age']=df['Victim_Age'].map(vicAge)
    
vicRace = {}
for race in df['Victim_Race']:
    (make_dict(race,vicRace))
df['Victim_Race']=df['Victim_Race'].map(vicRace)

relationship = {}
for relation in df['Relationship']:
    (make_dict(relation,relationship))
df['Relationship']=df['Relationship'].map(relationship)

murderWeapon = {}
for weapon in df['Weapon']:
    (make_dict(weapon,murderWeapon))  
df['Weapon']=df['Weapon'].map(murderWeapon)   

    
### Explore the correlations
corr = df.corr()## Which variables are correlated
# my original heatmap code: sns.heatmap(corr, annot=True)was not easy to understand bc it was too jumbled
## I got part of the below heatmap code from the article “Better Heatmaps and Correlation Matrix Plots in Python”  by Drazen Zaric, April 15, 2019 
# accessed from  https://towardsdatascience.comfrom https://towardsdatascience.com/better-heatmaps-and-correlation-matrix-plots-in-python-41445d0f2bec
plt.figure(figsize=(15,15))
ax = sns.heatmap(
    corr, 
    vmin=-1, vmax=1, center=0,
    linewidths=.1,
    cmap="inferno",
    square=True)

ax.set_xticklabels(
    ax.get_xticklabels(),
    rotation=45,
    horizontalalignment='right'
);

####################################################
    
### Reading in the Unemployment rate data
dfemp = pd.read_csv('C:/Users/katva/Documents/USUnemployment.csv')
dfemp
df2 = dfemp.set_index('Year') #Make the years the row names
df2

col = df2.loc[: , "Jan":"Dec"]
df2['Annual_Mean'] = col.mean(axis=1)
df2
df2['Avg_Unemployment_Rate']= df2['Annual_Mean'].round(decimals= 1)
df2

## Now I want to merge the average homicide df1 with this new unemployment df2
rates = pd.merge(df1, df2, on='Year')
rates.head()
rates = rates.drop(["Jan","Feb","Mar","Apr","May","Jun", "Jul","Aug", "Sep","Oct", "Nov", "Dec", "Annual_Mean"], axis=1)
rates.head()

rates.plot(x="Year", y=["Avg_Number_of_Homicides"], kind="line")
rates.plot(x="Year", y=["Avg_Unemployment_Rate"], kind="line", color = "orange")
#The above charts make it visually evident that I need to scale the data if I want to compare it
rates[['Avg_Number_of_Homicides','Avg_Unemployment_Rate']]-= rates[['Avg_Number_of_Homicides','Avg_Unemployment_Rate']].min()
rates[['Avg_Number_of_Homicides','Avg_Unemployment_Rate']]/= rates[['Avg_Number_of_Homicides','Avg_Unemployment_Rate']].max()
rates.plot(x="Year")

## Statistically, how strong of a predictor of number of homicides is the unemployment rate
X = rates["Avg_Number_of_Homicides"]
y = rates["Avg_Unemployment_Rate"]
z = rates["Year"]

model = sm.OLS(X, y).fit()
predictions = model.predict(y) 

# Print out the statistics
model.summary()

##### I'm surprised by the fact that in the last decade, during the Great Recession the homicide rate went down, while the unemployment rate went up
#### So, maybe avg household income is a better indicator.
## I tried using Beautiful Soup first, and it worked, but the table wasn't easy to extaract from once I grabbed that data, so after
## some searches on Stack.com for help making my table prettier, I found a post about using pandas to read in the table from URL
## It actually is so much better.


income_table = pd.read_html("https://www.multpl.com/us-average-income/table/by-year")
# Set the first column as the index
type(income_table)

income = income_table[0]
type(income)

income.describe()     

for col in income.columns: 
    print(col) 

income.columns = ['Date','Avg_Household_Income']


income = income[income.Date != 'Dec 31, 2016']
income = income[income.Date != 'Dec 31, 2015']
income = income[income.Date != 'Dec 31, 1967']
income = income[income.Date != 'Dec 31, 1968']
income = income[income.Date != 'Dec 31, 1969']
income = income[income.Date != 'Dec 31, 1970']
income = income[income.Date != 'Dec 31, 1971']
income = income[income.Date != 'Dec 31, 1972']
income = income[income.Date != 'Dec 31, 1973']
income = income[income.Date != 'Dec 31, 1974']
income = income[income.Date != 'Dec 31, 1975']
income = income[income.Date != 'Dec 31, 1976']
income = income[income.Date != 'Dec 31, 1977']
income = income[income.Date != 'Dec 31, 1978']
income = income[income.Date != 'Dec 31, 1979']

income

income['Year'] = pd.DatetimeIndex(income['Date']).year

income = income.drop(["Date"], axis=1)

df3 = pd.merge(df1, income, on='Year')

df3.plot(x='Year')

## scale the data for a better plot
df3[['Avg_Number_of_Homicides','Avg_Household_Income']] -= df3[['Avg_Number_of_Homicides','Avg_Household_Income']].min()  

df3[['Avg_Number_of_Homicides','Avg_Household_Income']] /= df3[['Avg_Number_of_Homicides','Avg_Household_Income']].max() 

df3.plot(x='Year')


## Statistically, how strong of a predictor of number of homicides is the avg household income
X = df3["Avg_Number_of_Homicides"]
y = df3["Avg_Household_Income"]
z = df3["Year"]

model = sm.OLS(X, y).fit()
predictions = model.predict(y) 

# Print out the statistics
model.summary()










