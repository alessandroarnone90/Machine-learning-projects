import warnings
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
from pylab import rcParams
rcParams['figure.figsize'] = 18, 8
sns.set(rc={'figure.figsize':(11, 4)})
warnings.filterwarnings("ignore")
plt.style.use('fivethirtyeight')



df = pd.read_csv("dataset.csv")
df.iloc[3]['DATA']


df['DATA']=pd.to_datetime(df.ANY * 10000 + df.MES * 100 + df.DIA, format='%Y%m%d')

#TO REPRODUCE ERROR
#df['DATA']=pd.to_datetime(df['DATA'])
#prova=df[df['DIA']==7]
#prova=prova[prova['MES']==3]
#prova=prova[prova['ANY']==2020]

#prova1=df[df['DIA']==13]
#prova1=prova1[prova1['MES']==3]
#prova1=prova1[prova1['ANY']==2020]


#test=df[(df['DATA'] > '2020-03-04') & (df['DATA'] < '2020-03-15')]

df.shape  #492286, 69
df.info() #colnames/types
description=df.describe() #min max 1991 -2020 -



df = df.iloc[:, :-1]

df.replace(['V','N'],[1,0], inplace=True)
index = df.columns.str.startswith('V')
colnames=df.columns[index]

#how many observation
df['totalValid'] = df.iloc[:,index].sum(axis=1)
dfTest=df.copy(deep=True)

#if there are 0 observation valid it means there's no data avaiable for that day for a specific contaminant - hence I remove it
df=df[df["totalValid"]!=0]
#prova=dfTest[dfTest["totalValid"]==0] how many NA values
#prova=prova[prova["ANY"]==2020] in 2020

#columns index starting with the V
index = df.columns.str.startswith('V')
#drop them
df.drop(df.columns[index], axis=1, inplace=True)
#sum by day - sum by row from column h1 to column h24
df["avg"] = df.iloc[:,-25:-1].sum(axis=1)
#averaging them
df["avg"]=df["avg"]/df['totalValid']

x=list(df.columns)
print(x)




toRemove=[ 'CODI MESURAMENT', 'MUNICIPI','TIPUS ESTACIÓ','UNITATS' ,'PUNT MOSTREIG','CODI EOI', 'PROVINCIA', 'CODI MUNICIPI', 'LATITUD', 'LONGITUD', 'ALTITUD',
           'MAGNITUD','CODI ESTACIÓ', 'H01', 'H02', 'H03', 'H04', 'H05', 'H06', 'H07', 'H08', 'H09', 'H10', 'H11', 'H12',
           'H13', 'H14', 'H15', 'H16', 'H17', 'H18', 'H19', 'H20', 'H21', 'H22', 'H23', 'H24','totalValid',
           'ÀREA URBANA','ANY','MES','DIA', 'NOM ESTACIÓ']
df = df.drop(toRemove, 1)




df=df.groupby(["DATA","CONTAMINANT"],  as_index=False).mean()

df.set_index('DATA', inplace=True)

df=df.pivot(index=df.index, columns='CONTAMINANT')['avg']


df['Year'] = df.index.year
df['Month']=df.index.month
df['Day']=df.index.day

x=list(df.columns)
print(x)
#HLC AND CH6H ONLY HAVE NA

cols_plot = [ 'CO', 'H2S', 'HCNM', 'HCT', 'NO', 'NO2', 'NOX', 'O3', 'PM10', 'PM2.5', 'PS', 'SO2']
df[cols_plot].plot( figsize=(11, 9), subplots=True)



toRemove=[  'H2S', 'HCNM', 'HCT', 'PS', 'PM2.5']
df = df.drop(toRemove, 1)
cols_plot = [ 'CO', 'NO', 'NO2', 'NOX', 'O3', 'PM10', 'SO2']

df[cols_plot].plot(figsize=(11, 9),linewidth=0.5, subplots=True)


#years=list(range(2,2021))
#df=df[df["Year"].isin(years)]
finalDf=df.copy(deep=True)
finalDf=finalDf.query("index >= '2012-01-01'")
df.index = pd.to_datetime(df.index)


finalDf[cols_plot].plot(figsize=(11, 9), subplots=True)

finalDf = finalDf.asfreq('D')
finalDf=finalDf.ffill()
#finalDf['Day']=finalDf.index.day

finalDf[cols_plot].plot(figsize=(11, 9), subplots=True)
#finalDf.index = finalDf.index.date
finalDf['type']='actuals'
finalDf.loc[finalDf['Year'] == 2020, 'type'] = 'low Emission Zone'
start_date="13/03/2020"
end_date="15/04/2020"
finalDf.loc[finalDf[start_date : end_date].index,'type']='Lockdown'
#inalDf.loc[(finalDf['Year'] == 2020) & (finalDf['Month'] >= 3) & (finalDf['Day'] >= 13 ),'type'] = 'LockDown'

finalDf.to_csv('/Users/alessandro/FinalInterview/Export.csv', index = True)



monthlyDf = finalDf[cols_plot].resample('M').mean()

#monthlyDf.index=monthlyDf.index.date
monthlyDf.plot(figsize=(11, 9), linewidth=1, subplots=True)
# Start and end of the date range to extract

