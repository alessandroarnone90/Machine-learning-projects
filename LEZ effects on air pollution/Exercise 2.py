import pandas as pd

dataset = pd.read_csv("generatedNumber.csv")
dataset["timestamp"]=pd.to_datetime(dataset["timestamp"])

#create a second dataset with no timestamp to test the function
cars = {'Brand': ['Honda Civic','Toyota Corolla','Ford Focus','Audi A4'],
        'Price': [22000,25000,27000,35000] }
cars = pd.DataFrame(cars, columns = ['Brand', 'Price'])

df2=pd.DataFrame()
df3=pd.DataFrame()
aggregationType=['mean','sum','max','min', 'median','count','first','last','std','var','mad','prod']
timeType=['year','month','day','weekday']

#df3=function(dataframe, "sum","day") format type

def function (df, l, y):
    if len(df.columns)==2: #check there is 2 columns
        if (pd.core.dtypes.common.is_datetime64_ns_dtype(df.iloc[:,1])) | pd.core.dtypes.common.is_datetime64_ns_dtype(df.iloc[:,0]):   #check  one and only one column timestamp
            if(pd.core.dtypes.common.is_numeric_dtype(df.iloc[:,1]) |  pd.core.dtypes.common.is_numeric_dtype(df.iloc[:,0])): #check one and only one column numeric
                if l in aggregationType: #check if it's a valid measure
                    if y in timeType: #check if it's a valid timeType
                        if pd.core.dtypes.common.is_datetime64_ns_dtype(df.iloc[:, 0]):
                            df2['timestamp'] = df.iloc[:, 0]   #we want it works also in case the columns are switched in the df
                            df2['x'] = df.iloc[:, 1]
                        else:
                            df2['timestamp'] = df.iloc[:, 1]
                            df2['x'] = df.iloc[:, 0]

                        if y=='year':
                            df2['Timing']= df2['timestamp'].dt.year
                            df3=df2.groupby(['Timing'])['x'].agg(l)

                        elif y=='day':
                            df2['Timing']= df2['timestamp'].dt.day
                            df3=df2.groupby(['Timing'])['x'].agg(l)

                        elif y=='month':
                            df2['Timing']= df2['timestamp'].dt.month
                            df3=df2.groupby(['Timing'])['x'].agg(l)

                        elif y == 'weekday':
                            df2['Timing'] = df2['timestamp'].dt.weekday
                            df3 = df2.groupby(['Timing'])['x'].agg(l)

                        print(df3)
                        return df3

                    else: print("aggregation timing not valid")
                else: print('aggregation measure not valid')
            else: print("Missing numeric data in the dataframe ")
        else : print("Missing timestamp in the dataframe")
    else: print("dataframe expects only 2 columns")


df3=function(cars, "mean","year")


