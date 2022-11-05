import pandas as pd
import matplotlib.pyplot as plt


plt.close("all")
df = pd.read_csv("./Data/helsinki_weather.csv")


# Droppaa timezone, 
# Muuta Year, m , d, Time to datetime object
# Set datetime to index
date_columns = ['Year','m', 'd', 'Time']
#df['datetime'] = pd.to_datetime(df['Year','m', 'd', 'Time'])   
df['Datetime'] =  pd.to_datetime(df['d'].astype(str) \
                                 + '-' + df['m'].astype(str) \
                                 + '-' + df['Year'].astype(str) \
                                 + ' ' + df['Time'], 
                                 format='%d-%m-%Y %H:%M')
df = df.drop(['d','m','Year', 'Time', 'Time zone'], axis=1)
# Sets the date as index
df = df.set_index('Datetime')


#df['Air temperature (degC)'] = df['Air temperature (degC)'].astype(float)
#df['Air temperature (degC)'] = df['Air temperature (degC)'].apply(lambda x: float(x.replace('\U00002013', '-')))


# print(df.loc[df['Air temperature (degC)'] == "-"])

#Drops air temp values which are just "-" (no number)
df.drop(df.loc[df['Air temperature (degC)']=="-"].index, inplace=True) 
# Sets type as float (from string)
df['Air temperature (degC)'] = df['Air temperature (degC)'].astype(float)
# print(df.head())


# plt.figure()
df.plot()

plt.show()

