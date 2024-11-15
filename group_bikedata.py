import pandas as pd
import numpy as np
import csv

data = []

filenames = ["201901-bluebikes-tripdata.csv", "201902-bluebikes-tripdata.csv","201903-bluebikes-tripdata.csv","201904-bluebikes-tripdata.csv","201905-bluebikes-tripdata.csv","201906-bluebikes-tripdata.csv","201907-bluebikes-tripdata.csv","201908-bluebikes-tripdata.csv","201909-bluebikes-tripdata.csv","201910-bluebikes-tripdata.csv","201911-bluebikes-tripdata.csv","201912-bluebikes-tripdata.csv"]

for name in filenames:
    with open(f'/Users/burchm/Downloads/{name}', mode ='r')as file:
        csvFile = csv.reader(file)
        next(csvFile)
        for lines in csvFile:
            data.append(lines)

columns = ['tripduration', 'starttime', 'stoptime', 'start_station_id', 'start_station_name', 'start_station_lat', 'start_station_lon', 'end_station_id', 'end_station_name', 'end_station_lat', 'end_station_lon', 'bikeid', 'usertype', 'birthyear', 'gender']

df = pd.DataFrame(data, columns=columns)

df['tripduration'] = df['tripduration'].astype(int)

df = df.loc[(df['tripduration'] <= 259200)]

df_group = df.groupby(['end_station_lat', 'end_station_lon'])

new_data = {'Lat': [], 'Lon': [], 'Sub_Prop': []}

for (lat, lon), group in df_group:
    if lat != "0.0":
        new_data['Lat'].append(lat)
    if lon != "0.0":
        new_data['Lon'].append(lon)
        total_trips = len(group)
        sub_trips = 0
        for index, row in group.iterrows():
            if row['usertype'] == "Subscriber":
                sub_trips += 1
        sub_prop = sub_trips/total_trips
        new_data['Sub_Prop'].append(sub_prop)

df_out = pd.DataFrame(new_data)

df_out.to_csv('output.csv', index=True, header=True, sep=',', encoding='utf-8')

