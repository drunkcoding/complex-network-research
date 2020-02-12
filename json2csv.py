import json
import csv
import pandas as pd



fpartner = open("partnerAreas.json", "r")
freporter = open("reporterAreas.json", "r")

data_partner = json.load(fpartner)
data_reporter = json.load(freporter)
f = csv.writer(open("country.csv", "w"))
f.writerow(["id", "text"])
for x in data_partner["results"]:
    f.writerow([x["id"], x["text"]])
for x in data_reporter["results"]:
    f.writerow([x["id"], x["text"]])    

df = pd.read_csv('country.csv')
df = df.drop_duplicates('id', keep='first')
df.to_csv("country-unique.csv", encoding='utf-8')


