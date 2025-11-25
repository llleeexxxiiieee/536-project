import xml.etree.ElementTree as ET
import pandas as pd
from pandasgui import show
import datetime as dt
import matplotlib.pyplot as plt
plt.style.use("fivethirtyeight")

# create element tree object
tree = ET.parse('data/Export.xml') 
# for every health record, extract the attributes
root = tree.getroot()
record_list = [x.attrib for x in root.iter('Record')]
# print(record_list[:10])
record_data = pd.DataFrame(record_list[:20])

print("after df and tree")

# proper type to dates
for col in ['creationDate', 'startDate', 'endDate']:
    record_data[col] = pd.to_datetime(record_data[col])

print("after loop")

# value is numeric, NaN if fails
record_data['value'] = pd.to_numeric(record_data['value'], errors='coerce')

# some records do not measure anything, just count occurences
# filling with 1.0 (= one time) makes it easier to aggregate
record_data['value'] = record_data['value'].fillna(1.0)

# shorter observation names
record_data['type'] = record_data['type'].str.replace('HKQuantityTypeIdentifier', '')
record_data['type'] = record_data['type'].str.replace('HKCategoryTypeIdentifier', '')
print(record_data.tail(20))
gui = show(record_data.tail(20))
print("done")