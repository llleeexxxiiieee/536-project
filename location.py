from geopy.geocoders import Nominatim
import os
import re

# Initialize the geolocator with a user agent
geolocator = Nominatim(user_agent="my_geocoder_app")


# for each file in the folder 

directory_path = "data/routes"  # Replace with the actual path

for filename in os.listdir(directory_path):
    full_path = os.path.join(directory_path, filename)
    if os.path.isfile(full_path):  # Check if it's a file
        # print(f"File: {filename}")
        pattern = "lon=\".*\""
        with open(full_path, "r") as f:
            file_content = f.read()

# # Extract all values
# all_extracted_values = re.findall(pattern, file_content)
# print(f"All extracted values: {all_extracted_values}")

        # Extract and print values one by one
        time = re.search(r'</ele><time>.*</time>', file_content, flags=0)
        date_val = re.search(r'\d{4}-\d{2}-\d{2}', time.group(), flags=0)
        date = date_val.group()
        values = re.search(pattern, file_content, flags=0)
        # print(values.group())
        numbers = re.findall(r'[-+]?\d+\.?\d*', values.group()) # Matches integers and floats
        # print(numbers)
        latitude = numbers[1]
        longitude = numbers[0]

        # Perform reverse geocoding
        location = geolocator.reverse(f"{latitude}, {longitude}")

        # Extract and print the city
        if location:
            address = location.raw['address']
            # print(address)
            city = address.get('city', '')
            town = address.get('town', '')
            state = address.get('state', '')
            country = address.get('country', '')
            if city:
                print(f"On {date} the person was in {city}, {state}, {country}. (File: {filename})")
            elif town:
                print(f"On {date} the person was in {town}, {state}, {country}. (File: {filename})")
            else:
                print("City information not found for these coordinates.")
        else:
            print("Location not found for these coordinates.")


# grab the lat and lon

# lookup the city

# save the date and city in an array 


# Define latitude and longitude




