
#Maurits Hilverda
#April 8, 2020
#The purpose of this code is to download images from the internet using those images' URLs provided in an excel file


############

import requests
import pandas as pd
import os
#import bs4 as bs
from tqdm import tqdm # for a nice looking progress bar
from urllib.parse import urljoin, urlparse

# Read excel sheet and select relevant columns
data = pd.read_excel('/Users/mauritshilverda/Documents/Studie/MSc/Thesis/DATA/METHODOLOGY_2/ScraperInputData2.xlsx')
df = pd.DataFrame(data, columns= ['Property_ID', 'Listing_Main_Image_URL', 'Host_Image_URL'])

# Make lists from url variables
#ids = df['id'].tolist()
URL_Listing_Image = df['Listing_Main_Image_URL'].tolist()
URL_Host_Image = df['Host_Image_URL'].tolist()


pathname = str(input('pathname: '))

def download(url, pathname):
    if not os.path.isdir(pathname):
        os.makedirs(pathname)
    # read 1024 bytes every time
    buffer_size = 1024
    # download by chunk to avoid crashing
    response = requests.get(url, stream=True)
    #retrieve total file size
    file_size = int(response.headers.get("Content-Length", 0))
    # create file name
    df2 = df.set_index('Host_Image_URL')
    id_name = df2.loc[url, 'Property_ID']
    filename = os.path.join(pathname, str(id_name) + '.jpg')
    # make progress bar and change the units to bytes
    progress = tqdm(response.iter_content(buffer_size), f"Downloading {filename}", total=file_size, unit="B", unit_scale=True, unit_divisor=1024)
    with open(filename, "wb") as f:
        for data in progress:
            # check data to update
            f.write(data)
            #update progress bar with data
            progress.update(len(data))

for url_0 in URL_Host_Image:
    download(url_0, pathname)

