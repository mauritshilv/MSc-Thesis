#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu May 28 13:59:04 2020

@author: mauritshilverda
"""
# The purpose of this Python script is to count the number of visible human faces for a large number of photos in the most efficient manner, 
# and consequently store this in a usable format


#create new environment
#conda create -n facerec2 python=3.7 anaconda -y
# check all available environments
#conda info --envs
#conda activate facerec2

# Load required packages
import cv2
import numpy as np
import os
from os import listdir
from os.path import isfile, join
import glob
import pandas as pd

#Load the Cascade file
cascade_path = '/Users/mauritshilverda/Downloads/haarcascade_frontalface_default.xml'
face_cascade = cv2.CascadeClassifier(cascade_path)
print(face_cascade)
#Check if file is properly loaded before continouing
test = face_cascade.load('/Users/mauritshilverda/Downloads/haarcascade_frontalface_default.xml')
print(test)

# Check current working directory
os.getcwd()


# Define function that take an image, adjusts it to a grey-scale, and activates the facial recognition technique to count the number of faces visible
def applyCascade(image_name):
    img = cv2.imread(image_name)
    grayimg = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
    #print('Loaded ', file)
    faces = face_cascade.detectMultiScale(grayimg, 1.3, 5)
    face_count = 0
    for (x, y, w, h) in faces:
        face_count = face_count + 1
    print(image_name, face_count)
    #dataframe = dataframe.append({'HostID': image_name, 'FaceCount': face_count}, ignore_index=True)
    return face_count

# Create a list with all the image names
pathname = '/Users/mauritshilverda/Documents/Studie/MSc/Thesis/python/airbnbscraper/host_images_jpg'
list_of_files = [f for f in glob.glob(pathname+'/*.jpg')]
# Check how many images are present (note: many images are empty files since they were not available anymore)
number_files = len(list_of_files)
print(number_files)

#Create empty list to store results
result = []

#Loop through the function for all host images
#Since many host images are empty, we use the pass command to ignore errors and continue with looping through the function
#For every image, the function's result is stored in the earlier created list
for i in list_of_files:
    try:
        result.append((i, applyCascade(i)))
    except:
        pass

#Store output as a dataframe with appropriate column labels instead of the list    
df3 = pd.DataFrame(result, columns=['HostID', 'FaceCount'])

#Export the results to a CSV file in the desired directory
df3.to_csv (r'/Users/mauritshilverda/Documents/Studie/MSc/Thesis/python/FaceRecognition/FaceRecognition_results.csv', index = False, header=True)
