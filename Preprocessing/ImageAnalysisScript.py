#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jun  3 12:34:34 2020

@author: mauritshilverda
"""
# The purpose of this script is to analyse  listing images to extract image quality features
# Note: for this code I was inspired by Adrian Rosebrock's example on https://www.pyimagesearch.com/2017/06/05/computing-image-colorfulness-with-opencv-and-python/

# Load required packages
from imutils import build_montages
from imutils import paths
import numpy as np
# Load full package just in case
import imutils


# Load os package
import os
# Check and if needed adjust working directory
os.getcwd()
os.chdir("/Users/mauritshilverda/Documents/Studie/MSc/THESIS/python/airbnbscraper/Scraped_Listing_Images/jpgFiles")

# Load cv2 package
import cv2


# Create function that calculates the mean of the luminance for all pixels in an image to determine that image's luminance
def average_luminance(image):
    # Get the image's RGB space values
    (B, G, R) = cv2.split(image.astype("float"))
    L = np.absolute(0.2126*R + 0.7152*G + 0.0722*B)
    # In previous research we find two common formulas for luminance, since we find that for our example the results are extremely similar we choose the one above
    #L = np.absolute(0.299*R + 0.587*G + 0.114*B)
    (LMean, LStd) = (np.mean(L), np.std(L))
    meanRoot = np.sqrt((LMean ** 2))
    return meanRoot

# Create function that calculates the standard deviation of the luminance for all pixels in an image to determine that image's contrast
def standarddeviation_luminance(image):
    # Get the image's RGB space values
    (B, G, R) = cv2.split(image.astype("float"))
    L = np.absolute(0.2126*R + 0.7152*G + 0.0722*B)
    # In previous research we find two common formulas for luminance, since we find that for our example the results are extremely similar we choose the one above
    #L = np.absolute(0.299*R + 0.587*G + 0.114*B)
    (LMean, LStd) = (np.mean(L), np.std(L))
    stdRoot = np.sqrt((LStd ** 2))
    return stdRoot

# Create function that quantifies colorfulnes based on Hasler and Suesstrunk's (2003) framework
def image_colorfulness(image):
    # Get the image's RGB space values
    (B, G, R) = cv2.split(image.astype("float"))
    # Calculate rg (the difference between the red and the green channel)
    rg = np.absolute(R - G)
    # Calculate yb (half of the sum of the red and green channels minus the blue channel)
    yb = np.absolute(0.5 * (R + G) - B)
    # Calculate the mean and standard deviation of `rg` and `yb`
    (rbMean, rbStd) = (np.mean(rg), np.std(rg))
    (ybMean, ybStd) = (np.mean(yb), np.std(yb))
    # Now combine the mean and standard deviations, needed for the colorfulness metric
    stdRoot = np.sqrt((rbStd ** 2) + (ybStd ** 2))
    meanRoot = np.sqrt((rbMean ** 2) + (ybMean ** 2))
    # Calculate and return the colorfulness metric
    return stdRoot + (0.3 * meanRoot)

# Create function that calculates the sharpness of an image
def sharpness(image):
    # Calculate the variance of the Laplacian of an image
    return cv2.Laplacian(image, cv2.CV_64F).var()



# Since we only need to run this script for a single map of images I prefer to specify the path here myself
#Instead of using the argument parser we simply hard copy the directory where the images are stored
#From this file map we make a list of all the images' directories
# Load another package
import csv

imagefolder = list(paths.list_images("/Users/mauritshilverda/Documents/Studie/MSc/THESIS/python/airbnbscraper/Scraped_Listing_Images/jpgFiles"))

# Create empty results list needed to store the manipulated images with the colorfulness metric
results = []

with open('OutputImageAnalysis3.csv', 'w') as csvfile:        
    filewriter = csv.writer(csvfile,delimiter=',',quotechar='|',quoting=csv.QUOTE_MINIMAL)
    filewriter.writerow(['imagepath', 'Colorfulness', 'Luminance', 'Contrasts', 'Sharpness'])
    for imagePath in imagefolder:
    # load the image, resize it (to speed up computation), and
    # compute the colorfulness metric for the image
        filewriter = csv.writer(csvfile,delimiter=',',quotechar='|',quoting=csv.QUOTE_MINIMAL)
        image = cv2.imread(imagePath)
        # Store a grey version of the image for calculating the sharpness and do not resize this version (because that may change the sharpness)
        print(imagePath,":")
        image = imutils.resize(image, width=250)
        greyimage = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
        image_colorf = image_colorfulness(image)
        avg_luminance = average_luminance(image)
        standarddev_luminance = standarddeviation_luminance(image)
        sharp = sharpness(greyimage)
        # Write output to a CSV
        filewriter.writerow([imagePath,image_colorf,avg_luminance,standarddev_luminance,sharp])  
        # display the colorfulness score on the image
        cv2.putText(image, "{:.2f}".format(C), (40, 40), cv2.FONT_HERSHEY_SIMPLEX, 1.4, (0, 255, 0), 3)    
        # add the image and colorfulness metric to the results list
        results.append((image, image_colorf))
        
# Check if results are properly stored        
#print(results)

# sort the results with more colorful images at the front of the
# list, then build the lists of the most and least colorful images
print("Displaying results...")
results = sorted(results, key=lambda x: x[1], reverse=True)

mostColor = [r[0] for r in results[:25]]
leastColor = [r[0] for r in results[-25:]][::-1]

# Construct the montages for the two sets of images
mostColorMontage = build_montages(mostColor, (128, 128), (5, 5))
leastColorMontage = build_montages(leastColor, (128, 128), (5, 5))

# View the monatages
cv2.imshow("Most Colorful", mostColorMontage[0])
cv2.imshow("Least Colorful", leastColorMontage[0])
cv2.waitKey(0)
