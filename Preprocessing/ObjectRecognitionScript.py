#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun  8 11:09:21 2020

@author: mauritshilverda
"""


# Load required packages
import numpy as np
import argparse
import time
import cv2
import csv
import os
from imutils import build_montages
from imutils import paths
import imutils

os.getcwd()
os.chdir("/Users/mauritshilverda/Documents/Studie/MSc/THESIS/python/yolo")

# Import the coco.names 80 predefined labels and store this as a list
labelsPath = "/Users/mauritshilverda/Documents/Studie/MSc/THESIS/python/yolo/classes.txt"
LABELS = open(labelsPath).read().strip().split("\n")

# Use different colors that represent each label
np.random.seed(42)
COLORS = np.random.randint(0, 255, size=(len(LABELS), 3), dtype="uint8")

# Specify the location of the YOLOV3 pre-trained model and weigths 
weightsPath = "/Users/mauritshilverda/Documents/Studie/MSc/THESIS/python/yolo/yolov3.weights"
configPath = "/Users/mauritshilverda/Documents/Studie/MSc/THESIS/python/yolo/yolov3.cfg"

# Activate the YOLO object detector trained on COCO dataset with 80 different classes
print("[INFO] loading YOLO from disk...")
net = cv2.dnn.readNetFromDarknet(configPath, weightsPath)

# Determine only the *output* layer names that we need from YOLO
ln = net.getLayerNames()
ln = [ln[i[0] - 1] for i in net.getUnconnectedOutLayers()]

# Create function that 
def results(hello):
    # construct a blob from the input image and then perform a forward
    # pass of the YOLO object detector, giving us our bounding boxes and
    # associated probabilities
    # Create variable for the path since we use this to identify the listings later
    path = hello
    # Import image
    image = cv2.imread(hello)
    #Rescale image to speed up analysis and create blob
    blob = cv2.dnn.blobFromImage(image, 1 / 255.0, (416, 416), swapRB=True, crop=False)
    net.setInput(blob)
    start = time.time()
    layerOutputs = net.forward(ln)
    end = time.time()
    print("[INFO] YOLO took {:.6f} seconds".format(end - start))
    (H, W) = image.shape[:2]
    # Now write a loop that takes the output of the neural nets and returns the desired information
    for output in layerOutputs:
        for detection in output:
            scores = detection[5:]
            classID = np.argmax(scores)
            confidence = scores[classID]
            boxes = []
            confidences = []
            classIDs = []
            # Only include identified objects if the confidence is at least 50%
            if confidence > 0.5:
                # scale the bounding box coordinates back relative to the
                # size of the image, keeping in mind that YOLO actually
                # returns the center (x, y)-coordinates of the bounding
                # box followed by the boxes' width and height
                box = detection[0:4] * np.array([W, H, W, H])
                (centerX, centerY, width, height) = box.astype("int")
                # use the center (x, y)-coordinates to derive the top and
                # and left corner of the bounding box
                x = int(centerX - (width / 2))
                y = int(centerY - (height / 2))
                # update our list of bounding box coordinates, confidences,
                # and class IDs
                boxes.append([x, y, int(width), int(height)])
                confidences.append(float(confidence))
                classIDs.append(classID) 
                # Apply non-maxima suppression to make sure we only keep the most confident identified objects for overlapping boxes
                # We choose to specify the confidence as 0.5 and the treshhold as 0.3
                idxs = cv2.dnn.NMSBoxes(boxes, confidences, 0.5, 0.3)
                # Only store results for images where at least one observation exists
                if len(idxs) > 0:
                    # Loop over the indexes we are keeping and identify the objects with the corresponding confidence levels
                    for i in idxs.flatten():
                        objects = classIDs[i]
                        conf = confidences[i]
                        # Add the output to a file we define below
                        filewriter.writerow([path, objects, conf])  
                        
                    

# We can first test if the codes works for one photo before starting with the heavy workload
#image2 = cv2.imread("/Users/mauritshilverda/Documents/Studie/MSc/THESIS/python/airbnbscraper/Scraped_Listing_Images/jpgFiles/20867625.jpg")
#b = "/Users/mauritshilverda/Documents/Studie/MSc/THESIS/python/airbnbscraper/Scraped_Listing_Images/jpgFiles/20867625.jpg"
#results(b)
#results(image2)
#print(finalresults)

# Specify the path where all our listing images are stored
imagefolder = list(paths.list_images("/Users/mauritshilverda/Documents/Studie/MSc/THESIS/python/airbnbscraper/Scraped_Listing_Images/jpgFiles"))

# Now we can create a file that will contain the output and we can use a loop the perform our previously defined function for all images in the specified map
# For each image, the results are stored in a single CSV file which is automatically written and exported
with open('OutputImageAnalysis100.csv', 'w') as csvfile:        
    filewriter = csv.writer(csvfile,delimiter=',',quotechar='|',quoting=csv.QUOTE_MINIMAL)
    # Give the columns recognizable names
    filewriter.writerow(['imagepath', 'Label', 'Confidence'])
    for imagePath in imagefolder:
        filewriter = csv.writer(csvfile,delimiter=',',quotechar='|',quoting=csv.QUOTE_MINIMAL)
        #We print the imagepath to track the progress while the code is running
        print(imagePath,":")
        results(imagePath)
        

                   