#!/usr/bin/env python
# reducer.py

"""
Before running, make sure you set the scripts folder and output
folder paths below.
Your Mapper.py and Reducer.py should both be within the scripts
folder of your s3 account and must be named 'Mapper.py' and 
'Reducer.py' (case sensitive).
The output folder is where the map/reduce resultant files will
be placed.
"""

## SCRIPT AND OUTPUT FOLDER PATHS MUST BE
## CHANGED PRIOR TO RUNNING THIS SCRIPT

scriptsPath =  "s3://mapperarnone/"
outputPath = "s3://outputarnone/test2/"




import sys
import re
import os


def main():
    scriptsPathCheck()
    inputFileName = getUserInputFile()
    outputFileName = getUserOutputFile()
    if not detectExistingOutputFile(outputFileName):
        with open("./" + inputFileName) as infile:
            with open("./" + outputFileName + '.json', 'w+') as outfile:

                # Begin a boolean flag
                isFirstLine = True
                lastLine = infile.readline();
                lastLine == False;
                # Print the JSON file header markup
                outfile.write("[\n")
                lineCounter = 0
                # Check for non-URL entries in input file
                for line in infile:
                           # if len(line) < 10:
                           #     print ("Short line in input file, please check/rid your input file of extra blank lines. Exiting...")
                           # sys.exit()
                    # Parse the elements out of the line
                            linesplit=line.split("/")
                            ccMainNumber = linesplit[4]
                            segmentnumber = linesplit[6]
                            filerange = linesplit[8].rstrip()

                           # if(isFirstLine == False):
                           #     outfile.write (",\n")
                           # isFirstLine = False
                    # Print the markup for each line
                            outfile.write ("{\n")
                            outfile.write ("\"Name\": \"segment_" + segmentnumber + "_file_" + filerange.split("-")[4].split(".")[0]  + "\",\n")
                            outfile.write ("\"ActionOnFailure\": \"CONTINUE\",\n")
                            outfile.write ("\"Jar\": \"/usr/lib/hadoop-mapreduce/hadoop-streaming.jar\",\n")
                            outfile.write ("\"Args\":\n")
                            outfile.write ("[\n")
                            outfile.write ("\"-files\", " + "\"" + scriptsPath + "Mapper.py," + scriptsPath + "Reducer.py\",\n")
                            outfile.write ("\"-mapper\", \"Mapper.py\",\n")
                            outfile.write ("\"-reducer\", \"Reducer.py\",\n")
                            outfile.write ("\"-input\", \"s3://commoncrawl/crawl-data/" + ccMainNumber + "/segments/" + segmentnumber + "/wet/" + filerange + "\",\n")
                            outfile.write ("\"-output\"" + ", " + "\"" + outputPath + segmentnumber + "_" + filerange.split("-")[4].split(".")[0] + "\"" + ",\n")
                            outfile.write ("\"-inputformat\", \"TextInputFormat\"\n")
                            outfile.write ("]\n")
                            outfile.write ("},\n")
                    
                            lineCounter += 1
                            print (ccMainNumber)
                            print ("\nSuccessfully wrote", lineCounter, "lines to output file:", outputFileName)

def getUserInputFile():
    file = input("Enter input file (ex: name.bdf): ")
    return file

def getUserOutputFile():
    file = input("Enter output file (dont add the .json extention) ")
    return file

def detectExistingOutputFile(file):
    if os.path.isfile(file + '.json'):
        decision = input("\nWARNING! If you proceed, you will overwrite your output file. Are you sure?\n[y/n]\n")
        if decision == "n":
            print ("Exiting")
            sys.exit()
        elif decision =="y":
            return False
    else:
        return False

def exitMessage():
    print ("\nSuccessfully wrote"), lineCounter, "lines to output file:", outputFileName

def scriptsPathCheck():
    if scriptsPath == "s3://[S3 Bucket]/[Scripts Folder]/":
        print ("You need to open and edit the script path of createJsonFiles.py file prior to running. Exiting...")
        sys.exit()
    elif outputPath == "s3://[S3 Bucket]/[EMR Output Folder]/":
        print ("You need to open and edit the output path of createJsonFiles.py file prior to running. Exiting...")
        sys.exit()

if __name__ == "__main__":
        main()

