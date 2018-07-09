#!/usr/bin/python

import os
import csv
import logging

logging.basicConfig(
	level=logging.INFO,
	format='%(asctime)s %(levelname)s %(message)s'
)
lgr = logging.getLogger(__name__)
linksFilepath = "links.tsv"
projectDir =  os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

print projectDir

def main():
	linkFiles()

def linkFiles():
	section = ''
	with open(linksFilepath) as linksFile:
		for line in csv.reader(decomment(linksFile)):
			
			# Log a message for each new program
			if line[0] != section:
				section = line[0]
				lgr.info("Creating symlinks for " + section)
			
			# Create symlinks
			if len(line) < 3:
				lgr.warning("Unable to parse line: [" + ", ".join(line) + "]")
				continue

			source = os.path.join(projectDir, line[1])
			destination = os.path.expanduser(line[2])
			
			if os.path.lexists(destination):
				lgr.warning("Overwriting pre-existing file at " + source)
				os.remove(destination)

			lgr.info(destination + " -> " + source)
			os.symlink(source, destination)



def decomment(csvfile):
    for row in csvfile:
        raw = row.split("#")[0].strip()
        if raw: yield raw
		


if __name__ == "__main__":
	main()


