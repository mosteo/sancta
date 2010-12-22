#!/usr/bin/python

import math
import random

def printcar(x, y, a, color, bitmap):
	rx = round (x + random.uniform (-0.2, 0.2), 2)
	ry = round (y + random.uniform (-0.2, 0.2), 2)
        ra = round (a + random.uniform (-3.0,  3.0), 2)
	print 'car ('
	print '   color "' + color + '"'
	print '   pose [' + str(rx) + ' ' + str(ry) + ' ' + str(ra) + ']'
        print '   bitmap "' + bitmap + '"'
	print ')'
	print

# MAIN

colors=["red", "blue", "black", "DarkSlateGray", "DarkRed", "DarkBlue", 
        "DarkGreen", "DarkMagenta", "DarkCyan","DarkOrchid1",
        "maroon1", "maroon4", "VioletRed1", "magenta1", "pink1", "cyan1",
        "DarkOrange", "chocolate1", "SeaGreen", "green", "aquamarine4",
        "turquoise1", "DeepSkyBlue1", "SteelBlue1", "navy"]

bitmaps=["../../cars/car1.png",
         "../../cars/car2.png",
         "../../cars/car3.png",
         "../../cars/car4.png",
         "../../cars/car5.png"]

bitmapsr=["../../cars/car1r.png",
         "../../cars/car2r.png",
         "../../cars/car3r.png",
         "../../cars/car4r.png",
         "../../cars/car5r.png"]

def getcolor():
	return colors [random.randint (0, len (colors) - 1)]

def getbitmap():
	return bitmaps [random.randint (0, len (bitmaps) - 1)]

def getbitmapr():
	return bitmapsr [random.randint (0, len (bitmapsr) - 1)]

for i in range(-37, 32, 3):
	printcar(i, -23, 0.0, getcolor (), getbitmap ())

	printcar(i, -11, 0.0, getcolor (), getbitmap ())
	printcar(i, -6, 0.0, getcolor (), getbitmap ())

	printcar(i, 6, 0.0, getcolor (), getbitmap ())
	printcar(i, 11, 0.0, getcolor (), getbitmap ())

	printcar(i, 23, 0.0, getcolor (), getbitmap ())

for i in range(-14, 24, 3):
#	printcar(-43, i,  90.0, getcolor (), getbitmap ())
	printcar( 41, i, -90.0, getcolor (), getbitmap ())
