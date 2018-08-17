#!/usr/bin/env python3

# Converts a Kelvin temperature to a Hex colour
# Adapted from an implementation by Neil Bartlett, originally implemented by Tanner Helland
# http://www.zombieprototypes.com/?p=210

from __future__ import print_function
from math import pow, log
import sys

def eprint(*args, **kwargs):
    """Print a series of strings to stderr."""
    print(*args, file=sys.stderr, **kwargs)

def clamp(n):
    """Take a floating point value and return the nearest integer bounded within [0, 255]"""
    return max(min(int(round(n)), 255), 0)

def rgb_to_hex(rgb):
    return '#%02x%02x%02x' % rgb

def calculate_red(temperature):
    if temperature < 6600:
        return 255

    return 329.698727446 * pow(temperature / 100 - 60, -0.1332047592)

def calculate_green(temperature):
    if temperature <= 6600:
        return 99.4708025861 * log(temperature / 100) - 161.1195681661

    return 288.1221695283 * pow(temperature / 100 - 60, -0.0755148492)

def calculate_blue(temperature):
    if temperature >= 6600:
        return 255
    if temperature <= 1900:
        return 0

    return 138.5177312231 * log(temperature / 100 - 10) - 305.0447927307

def main(argv):
    """Convert a Kelvin temperature to an RGB string."""
    if len(argv) != 2:
        eprint("Usage: kelvin_colour <temperature>")
        return 1

    if not argv[1].isdigit():
        eprint("Temperature must be a positive integer")
        return 2

    temperature = int(argv[1])
    floatColours = [f(temperature) for f in [calculate_red, calculate_blue, calculate_green]]
    intColours = tuple([clamp(colour) for colour in floatColours])

    print(rgb_to_hex(intColours))
    return 0

if __name__ == "__main__":
    sys.exit(main(sys.argv))
