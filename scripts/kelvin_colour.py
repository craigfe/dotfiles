#!/usr/bin/env python3

# Converts a Kelvin temperature to a Hex colour
# Adapted from an implementation by Neil Bartlett, originally implemented by Tanner Helland
# http://www.zombieprototypes.com/?p=210

from __future__ import print_function
import sys

def eprint(*args, **kwargs):
    """Print a series of strings to stderr."""
    print(*args, file=sys.stderr, **kwargs)

def main(argv):
    """Convert a Kelvin temperature to an RGB string."""
    if len(argv) != 2:
        eprint("Usage: kelvin_colour <temperature>")
        return 1

    print(argv[0])
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
