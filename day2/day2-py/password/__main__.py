#!/usr/bin/env python3

from . import problem1
from . import problem2
import fileinput

def main():
    records = list(fileinput.input())

    print("problem 1:"); problem1.main(records)
    print("problem 2:"); problem2.main(records)
    
if  __name__ == '__main__':
    main()
