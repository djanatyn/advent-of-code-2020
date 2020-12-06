#!/usr/bin/env python3

import re


class Record(object):
    def __init__(self, lowerBound, upperBound, matchChar, matchText):
        self.lowerBound = lowerBound
        self.upperBound = upperBound
        self.matchChar = matchChar
        self.matchText = matchText


def parse(line):
    regex = r"(\d+)\-(\d+) (\w)\: (\w+)$"
    match = re.match(regex, line)

    return Record(
        int(match.groups()[0]),
        int(match.groups()[1]),
        match.groups()[2],
        match.groups()[3],
    )


def validate(record):
    occurrences = record.matchText.count(record.matchChar)
    return record.lowerBound <= occurrences <= record.upperBound


def load(records):
    return [parse(line) for line in records]


def main(records):
    validation = [validate(record) for record in load(records)]

    print(f"valid records: {validation.count(True)}")
    print(f"invalid records: {validation.count(False)}")


if __name__ == "__main__":
    main()
