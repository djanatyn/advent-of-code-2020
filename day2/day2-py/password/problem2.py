#!/usr/bin/env python3

import re


class Record(object):
    def __init__(self, index1, index2, matchChar, matchText):
        self.indexes = [index1, index2]
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
    matches = [record.matchText[index - 1] for index in record.indexes]
    return matches.count(record.matchChar) == 1


def load(records):
    return [parse(line) for line in records]


def main(records):
    validation = [validate(record) for record in load(records)]

    print(f"valid records: {validation.count(True)}")
    print(f"invalid records: {validation.count(False)}")


if __name__ == "__main__":
    main()
