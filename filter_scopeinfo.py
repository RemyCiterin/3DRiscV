#!/usr/bin/env python

import json

FILE_NAME = "build/mkTop.json"

file = open(FILE_NAME, "r")
content = json.loads(file.read())
file.close()

paths = []

for module in content["modules"]:
    if "cells" in content["modules"][module]:
        cells = content["modules"][module]["cells"]
        for name in cells:

            if "type" in cells[name]:
                if cells[name]["type"] == "$scopeinfo":
                    paths.append(["modules", module, "cells", name])


def remove_path(map, path):
    for i in range(len(path)-1):
        map = map[path[i]]
    map.pop(path[-1])

for path in paths:
    remove_path(content, path)

with open(FILE_NAME, 'w', encoding='utf-8') as f:
    json.dump(content, f, ensure_ascii=False, indent=2)

