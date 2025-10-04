# Display the statistic (in term of surface) of each verilog modules of the
# design

import json
import re

file = open("./build/mkTop.json", "r")
file = json.loads(file.read())

class Module:
    def __init__(self, names):
        self.count = 0
        self.patterns = [re.compile(".*" + n + ".*") for n in names]
        self.types = {}
        self.name = names[0]

    def match(self, name, typ):
        if any((p.match(name) for p in self.patterns)):
            self.count += 1

            if not typ in self.types:
                self.types[typ] = 0
            self.types[typ] += 1

            return True
        else:
            return False

def match(l, name, typ):
    return any([x.match(name, typ) for x in l])

def getAll(tree):
    if type(tree) is str:
        return [tree]

    out = [tree[0]]
    for i in range(1,len(tree)):
        out += getAll(tree[i])
    return out

def buildModule(prefix, tree):
    if type(tree) is str:
        return [Module([prefix+"*"+tree if len(tree) > 0 else prefix])]

    modules = [Module([prefix+"*"+x if len(x) > 0 else prefix for x in getAll(tree)])]
    for i in range(1,len(tree)):
        modules += buildModule(prefix, tree[i])
    return modules


modules = [
        "exec",
        "decode",
        "schedule",
        "write_back",
        "register_read",
        "xbar",
        "ram",
        "fetch",
        "broadcast",
        "DEST_Register",
        "BlockRAMQuad",
        "BlockRAMDual",
        "BlockRAMBE",
        "BlockRAMDualBE",
        "memory_slave",
        ]

modules=  [Module([m]) for m in modules]

for name in file["modules"]["mkTopULX3S"]["cells"]:

    if match(modules, name, file["modules"]["mkTopULX3S"]["cells"][name]["type"]):
        pass
    else:
        print(name, file["modules"]["mkTopULX3S"]["cells"][name]["type"])

for m in modules:
    if m.count > 0:
        print(m.name)

        for t in m.types:
            print("\t| {}: {}".format(t, m.types[t]))

        print("\t+---------\n\ttotal: {}".format(m.count))
