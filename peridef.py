# -*- coding: utf-8 -*-
"""
   Copyright Manuel Freiberger 2013.
   Distributed under the Boost Software License, Version 1.0.
   (See accompanying file LICENSE_1_0.txt or copy at
   http://www.boost.org/LICENSE_1_0.txt)
"""

import os, os.path
import pyparsing

# ----======================================================================----
#     Parse tree-related
# ----======================================================================----

class Peripheral(object):
    def __init__(self, name):
        self.name = name
        self.children = []

    def addChild(self, c):
        self.children.append(c)


class Register(object):
    def __init__(self):
        self.name = None
        self._size = None
        self.bitFields = []
        self.arraySize = 1
        self.parameters = []
        self.type = None

    def __str__(self):
        if self.name:
            return '%s:%d' % (self.name, self.size)
        else:
            return '<reserved>:%d' % self.size

    def addBitField(self, b):
        self.bitFields.append(b)

    @property
    def align(self):
        if self.isTupleReference:
            return self.type.align
        else:
            return self.size

    @property
    def isReserved(self):
        return self.name is None

    @property
    def isTupleReference(self):
        return self.type is not None

    def setParameters(self, params):
        for p in params:
            if p[0] == "arraySize":
                self.arraySize = int(p[1], 0)
            else:
                self.parameters.append((p[0], p[1]))

    @property
    def size(self):
        if self.isTupleReference:
            return self.type.size
        else:
            return self._size

    @staticmethod
    def createFromSize(name, size):
        r = Register()
        r._size = size
        r.name = name
        return r

    @staticmethod
    def createFromType(name, type):
        r = Register()
        r.name = name
        r.type = type
        return r

    @staticmethod
    def createReserved(size):
        r = Register()
        r._size = size
        return r


class BitField(object):
    def __init__(self, size, name=None):
        self.name = None
        self.size = size
        self.values = []

    def __str__(self):
        if self.name:
            return '%s:%d' % (self.name, self.size)
        else:
            return '<reserved>:%d' % self.size

    def addValue(self, name, value):
        self.values.append((name, value))
        
    @property
    def isReserved(self):
        return self.name is None

    @staticmethod
    def create(name, size):
        r = BitField(size)
        r.name = name
        return r

    @staticmethod
    def createReserved(size):
        r = BitField(size)
        return r


class Tuple(object):
    def __init__(self, name):
        self.name = name
        self.children = []

    def addChild(self, c):
        self.children.append(c)

    @property
    def align(self):
        a = 0
        for c in self.children:
            a = max(a, c.align)
        return a

    @property
    def size(self):
        s = 0
        for c in self.children:
            s += c.size
        return s


class Address(object):
    def __init__(self, name, base):
        self.name = name
        self.base = base
        self.offset = None

    def setOffset(self, ofs):
        self.offset = ofs


class Map(object):
    def __init__(self, name, type, addr):
        self.name = name
        self.type = type
        self.address = addr
        self.offset = None

    def setOffset(self, ofs):
        self.offset = ofs



def toAst(tokens):
    result = []
    tuples = dict()
    for t in tokens:
        if t[0] == "peripheral":
            result.append(toPeripheral(t))
        elif t[0] == "tuple":
            tpl = toTuple(t)
            result.append(tpl)
            tuples[tpl.name] = tpl
        elif t[0] == "address":
            result.append(toAddress(t))
        elif t[0] == "map":
            result.append(toMap(t))
        else:
            raise Exception("Unknown token %s" % t[0])
    # Resolve references to tuples
    for p in result:
        if not isinstance(p, Peripheral):
            continue
        for r in p.children:
            if r.isTupleReference:
                r.type = tuples[r.type]

    return result

def toPeripheral(t):
    assert(t[0] == "peripheral")
    p = Peripheral(t["name"])
    for c in t["children"]:
        p.addChild(toRegister(c))
    return p

def toRegister(t):
    if t[0] == "reserved":
        r = Register.createReserved(int(t["size"], 0))
    else:
        assert(t[0] == "register")
        if "size" in t.keys():
            r = Register.createFromSize(t["name"], int(t["size"], 0))
        else:
            r = Register.createFromType(t["name"], t["type"])
    if "parameters" in t.keys():
        r.setParameters(t["parameters"])
    if "body" in t.keys():
        for b in t["body"][0]:
            r.addBitField(toBitField(b))
    return r

def toBitField(t):
    if t[0] == "reserved":
        b = BitField.createReserved(int(t["size"], 0))
    else:
        assert(t[0] == "bit")
        if "size" in t:
            size = int(t["size"], 0)
        else:
            size = 1
        b = BitField.create(t["name"], size)
        if "body" in t.keys():
            for nvp in t["body"][0]:
                b.addValue(nvp[0], int(nvp[1], 0))
    return b

def toTuple(t):
    assert(t[0] == "tuple")
    res = Tuple(t["name"])
    for c in t["children"]:
        res.addChild(toRegister(c))
    return res

def toAddress(t):
    assert(t[0] == "address")
    a = Address(t["name"], t["base"])
    if "offset" in t.keys():
        a.setOffset(t["offset"])
    return a

def toMap(t):
    assert(t[0] == "map")
    m = Map(t["name"], t["type"], t["base"])
    if "offset" in t.keys():
        m.setOffset(t["offset"])
    return m


# ----======================================================================----
#     Parser
# ----======================================================================----
def parseDefinitionFile(source):
    comma = pyparsing.Literal(',').suppress();
    lbrace = pyparsing.Literal('{').suppress();
    rbrace = pyparsing.Literal('}').suppress();
    lparen = pyparsing.Literal('(').suppress();
    rparen = pyparsing.Literal(')').suppress();
    equal = pyparsing.Literal('=').suppress();

    integer = pyparsing.Word(pyparsing.nums)
    binNumber = pyparsing.Combine(pyparsing.Literal('0b')
                                  + pyparsing.Word('01'))
    hexNumber = pyparsing.Combine(pyparsing.Literal('0x')
                                  + pyparsing.Word(pyparsing.hexnums))
    number = integer ^ binNumber ^ hexNumber

    identifier = pyparsing.Word(pyparsing.alphas + '_',
                                pyparsing.alphanums + '_');

    literal = identifier ^ number

    keyValuePair = pyparsing.Group(
                       identifier
                       + equal
                       + pyparsing.CharsNotIn(',)'))
    keyValueList= pyparsing.Group(
                      keyValuePair
                      + pyparsing.ZeroOrMore(comma + keyValuePair))

    nameValuePair = pyparsing.Group(
                        identifier 
                        + equal
                        + number)

    bitFieldBody = pyparsing.Group(
                       lbrace
                       + pyparsing.OneOrMore(nameValuePair)
                       +rbrace)
    bitField = pyparsing.Group(
                   pyparsing.Literal("bit")
                   + lparen
                   + identifier("name")
                   + pyparsing.Optional(
                         comma + number("size"))
                   + rparen
                   + pyparsing.Optional(bitFieldBody)("body"))
    reservedBit = pyparsing.Group(
                      pyparsing.Literal("reserved")
                      + lparen
                      + number("size")
                      + rparen)

    registerBody = pyparsing.Group(
                       lbrace
                       + pyparsing.OneOrMore(bitField | reservedBit)
                       + rbrace)
    register = pyparsing.Group(
                   pyparsing.Literal("register")
                   + lparen
                   + identifier("name")
                   + comma
                   + (number("size") | identifier("type"))
                   + pyparsing.Optional(comma + keyValueList("parameters"))
                   + rparen
                   + pyparsing.Optional(registerBody)("body"))
    reservedRegister = pyparsing.Group(
                           pyparsing.Literal("reserved")
                           + lparen
                           + number("size")
                           + pyparsing.Optional(comma + keyValueList("parameters"))
                           + rparen)

    _tuple = pyparsing.Group(
                 pyparsing.Literal("tuple")
                 + lparen
                 + identifier("name")
                 + pyparsing.Optional(comma + keyValueList("parameters"))
                 + rparen
                 + lbrace
                 + pyparsing.OneOrMore(register
                                       | reservedRegister)("children")
                 + rbrace)

    peripheral = pyparsing.Group(
                     pyparsing.Literal("peripheral")
                     + lparen
                     + identifier("name")
                     + pyparsing.Optional(comma + keyValueList)
                     + rparen
                     + lbrace
                     + pyparsing.OneOrMore(register
                                           | reservedRegister)("children")
                     + rbrace)

    address = pyparsing.Group(
                  pyparsing.Literal("address")
                  + lparen
                  + identifier("name")
                  + comma
                  + literal("base")
                  + pyparsing.Optional(comma + literal("offset"))
                  + rparen)

    mapping = pyparsing.Group(
                  pyparsing.Literal("map")
                  + lparen
                  + identifier("type")
                  + comma
                  + identifier("name")
                  + comma
                  + literal("base")
                  + pyparsing.Optional(comma + literal("offset"))
                  + rparen)

    definitionFile = pyparsing.OneOrMore(peripheral
                                         | _tuple
                                         | address
                                         | mapping)
    definitionFile.ignore(pyparsing.cStyleComment)
    definitionFile.ignore(pyparsing.cppStyleComment)

    return definitionFile.parseString(source, parseAll=True)


# ----======================================================================----
#     Validation
# ----======================================================================----
class Validator(object):
    def validate(self, instr):
        for i in instr:
            if isinstance(i, Peripheral):
                self.validatePeripheral(i)

    def validatePeripheral(self, p):
        offset = 0
        for c in p.children:
            if offset % c.size != 0:
                raise Exception("Unaligned register %s in %s"
                                % (c.name, p.name))
            self.validateRegister(p, c)
            offset += c.size * c.arraySize
        if offset % 32 != 0:
            raise Exception("Register map of %s needs padding" % p.name)

    def validateRegister(self, p, r):
        if not r.bitFields:
            return
        bitSize = 0
        for b in r.bitFields:
            bitSize += b.size
        if bitSize != r.size:
            raise Exception("Register %s in %s has size % d but sum of bit fields is %d"
                            % (r.name, p.name, r.size, bitSize))

# ----======================================================================----
#     Printer
# ----======================================================================----
class CFilePrinter(object):
    def __init__(self, fileName):
        self.fileName = fileName

    def handle(self, instr):
        s = ""

        s += "/*\n"
        s += "   Copyright Manuel Freiberger 2013.\n"
        s += "   Distributed under the Boost Software License, Version 1.0.\n"
        s += "   (See accompanying file LICENSE_1_0.txt or copy at\n"
        s += "   http://www.boost.org/LICENSE_1_0.txt)\n*/\n\n"

        bn = os.path.basename(self.fileName)
        guard = "PERIDEF_" + bn.replace('.', '_').upper()
        s += "#ifndef %s\n" % guard
        s += "#define %s\n\n" % guard
        s += "#include <stdint.h>\n\n"

        s += '#ifdef __cplusplus\n'
        s += 'extern "C" {\n'
        s += '#endif\n\n'

        for i in instr:
            if isinstance(i, Peripheral) or isinstance(i, Tuple):
                s += self.printPeripheralType(i)
        s += '\n'

        for i in instr:
            if isinstance(i, Address):
                s += self.printAddress(i)
        s += '\n'

        for i in instr:
            if isinstance(i, Map):
                s += self.printMap(i)
        s += '\n'

        for i in instr:
            if isinstance(i, Peripheral):
                s += self.printPeripheralBitFields(i)
        s += '\n'

        s += '#ifdef __cplusplus\n'
        s += '} /* extern "C" */\n'
        s += '#endif\n\n'

        s += "#endif /* %s */\n" % guard
        return s

    def printPeripheralType(self, p):
        s = "typedef struct\n{\n"
        reservedCount = 0
        for r in p.children:
            typeName = self.registerType(r)
            arraySize = ""
            if r.isReserved:
                name = "reserved%d" % reservedCount
                reservedCount += 1
            else:
                name = r.name
            if r.arraySize > 1:
                arraySize = "[%d]" % r.arraySize
            s += "    %s %s%s;\n" % (typeName, name, arraySize)
        s += "} %s_TypeDef;\n\n" % p.name
        return s

    def printAddress(self, a):
        addr = self.castedAddress(a.base)
        if a.offset:
            ofs = self.castedAddress(a.offset)
            addr += " + " + ofs

        s = "#define {0:<30} ({1})\n".format(a.name, addr)
        return s

    def printMap(self, m):
        addr = self.castedAddress(m.address)
        if m.offset:
            ofs = self.castedAddress(m.offset)
            addr += " + " + ofs
        s = "#define {0:<30} (({1}_TypeDef*)({2}))\n".format(
                m.name, m.type, addr)
        return s

    def printPeripheralBitFields(self, p, prefix=None):
        s = ""
        if prefix is None:
            prefix = p.name
        for r in p.children:
            if not r.isReserved:
                s += self.printRegisterBitFields(r, prefix)
        return s

    def printRegisterBitFields(self, r, prefix):
        if r.isTupleReference:
            return self.printPeripheralBitFields(r.type, prefix)

        offset = 0
        s = ""
        prefix += '_' + r.name
        for b in r.bitFields:
            if not b.isReserved:
                mask = ((1 << b.size) - 1) << offset
                fmt = '((uint%%d_t)0x%%0%dX)' % (r.size / 4)
                s += "#define {0:<30} {1}\n".format(
                         prefix + '_' + b.name, fmt % (r.size, mask))
                vs = ""
                for v in b.values:
                    vs += "#define {0:<30} {1}\n".format(
                              prefix + '_' + b.name + '_' + v[0],
                              fmt % (r.size, v[1] << offset))
                if vs:
                    s += vs + '\n'
            offset += b.size
        if s:
            s = "/* Bit fields of the {0} register */\n".format(
                    prefix) + s + '\n'
        return s

    @staticmethod
    def castedAddress(s):
        try:
            l = long(s, 0)
            addr = "(uintptr_t)0x%08X" % l
        except ValueError:
            addr = str(s)
        return addr

    @staticmethod
    def registerType(r):
        s = ""
        if not r.isReserved and not r.isTupleReference:
            s += "volatile "
        if r.isTupleReference:
            s += r.type.name + "_TypeDef"
        else:
            s += "uint%d_t" % r.size
        return s


# ----======================================================================----
#     main
# ----======================================================================----

# Testing
fileName = r"stm32f4xx.def"
with open(fileName) as f:
    t = parseDefinitionFile(f.read())

instr = toAst(t)
validator = Validator()
validator.validate(instr)

outFileName = r'stm32f4xx.h'
with open(outFileName, 'w') as f:
    printer = CFilePrinter(outFileName)
    s = printer.handle(instr)
    f.write(s)
