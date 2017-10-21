
import sys
import re


# class Byte
#
# Class of 8-bit values that can be interpreted as signed or
# unsigned integer bytes.
#
# Supports the operations +, -, &, ^, <<1, >>1
#
class Byte:
    
    # These words are 8 bits wide.
    size = 8

    # b = Byte(v)
    # 
    # Make a byte.  v is either another Byte, an integer,
    # a decimal digit string, a hexadecimal digit string,
    # or a binary digit string.
    #
    # This code interprets any of those as specifying an
    # an 8-bit quantity,
    #
    def __init__(self,v):

        if type(v) == Byte:
            self.value = v.value
        elif type(v) == int:
            self.value = v
        elif type(v) == str:
            try:
                if v[:2] == '0x' or v[:2] == '0X':
                    self.value = int(v[2:], 16)
                elif v[:2] == '0b' or v[:2] == '0B': 
                    self.value = int(v[2:], 2)
                else:
                    self.value = int(v)
            except ValueError:
                print("Unexpected string for building a byte: '"+v+"'.")
                raise ValueError
                sys.exit(-1);
        else:
            print("Bug in simulator. Built a byte with a weird value.")
            sys.exit(-1)

        self.prep()

    # b.prep()
    # 
    # Used by __init__ to make a byte value from an integer.
    # 
    def prep(self):
        self.value = self.value & ((1 << self.size) - 1)

    #
    # b.signed()
    # b.unsigned()
    #
    # Interpret the integer value as either a signed or unsigned
    # byte.
    #
    def unsigned(self):
        return self.value & ((1 << self.size) - 1)
    #
    #
    def signed(self):
        v = self.unsigned()
        if v >= (1 << (self.size-1)):
            return v - (1 << self.size)
        else:
            return v
    #
    #
    def hex(self):
        space = (self.size + 3) // 4
        return ("%0."+str(space)+"X") % self.unsigned()

    # 
    # Define the "special methods" for performing addition, subtraction,
    # bitwise logic, and arithmetic shifts.
    #

    def __add__(self,other):
        if (type(other) != Byte):
            other = Byte(other)
        return Byte(self.unsigned() + other.unsigned())

    def __sub__(self,other):
        return Byte(self.unsigned() - other.unsigned())

    def __neg__(self):
        return Byte(-self.unsigned())

    def __and__(self,other):
        return Byte(self.unsigned() & other.unsigned())

    def __xor__(self,other):
        return Byte(self.unsigned() ^ other.unsigned())

    def __lshift__(self,positions):
        assert(positions == 1)
        shift = self.unsigned() << 1
        rightmost = 0b11111110
        return Byte(shift & rightmost)

    def __rshift__(self,positions):
        assert(positions == 1)
        leftmost = self.unsigned() & 128
        shift = self.unsigned() >> 1
        return Byte(shift | leftmost)

    # 
    # Represent as a hexadecimal code string, by default.
    #
    __str__  = hex
    __repr__ = hex


def parseHa(file,start):
    def opRE(s):
        return "\s*"+s+"\s*"
    
    s      = "\s*"
    targetRE = "([_a-zA-Z][_a-zA-Z0-9]*)"
    labelRE = "^"+targetRE+":\s*$"
    regRE = "([Rr][a-f]|[Rr][0-9]|[Rr]1[0-5]|ip|IP|sp|SP|pc|PC)"
    regRE = targetRE
    intRE = "(\-?[0-9]+|0x[0-9a-fA-F]+|0b[01]+)"
    uintRE = "([0-9]+|0x[0-9a-fA-F]+|0b[01]+)"
    sintRE = "(\-[1-9][0-9]*)"
    memRE = "mem\s*\[\s*"+regRE+"\s*\]"
    uoffRE = "mem\s*\[\s*"+regRE+"\s*\+\s*"+uintRE+"\s*\]"
    soffRE = "mem\s*\[\s*"+regRE+"\s*"+sintRE+"\s*\]"
    twoRE = "2"
    sRE    = "^\s*"
    eRE    = "\s*$"
    nopRE  = "^\s*$"
    addRE  = sRE+regRE+opRE('=')+regRE+opRE('\+')+regRE+eRE
    subRE  = sRE+regRE+opRE('=')+regRE+opRE('\-')+regRE+eRE
    andRE  = sRE+regRE+opRE('=')+regRE+opRE('&')+regRE+eRE
    xorRE  = sRE+regRE+opRE('=')+regRE+opRE('\^')+regRE+eRE
    movRE  = sRE+regRE+opRE('=')+regRE+eRE
    negRE  = sRE+regRE+opRE('=')+opRE('\-')+regRE+eRE
    aslRE  = sRE+regRE+opRE('=')+regRE+opRE('\*')+twoRE+eRE
    asrRE  = sRE+regRE+opRE('=')+regRE+opRE('//')+twoRE+eRE
    setRE  = sRE+regRE+opRE('=')+intRE+"\s*$"
    incRE  = sRE+regRE+opRE('\+=')+intRE+"\s*$"
    stopRE = sRE+"stop"+eRE
    brnRE  = sRE+"if\s*"+regRE+"\s*<\s*0\s*go\s*to\s+"+targetRE+eRE
    brzRE  = sRE+"if\s*"+regRE+"\s*==\s*0\s*go\s*to\s+"+targetRE+eRE
    braRE  = sRE+"go\s*to\s+"+targetRE+eRE
    getRE  = sRE+regRE+opRE('=')+"get"+eRE
    putRE  = sRE+"put\s+"+regRE+eRE
    stoRE  = sRE+memRE+opRE('=')+regRE+"\s*$"
    loadRE = sRE+regRE+opRE('=')+memRE+"\s*$"
    stOfRE = sRE+uoffRE+opRE('=')+regRE+"\s*$"
    ldOfRE = sRE+regRE+opRE('=')+uoffRE+"\s*$"
    popRE  = sRE+regRE+opRE('=')+"pop"+eRE
    pushRE = sRE+"push\s+"+regRE+eRE
    callRE = sRE+"call\s+"+targetRE+eRE
    rtrnRE = sRE+"return"+eRE

    program = []
    labels  = {}

    for line in file:
        code = line.split('#')[0]
        if re.match(labelRE,code):
            m = re.match(labelRE,code)
            label = m.group(1)
            labels[label] = start + len(program)
        elif re.match(brnRE,code):
            m = re.match(brnRE,code)
            program.append(["BRN",m.group(1),m.group(2)])
        elif re.match(brzRE,code):
            m = re.match(brzRE,code)
            program.append(["BRZ",m.group(1),m.group(2)])
        elif re.match(braRE,code):
            m = re.match(braRE,code)
            program.append(["BRA",m.group(1)])
        elif re.match(callRE,code):
            m = re.match(callRE,code)
            program.append(["CALL",m.group(1)])
        elif re.match(rtrnRE,code):
            m = re.match(rtrnRE,code)
            program.append(["RTS"])
        elif re.match(getRE,code):
            m = re.match(getRE,code)
            program.append(["INPUT",m.group(1)])
        elif re.match(putRE,code):
            m = re.match(putRE,code)
            program.append(["OUTPUT",m.group(1)])
        elif re.match(popRE,code):
            m = re.match(popRE,code)
            program.append(["POP",m.group(1)])
        elif re.match(pushRE,code):
            m = re.match(pushRE,code)
            program.append(["PUSH",m.group(1)])
        elif re.match(loadRE,code):
            m = re.match(loadRE,code)
            program.append(["LOAD","$0",m.group(2),m.group(1)])
        elif re.match(stoRE,code):
            m = re.match(stoRE,code)
            program.append(["STORE",m.group(2),"$0",m.group(1)])
        elif re.match(ldOfRE,code):
            m = re.match(ldOfRE,code)
            program.append(["LOAD","$"+m.group(3),m.group(2),m.group(1)])
        elif re.match(stOfRE,code):
            m = re.match(stOfRE,code)
            program.append(["STORE",m.group(3),"$"+m.group(2),m.group(1)])
        elif re.match(addRE,code):
            m = re.match(addRE,code)
            program.append(["ADD",m.group(2),m.group(3),m.group(1)])
        elif re.match(subRE,code):
            m = re.match(subRE,code)
            program.append(["SUB",m.group(2),m.group(3),m.group(1)])
        elif re.match(andRE,code):
            m = re.match(andRE,code)
            program.append(["AND",m.group(2),m.group(3),m.group(1)])
        elif re.match(xorRE,code):
            m = re.match(xorRE,code)
            program.append(["XOR",m.group(2),m.group(3),m.group(1)])
        elif re.match(movRE,code):
            m = re.match(movRE,code)
            program.append(["MOV",m.group(2),m.group(1)])
        elif re.match(negRE,code):
            m = re.match(negRE,code)
            program.append(["NEG",m.group(2),m.group(1)])
        elif re.match(asrRE,code):
            m = re.match(asrRE,code)
            program.append(["ASR",m.group(2),m.group(1)])
        elif re.match(aslRE,code):
            m = re.match(aslRE,code)
            program.append(["ASL",m.group(2),m.group(1)])
        elif re.match(setRE,code):
            m = re.match(setRE,code)
            program.append(["SET","$"+m.group(2),m.group(1)])
        elif re.match(incRE,code):
            m = re.match(incRE,code)
            program.append(["INC","$"+m.group(2),m.group(1)])
        elif re.match(stopRE,code):
            program.append(["STOP"])
        elif re.match(nopRE,code):
            pass
        else:
            print("Unrecognized instruction '"+code+"'.")

    return program,labels

# class Machine
#
# 
class Machine:

    # M = Machine(nregs,msize)
    # 
    # Create a new Ha75 execution engine.
    #
    # Initialization method(s).  Sets up the list of 
    # registers, a dictionary mapping register names
    # to register numbers, a list of memory bytes,
    #
    def __init__(self, nregs, msize, prmpt=True):       
        self.regFile = [Byte(0) for r in range(nregs)]
        self.buildRegNames()
        self.mem = [Byte(0) for r in range(msize)]
        self.temps = {}
        self.halted = False
        self.prompt = prmpt
        self.program = []
        self.labels = {}
        self.top = 0
        
    #
    #
    def buildRegNames(self):
        self.regNames = {}
        for i in range(len(self.regFile)):
            for rl in ["r","R"]:
                decimal = "%d" % i
                hex = "%x" % i
                HEX = "%X" % i
                for il in [decimal,hex,HEX]:
                    self.nameReg(i,rl+il)
        self.nameReg(14,"SP")
        self.nameReg(14,"sp")
        self.nameReg(15,"IP")
        self.nameReg(15,"ip")
        self.nameReg(15,"PC")
        self.nameReg(15,"pc")
    #
    #
    def nameReg(self, index, name):
        self.regNames[name] = index


    # Register & memory getters/setters.
    #
    # M[reg]
    # M[reg] = b
    # M[addr]
    # M[addr] = b
    #
    # If you have a Machine instance M, you can use notation like
    # M['R7'] to access a register's byte value, and M['R7'] = b to
    # change a register's byte value. 
    #
    # If A is a Byte value representing a memory address, then the
    # notation M[A] fetches a byte at address A from memory and the
    # notation M[A] = b sets a byte at address A to the byte value b.
    #
    def getregister(self, name):
        if self.regNames[name] == 0:
            return Byte(0)
        else:
            return self.regFile[self.regNames[name]]

    def gettemp(self, name):
        if name in self.temps:
            return self.temps[name]
        else:
            return Byte(0)

    #
    #
    def getmemory(self, address):
        return self.mem[address]
    #
    #
    def setregister(self, name, bvalue):
        assert(type(bvalue) == Byte)
        if self.regNames[name] != 0:
            self.regFile[self.regNames[name]] = bvalue
    #
    #
    def settemp(self, name, bvalue):
        assert(type(bvalue) == Byte)
        self.temps[name] = bvalue
    #
    #
    def setmemory(self, address, bvalue):
        assert(type(bvalue) == Byte)
        self.mem[address] = bvalue
    #
    #
    def __getitem__(self,which):
        if type(which) == str:
            if which in self.regNames:
                return self.getregister(which)
            else:
                return self.gettemp(which)
        elif type(which) == int:
            if 0 <= which < len(self.mem):
                return self.getmemory(which)
            else:
                print("ERROR: Invalid memory address.")
        elif type(which) == Byte:
            if 0 <= which.unsigned() < len(self.mem):
                return self.getmemory(which.unsigned())
            else:
                print("ERROR: Invalid memory address.")
        else:
            print("ERROR: Invalid storage reference."+str(which))

    #
    #
    def __setitem__(self,which,value):
        if type(which) == str:
            if which in self.regNames:
                self.setregister(which,Byte(value))
            else:
                self.settemp(which,Byte(value))
        elif type(which) == int:
            if 0 <= which < len(self.mem):
                self.setmemory(which,Byte(value))
            else:
                print("ERROR: Invalid memory address.")
        elif type(which) == Byte:
            if 0 <= which.unsigned() < len(self.mem):
                return self.setmemory(which.unsigned(),Byte(value))
            else:
                print("ERROR: Invalid memory address.")
        else:
            print("ERROR: Invalid storage reference."+str(which))


    #
    # M.load(filename):
    #
    # Parses a Ha program source file, loading it into the machine
    # for execution.
    #
    def load(self,filename):

        extension = filename.split('.')[-1]
        
        if extension == "hasm":
            # Ha75 assembly code.
            raise Exception
        elif extension == "pha":
            # pseudo-Ha programming language code.
            f = open(filename,"r")
            code, targets = parseHa(f,len(self.program))
            self.program += code
            for lbl in targets:
                if lbl in self.labels:
                    print("Warning! Redefining target line label '"+lbl+"'.")
                self.labels[lbl] = targets[lbl]
        elif extension == "img":
            # memory image file
            self.image(filename)
        else:
            raise Exception

            
    #
    # M.image(filename):
    #
    # Parses a LogiSim RAM image file, loading the prescribed byte values into
    # the data memory.
    #
    def image(self,filename):
        f = open(filename,"r")
        address = 0
        header = True
        for line in f:
            if not header:
                for b in line[:-1].split(' '):
                    self[address] = '0x'+b
                    address += 1
            header = False
    #
    # M.step():
    #
    # Executes the instruction designated by the register 'IP'
    # (register 15).
    #
    def step(self):
        ip = self['IP'].unsigned()
        line = self.program[ip]
        op = line[0]
        self['IP'] = Byte(ip+1)
        if op in ["MOV","NEG","ASL","ASR"]:
            srce = line[1]
            dest = line[2]  
            if op == "MOV":
                self[dest] = self[srce]
            elif op == "NEG":
                self[dest] = -self[srce]
            elif op == "ASL":
                self[dest] = self[srce] << 1
            elif op == "ASR":
                self[dest] = self[srce] >> 1
        elif op in ["ADD","SUB","XOR","AND"]:
            src1 = line[1]
            src2 = line[2]
            dest  = line[3]
            if op == "ADD":
                self[dest] = self[src1] + self[src2]
            elif op == "SUB":
                self[dest] = self[src1] - self[src2]
            elif op == "XOR":
                self[dest] = self[src1] ^ self[src2]
            elif op == "AND":
                self[dest] = self[src1] & self[src2]
        elif op in ["SET","INC"]:
            constant = Byte(line[1][1:])
            dest = line[2]
            if op == "SET":
                self[dest] = constant
            elif op == "INC":
                self[dest] = self[dest] + constant
        elif op in ["BRZ","BRN"]:
            srce = line[1]
            trgt = line[2] 
            if op == "BRZ":
                if self[srce].unsigned() == 0:
                    self['IP'] = self.labels[trgt]
            elif op == "BRN":
                if self[srce].signed() < 0:
                    self['IP'] = self.labels[trgt]
        elif op == "STOP":
            self['IP'] = ip
            self.halted = True
        elif op == "BRA":
            trgt = line[1] 
            self['IP'] = self.labels[trgt]
        elif op == "LOAD":
            cnst = Byte(line[1][1:])
            addr = line[2]
            dest = line[3] 
            self[dest] = self[self[addr]+Byte(cnst)]
        elif op == "STORE":
            srce  = line[1]
            cnst = Byte(line[2][1:])
            addr = line[3] 
            self[self[addr]+Byte(cnst)] = self[srce]
        elif op == "INPUT":
            dest = line[1]
            if self.prompt:
                value = int(input("Enter an integer value: "))
            else:
                value = int(input(""))
            self[dest] = value
            print(value,"<==")
        elif op == "OUTPUT":
            srce = line[1]
            value = self[srce].signed()
            print("==>",value)
        elif op == "POP":
            dest = line[1]
            self[dest] = self[self['SP']]
            self['SP'] = self['SP'] + Byte(1)
        elif op == "PUSH":
            srce = line[1]
            self['SP'] = self['SP'] - Byte(1)
            self[self['SP']] = self[srce]
        elif op == "RTS":
            self['IP'] = self[self['SP']]
            self['SP'] = self['SP'] + Byte(1)
        elif op == "CALL":
            trgt = line[1] 
            self['SP'] = self['SP'] - Byte(1)
            self[self['SP']] = self['IP']
            self['IP'] = self.labels[trgt]
        else:
            print("Operation not supported.")
    
    #
    # M.execute():
    #
    # Executes the M's program starting at instruction at 0.
    #
    def execute(self):
        self['IP'] = Byte(0)
        while not self.halted:
            self.step()

#
# The main script.  Usage:
# 
#    python3 Ha75.py <Ha file>
#
# or
#    python3 Ha75.py <Ha file> <image file> 
#
# to load a memory image file.
#    
# You can append "--silent" at the end if you do not want any
# extraneous output other than the output of the program's
# execution. For example
#
#    python3 Ha75.py test.ha test.img --silent
#
# silently runs the Ha75 with the program "test.ha", with a
# starting memory image given in "test.img".
#
#


#
# The main script.  Usage:
# 
#    python3 Ha75.py <Ha files>
#
# or
#    python3 Ha75.py <Ha files> <image file> 
#
# to load a memory image file.
#    
# You can append "--silent" at the end if you do not want any
# extraneous output other than the output of the program's
# execution. For example
#
#    python3 Ha75.py test.pha test.img --silent
#
# silently runs the Ha75 with the program "test.pha", with a
# starting memory image given in "test.img".
#
#

silent = False
if len(sys.argv) > 2 and sys.argv[-1] == "--silent":
    silent = True
    del sys.argv[-1]

if not silent:
    print()
    print("_M_ _M_ _M_ _M_ _M_ seventy - five P _M_ _M_ _M_ _M_ _M_")
    print("_M_                                                  _M_")
    print("_M_ Reed CSii Ha75 pseudo-assembly simulator v1.0p   _M_")
    print("_M_                                                  _M_")
    print("_M_ _M_ _M_ _M_ _M_ seventy - five P _M_ _M_ _M_ _M_ _M_")
    print()

if len(sys.argv) < 2:
    print("ERROR: Please specify a Ha source file name.")
    print("Exiting simulation...")
    sys.exit(-1)

#
# Create the machine's state.
#
ha75 = Machine(16,256,not silent)

#
# Load in the pseudo-Ha program and memory image files,
#
for ha_file in sys.argv[1:]:
    if not silent: print("Loading '"+ha_file+"'...",end="")
    try:
        ha75.load(ha_file)
    except:
        if not silent: print(" Error.")
        print("Failed to load file '"+ha_file+"'.")
        sys.exit(-1)

#
# Run the program.
#
if not silent:
    print(" Done.")
    print("Executing...")
    print()
    print("_M_ _M_ _M_ _M_ _M_ _M_ _M_ _M_ _M_ _M_ _M_ _M_")
ha75.execute()
