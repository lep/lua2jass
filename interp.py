
class Value:
    def __init__(self, type):
        self.type = type

class Context:
    def __init__(self, i, ip):
        self.interpreter = i
        self.ip = ip
        self.locals = {}
        self.params = {}
        self.tmps = {}
        self.parent_call = None
        self.parent = None

    def clone(self):
        #return self # TODO
        ctx = Context(self.interpreter, self.ip)
        ctx.parent = self.parent
        ctx.parent_call = self.parent_call
        return ctx

    def __str__(self):
        return "{}".format(("Context<", "instruction:", self.interpreter.instructions[self.ip], "locals:", self.locals, "params:", self.params, "tmps:", self.tmps, "parent:", self.parent, ">"))

    def get(self, name):
        if name in self.locals:
            return self.locals[name]
        elif self.parent is not None:
            return self.parent.get(name)
        else:
            return ()
    
    def set(self, name, value):
        if name in self.locals or self.parent is None:
            self.locals[name] = value
        else:
            self.parent.set(name, value)
        

    def step(self):
        ins = self.interpreter.instructions[self.ip]
        print("executing", ins)
        if ins[0] == "fun":
            print(self)
            pass
        elif ins[0] == "getlit":
            self.tmps[ins[1]] = self.get(ins[2])
        elif ins[0] == "add":
            self.tmps[ins[1]] = self.tmps[ins[2]] + self.tmps[ins[3]]
        elif ins[0] == "sub":
            self.tmps[ins[1]] = self.tmps[ins[2]] - self.tmps[ins[3]]
        elif ins[0] == "ret":
            parent_call_ins = self.interpreter.instructions[self.parent_call.ip]
            self.parent_call.tmps[parent_call_ins[1]] = self.tmps[0]
            self.parent_call.ip += 1
            return self.parent
        elif ins[0] == "lambda":
            ctx = self.interpreter.call(ins[2])
            ctx.parent = self
            self.tmps[ins[1]] = ctx
        elif ins[0] == "setlit":
            self.set(ins[1], self.tmps[ins[2]])
        elif ins[0] == "lit":
            self.tmps[ins[1]] = ins[2]
        elif ins[0] == "bindlit":
            self.params[ ins[1] ] = self.tmps[ins[2]]
        elif ins[0] == "calllit":
            if ins[2] == "print":
                print("print", self)
                self.params = {}
            else:
                ctx = self.get( ins[2] ).clone()
                ctx.locals = self.params
                ctx.parent_call = self
                self.params = {}
                return ctx
        elif ins[0] == "call":
            ctx = self.tmps[ins[2]].clone()
            ctx.locals = self.params
            ctx.parent_call = self
            self.params = {}
            return ctx
        elif ins[0] == "lbl":
            pass
        elif ins[0] == "eq":
            self.tmps[ ins[1] ] = self.tmps[ ins[2] ] == self.tmps[ ins[3] ]
        elif ins[0] == "jmpt":
            v = self.tmps[ ins[2] ]
            if v:
                self.ip = self.interpreter.labels[ ins[1] ]
                return self
        elif ins[0] == "jmp":
            self.ip = self.interpreter.labels[ ins[1] ]
            
        else:
            raise Exception("Unknown instruction", ins)

        self.ip += 1
        return self
            
        

class Interpreter:
    def __init__(self, instructions):
        self.labels = {}
        self.instructions = instructions
        self.stack = []
        self.find_labels()

    def find_labels(self):
        for i in range(len(self.instructions)):
            x = self.instructions[i]
            if x[0] == "fun":
                self.labels[x[1]] = i
            elif x[0] == "lbl":
                self.labels[ x[1] ] = i

    def step(self):
        ctx = self.stack[-1]
        ins = self.instructions[ ctx.ip ]
    
    def call(self, label):
        idx = self.labels[label]
        return Context(self, idx)

prog_fac = [
    ("fun", "$_fac"),
    ("getlit", -1, "n"),
    ("lit", -2, 0),
    ("lit", -3, 1),
    ("eq", -4, -1, -2),
    ("jmpt", "ret_1", -4),
    ("sub", -5, -1, -3),
    ("bindlit", "n", -5),
    ("calllit", -6, "fac"),
    ("mul", 0, -6, -5),
    ("jmp", "ret_2"),
    ("lbl", "ret_1"),
    ("lit", 0, 1),
    ("lbl", "ret_2"),
    ["ret"],

    ("fun", "$_main"),
    ("lambda", -1337, "$_fac"),
    ("setlit", "fac", -1337),
    ("lit", -1, 1),
    ("bindlit", 'n', -1),
    ("calllit", -2, "fac"),
    ("bindlit", "a", -2),
    ("calllit", -3, "print"),
]

prog = [
    ("fun", "$_lambda_addto_1"),
    ("getlit", -1, "x"),
    ("getlit", -2, "y"),
    ("add", 0, -1, -2),
    ["ret"],

    ("fun", "$_addto"),
    ("lambda", 0, "$_lambda_addto_1"),
    ["ret"],

    ("fun", "$_main"),
    ("lambda", -1, "$_addto"),
    ("setlit", "addto", -1),
    
    ("getlit", -2, "addto"),
    ("lit", -3, 4),
    ("bindlit", "x", -3),
    ("call", -4, -2),
    #("calllit", -4, "addto"),
    ("setlit", "fourplus", -4),

    ("lit", -5, 3),
    ("bindlit", "y", -5),
    ("calllit", -6, "fourplus"),
    ("bindlit", "a", -6),
    ("calllit", -7,  "print"),
]

i = Interpreter(prog_fac)
ctx = i.call("$_main")
while True:
    ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step() # fac(0)
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()
ctx = ctx.step()





"""
Since everything in lua is a hashtable and jass also gives us some good
hashtables we should probably use it as much as possible.
For example:
    * Have (most) instructions take a table
    * Have special instructions for _G, parameters, locals and closed over
      variables

Also use special instructions for common constructs like:
    * Calling a "well-known" function (this still does a string
        lookup etc. but condensed into one instruction)
    * Might even use rewrite rules for that

"""

"""
function addto(x)
  -- Return a new function that adds x to the argument
  return function(y)
    --[=[ When we refer to the variable x, which is outside the current
      scope and whose lifetime would be shorter than that of this anonymous
      function, Lua creates a closure.]=]
    return x + y
  end
end
fourplus = addto(4)
print(fourplus(3))  -- Prints 7
"""
