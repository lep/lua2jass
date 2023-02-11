
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
        ctx = Context(self.interpreter, self.ip)
        ctx.parent = self.parent
        ctx.parent_call = self.parent_call
        return ctx

    def __str__(self):
        return "{}".format(("Context<", "locals:", self.locals, "params:", self.params, "tmps:", self.tmps, "parent:", self.parent, ">"))

    def get(self, name):
        if name in self.locals:
            return self.locals[name]
        elif self.parent is not None:
            return self.parent.get(name)
        else:
            return ()

    def has_rec(self, name):
        if name in self.locals:
            return self
        elif self.parent is not None:
            return self.parent.has_rec(name)
        else:
            return None
    
    def set(self, name, value):
        if ctx := self.has_rec(name):
            #print("Setting", name, "=", value, "in", ctx)
            ctx.locals[name] = value
        else:
            #print("Setting", name, "=", value, "in", self)
            self.locals[name] = value
        #if name in self.locals or self.parent is None:
        #    self.locals[name] = value
        #else:
        #    self.parent.set(name, value)


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
        ctx.ip += 1
        #print("executing", ins)
        if ins[0] == "fun":
            #print(ctx)
            pass
        elif ins[0] == "getlit":
            if ins[2] == "print":
                ctx.tmps[ins[1]] = lambda x: print(*list(x.values()))
            else:
                ctx.tmps[ins[1]] = ctx.get(ins[2])
        elif ins[0] == "not":
            ctx.tmps[ins[1]] = not ctx.tmps[ins[2]]
        elif ins[0] == "add":
            ctx.tmps[ins[1]] = ctx.tmps[ins[2]] + ctx.tmps[ins[3]]
        elif ins[0] == "set":
            ctx.tmps[ins[1]] = ctx.tmps[ins[2]]
        elif ins[0] == "sub":
            ctx.tmps[ins[1]] = ctx.tmps[ins[2]] - ctx.tmps[ins[3]]
        elif ins[0] == "mul":
            ctx.tmps[ins[1]] = ctx.tmps[ins[2]] * ctx.tmps[ins[3]]
        elif ins[0] == "gt":
            ctx.tmps[ins[1]] = ctx.tmps[ins[2]] > ctx.tmps[ins[3]]
        elif ins[0] == "gte":
            #print(ins)
            #print(ctx)
            ctx.tmps[ins[1]] = ctx.tmps[ins[2]] >= ctx.tmps[ins[3]]
        elif ins[0] == "lte":
            ctx.tmps[ins[1]] = ctx.tmps[ins[2]] <= ctx.tmps[ins[3]]
        elif ins[0] == "ret":
            self.stack.pop()
            parent_ctx = self.stack[-1]
            #print("parent ctx", parent_ctx)
            parent_instruction = self.instructions[ parent_ctx.ip-1 ] # TODO
            parent_ctx.tmps[ parent_instruction[1] ] = ctx.tmps[0]
        elif ins[0] == "lambda":
            new_ctx = self.call(ins[2])
            new_ctx.parent = ctx
            ctx.tmps[ ins[1] ] = new_ctx
        elif ins[0] == "setlit":
            ctx.set(ins[1], ctx.tmps[ins[2]])
            #print(ctx)
        elif ins[0] == "lit":
            ctx.tmps[ins[1]] = ins[2]
        elif ins[0] == "bindlit":
            ctx.params[ ins[1] ] = ctx.tmps[ins[2]]
        elif ins[0] == "bind":
            ctx.params[ ins[1] ] = ctx.tmps[ins[2]]
        elif ins[0] == "calllit":
            if ins[2] == "print":
                #print("print", ctx.params['a'])
                ctx.params = {}
            else:
                new_ctx = ctx.get( ins[2] ).clone()
                new_ctx.locals = ctx.params
                new_ctx.parent_call = ctx
                ctx.params = {}
                self.stack.append(new_ctx)
        elif ins[0] == "call":
            ##print(ctx)
            if hasattr(ctx.tmps[ ins[2] ], '__call__'):
                ctx.tmps[ins[2]](ctx.params)
            else:
                new_ctx = ctx.tmps[ins[2]].clone()
                new_ctx.tmps = ctx.params
                new_ctx.parent_call = ctx
                #print("call new ctx", new_ctx)
                ctx.params = {}
                self.stack.append(new_ctx)
        elif ins[0] == "lbl":
            pass
        elif ins[0] == "eq":
            ctx.tmps[ ins[1] ] = ctx.tmps[ ins[2] ] == ctx.tmps[ ins[3] ]
        elif ins[0] == "jmpt":
            v = ctx.tmps[ ins[2] ]
            if v:
                ctx.ip = self.labels[ ins[1] ]
                #return ctx
        elif ins[0] == "jmp":
            ctx.ip = self.labels[ ins[1] ]
        elif ins[0] == "enter":
            pass
            #new_ctx = ctx.clone()
            #new_ctx.parent = ctx
            #self.stack.append(new_ctx)
            ##ctx = new_ctx
        elif ins[0] == "leave":
            pass
            #self.stack.pop()
            #new_old_ctx = self.stack[-1]
            #new_old_ctx.ip = ctx.ip
            ##ctx = new_old_ctx
        elif ins[0] == "local":
            ctx.locals[ins[1]] = None
        elif ins[0] == "table":
            ctx.tmps[ins[1]] = {}
        elif ins[0] == "settable":
            ctx.tmps[ins[1]][ctx.tmps[ins[2]]] = ctx.tmps[ins[3]]
        elif ins[0] == "gettable":
            ctx.tmps[ins[1]] = ctx.tmps[ins[2]][ctx.tmps[ins[3]]]
        elif ins[0] == "dump":
            for x in self.stack:
                print(x)
            #if ins[1]:
            #    for v in ins[1]:
            #        print(v, "=", self.stack[-1].tmps.get(v))
            #else:
            #    print( list(str(x) for x in self.stack ))
        else:
            raise Exception("Unknown instruction", ins)

        #return self
    
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
    ("mul", 0, -6, -1),
    ("jmp", "ret_2"),
    ("lbl", "ret_1"),
    ("lit", 0, 1),
    ("lbl", "ret_2"),
    ["ret"],

    ("fun", "$_main"),
    ("lambda", -1337, "$_fac"),
    ("setlit", "fac", -1337),
    ("lit", -1, 6),
    ("bindlit", 'n', -1),
    ("calllit", -2, "fac"),
    ("bindlit", "a", -2),
    ("calllit", -3, "print"),
    ("dump", ["a", "fac"]),
]

prog_closure = [
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

    ("lit", -5, 123),
    ("bindlit", "y", -5),
    ("calllit", -6, "fourplus"),
    ("bindlit", "a", -6),
    ("calllit", -7,  "print"),
]

prog_closure2 = [
    ("fun", "$_lambda_main_1"),
    ("lit", -1, 1),
    ("getlit", -2, "y"),
    ("add", -3, -1, -2),
    ("setlit", "y", -3),
    ("getlit", -4, "y"),
    ("getlit", -5, "x"),
    ("add", 0, -4, -5),
    ["ret"],


    ("fun", "$_main"),
    ("table", -1),
    ("setlit", "a", -1),
    ("lit", -20, 20),
    ("setlit", "x", -20),
    ("lit", "%_loop", 3),
    ("lit", "%_zero", 0),
    ("lit", "%_one", 1),
    ("setlit", "%loop", "%_loop"),
    ("setlit", "%zero", "%_zero"),
    ("setlit", "%one", "%_one"),
    #("dump", []),

    # for loop ...
    ("lbl", "#start-loop"),
    ["enter"],
    ("getlit", "%_loop", "%loop"),
    ("getlit", "%_zero", "%zero"),
    ("getlit", "%_one", "%one"),
    ("eq", "%_cond", "%_loop", "%_zero"),
    ("jmpt", "#after-loop", "%_cond"),
    ("local", "y"),
    ("lit", 0, 0),
    ("setlit", "y", 0),
    ("lambda", -2, "$_lambda_main_1"),
    ("getlit", -5, "a"),
    ("getlit", "%_loop", "%loop"),
    ("settable", -5, "%_loop", -2),
    ("sub", "%_loop", "%_loop", "%_one"),
    ("setlit", "%loop", "%_loop"),
    ("dump", ["%_loop", "%_one"]),
    ["leave"], # TODO: potentially annotate with some form of "level" in case of goto
    ("jmp", "#start-loop"),
    ("lbl", "#after-loop"),
    ["leave"], # a bit ugly, might be solvable with better loop structure
    ("dump", []),

    ("lit", "%_loop", 3),
    ("lit", "%_zero", 0),
    ("lit", "%_one", 1),
    ("lbl", "#loop2-start"), # dont use enter here for simplicitys sake
    ("eq", "%_cond", "%_loop", "%_zero"),
    ("jmpt", "#loop2-end", "%_cond"),
    ("getlit", "%_a", "a"),
    ("gettable", "%_lambda", "%_a", "%_loop"),
    ("call", "_r", "%_lambda"),
    ("bindlit", "a", "_r"),
    ("calllit", "_", "print"),
    ("sub", "%_loop", "%_loop", "%_one"),
    ("jmp", "#loop2-start"),
    ("lbl", "#loop2-end"),
    ("dump", []),
]
pp = [
("fun","$_addto"),
("local","x"),
("setlit","x",1),
("getlit",-3,"x"),
("lit",-4,1),
("add",-2,-3,-4),
("setlit","x",-2),
("lambda",-5,"$_lambda-5"),
("set",0,-5),
["ret"],
["leave"],
["ret"],
("fun","$_lambda-5"),
("local","y"),
("setlit","y",1),
("getlit",-7,"x"),
("getlit",-8,"y"),
("add",-6,-7,-8),
("set",0,-6),
["ret"],
["leave"],
["ret"],
("fun","$_main"),
["enter"],
("lambda",-1,"$_addto"),
("setlit","addto",-1),
("getlit",-10,"addto"),
("lit",-11,5),
("bind",1,-11),
("call",-9,-10),
("local","add5"),
("setlit","add5",-9),
("getlit",-13,"print"),
("getlit",-15,"add5"),
("lit",-16,7),
("bind",1,-16),
("call",-14,-15),
("bind",1,-14),
("call",-12,-13),
("getlit",-18,"print"),
("getlit",-20,"add5"),
("lit",-21,5),
("bind",1,-21),
("call",-19,-20),
("bind",1,-19),
("call",-17,-18),
("getlit",-23,"print"),
("getlit",-25,"add5"),
("lit",-26,0),
("bind",1,-26),
("call",-24,-25),
("bind",1,-24),
("call",-22,-23),
["leave"],
["ret"],
]

i = Interpreter(pp)
i.stack.append(i.call("$_main"))
ins_count = 0
while True:
    i.step()
    ins_count += 1
    #try:
    #    i.step()
    #    ins_count += 1
    #except IndexError:
    #    break
print("Script took", ins_count, "steps")
exit(0)
