import json
import sys
import io

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
        self.output = io.StringIO()

    def print(self, ctx):
        l = list(ctx.values())
        print(*l)
        print(*l, file=self.output)
        
        

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
                ctx.tmps[ins[1]] = self.print #lambda x: print(*list(x.values()))
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
        elif ins[0] == "lt":
            ctx.tmps[ins[1]] = ctx.tmps[ins[2]] < ctx.tmps[ins[3]]
        elif ins[0] == "gte":
            #print(ins)
            #print(ctx)
            ctx.tmps[ins[1]] = ctx.tmps[ins[2]] >= ctx.tmps[ins[3]]
        elif ins[0] == "lte":
            ctx.tmps[ins[1]] = ctx.tmps[ins[2]] <= ctx.tmps[ins[3]]
        elif ins[0] == "ret":
            self.stack.pop()
            #parent_ctx = self.stack[-1]
            parent_ctx = ctx.parent_call #self.stack[-1]
            #print("parent ctx", parent_ctx)
            parent_instruction = self.instructions[ parent_ctx.ip-1 ] # TODO
            #parent_ctx.tmps[ parent_instruction[1] ] = ctx.tmps[0]
            parent_ctx.tmps[ parent_instruction[1] ] = ctx.get("$_ret")
        elif ins[0] == "table":
            ctx.tmps[ ins[1] ] = {}
        elif ins[0] == "settable":
            ctx.tmps[ ins[1] ][ ctx.tmps[ins[2]] ] = ctx.tmps[ins[3]]
        elif ins[0] == "gettable":
            ctx.tmps[ ins[1] ] = ctx.tmps[ins[2]][ ctx.tmps[ins[3]] ]
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
        #elif ins[0] == "calllit":
        #    if ins[2] == "print":
        #        #print("print", ctx.params['a'])
        #        ctx.params = {}
        #    else:
        #        new_ctx = ctx.get( ins[2] ).clone()
        #        new_ctx.locals = ctx.params
        #        new_ctx.parent_call = ctx
        #        ctx.params = {}
        #        self.stack.append(new_ctx)
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
            #pass
            new_ctx = ctx.clone()
            new_ctx.parent = ctx
            self.stack.append(new_ctx)
            #ctx = new_ctx
        elif ins[0] == "leave":
            #pass
            self.stack.pop()
            new_old_ctx = self.stack[-1]
            new_old_ctx.ip = ctx.ip
            #ctx = new_old_ctx
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


def run_bytecode(pp):
    #pp = json.load(open(sys.argv[1]))
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
    return i.output.getvalue()

if __name__ == '__main__':
    run_bytecode(json.load(open(sys.argv[1])))
