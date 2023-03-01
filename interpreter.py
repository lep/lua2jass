import json
import sys
import io

def log(*args, **kwargs):
    print(*args, **kwargs, file=sys.stderr)

class StopInterpreterException(Exception):
    pass

class Value:
    def __init__(self, type):
        self.type = type

class Context:
    def __init__(self, ip, chunk_name):
        self.ip = ip
        self.locals = {}
        self.params = {}
        self.tmps = {}
        self.parent_call = None
        self.parent = None
        self.chunk_name = chunk_name
        self.depth = 0

    def clone(self):
        ctx = Context(self.ip, self.chunk_name)
        ctx.parent = self.parent
        ctx.parent_call = self.parent_call
        ctx.depth = self.depth + 1
        return ctx

    def __str__(self):
        return "{}".format(("Context<", "locals:", self.locals, "params:", self.params, "tmps:", self.tmps, "parent:", self.parent, "depth:", self.depth, ">"))

    def has_rec(self, name):
        if name in self.locals:
            return self
        elif self.parent is not None:
            return self.parent.has_rec(name)
        else:
            return None

    def get(self, name):
        if ctx := self.has_rec(name):
            return ctx.locals.get(name)
        return None

    
    def set(self, name, value):
        if ctx := self.has_rec(name):
            ctx.locals[name] = value
        else:
            self.locals[name] = value


class Interpreter:
    def __init__(self, instructions):
        self.labels = {}
        self.instructions = instructions
        self.stack = []
        self.find_labels()
        self.output = io.StringIO()

    def print(self, ctx):
        l = "\t".join(list(map(str, ctx.values())))
        print(l)
        print(l, file=self.output)
        
        

    def find_labels(self):
        for i in range(len(self.instructions)):
            x = self.instructions[i]
            if x[0] == "fun":
                self.labels[ x[1] ] = i
            elif x[0] == "lbl":
                self.labels[ x[1] ] = i

    def step(self):
        ctx = self.stack[-1]
        try:
            ins = self.instructions[ ctx.ip ]
        except IndexError:
            raise StopInterpreterException()
        ctx.ip += 1
        log("executing", ins)
        if ins[0] == "fun":
            pass
        elif ins[0] == "getlit":
            if ins[2] == "print":
                ctx.tmps[ins[1]] = self.print
            elif ins[2] == "$params":
                #log("my params are", ctx.tmps)
                ctx.tmps[ins[1]] = ctx.tmps
            else:
                #name = ins[2]
                #log("searching for", name, ctx)
                #if fctx := ctx.has_rec(name):
                #    log("found in", fctx)
                #else:
                #    log("did not find")
                ctx.tmps[ins[1]] = ctx.get(ins[2])
        elif ins[0] == "not":
            ctx.tmps[ins[1]] = not ctx.tmps[ins[2]]
        elif ins[0] == "neg":
            ctx.tmps[ins[1]] = - ctx.tmps[ins[2]]
        elif ins[0] == "complement":
            ctx.tmps[ins[1]] =  -ctx.tmps[ins[2]] - 1
        elif ins[0] == "len":
            ctx.tmps[ins[1]] =  len(ctx.tmps[ins[2]])
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
            ctx.tmps[ins[1]] = ctx.tmps[ins[2]] >= ctx.tmps[ins[3]]
        elif ins[0] == "lte":
            ctx.tmps[ins[1]] = ctx.tmps[ins[2]] <= ctx.tmps[ins[3]]
        elif ins[0] == "ret":
            assert(self.stack.pop() is ctx)
            parent_ctx = ctx.parent_call
            if parent_ctx is None:
                raise StopInterpreterException()
            #log("returning from chunk", ctx.chunk_name, "to", parent_ctx.chunk_name, "return value", ctx.get("$ret"))
            parent_instruction = self.instructions[ parent_ctx.ip-1 ] # TODO
            parent_ctx.tmps[ parent_instruction[1] ] = ctx.get("$ret")

        elif ins[0] == "table":
            ctx.tmps[ ins[1] ] = {}
        elif ins[0] == "append":
            offset = ins[ 1 ] -1
            target_tbl = ctx.tmps[ ins[2] ]
            source_tbl = ctx.tmps[ ins[3] ]
            #log("target", target_tbl)
            #log("source", source_tbl)
            for k in source_tbl:
                target_tbl[ k + offset ] = source_tbl[k]
        elif ins[0] == "getlist":
            offset = ins[1]
            target_tbl = ctx.tmps[ ins[2] ]
            source_tbl = ctx.tmps[ ins[3] ]
            i = offset
            #log("source", source_tbl, "offset", offset)
            while True:
                if i in source_tbl:
                    target_tbl[ i - offset +1 ] = source_tbl[ i ]
                else:
                    break
                i += 1
            #log("target", target_tbl)
            #for k in source_tbl:
            #    target_tbl[ k - off ] = source_tbl[ k ]
        elif ins[0] == "settable":
            ctx.tmps[ ins[1] ][ ctx.tmps[ins[2]] ] = ctx.tmps[ins[3]]
        elif ins[0] == "gettable":
            ctx.tmps[ ins[1] ] = ctx.tmps[ins[2]].get( ctx.tmps[ins[3]] )
        elif ins[0] == "lambda":
            # due to how we compile the internal label is always equal to the
            # target register
            new_ctx = self.call(ins[1])
            new_ctx.parent = ctx
            new_ctx.chunk_name = ins[2]
            ctx.tmps[ ins[1] ] = new_ctx
            #log("created lambda with closed locals", ctx)
        elif ins[0] == "setlit":
            name = ins[1]
            value = ctx.tmps[ins[2]]
            #log("Setting", name, "to", value, "in ctx", ctx)
            ctx.set(ins[1], ctx.tmps[ins[2]])
        elif ins[0] == "lit":
            ctx.tmps[ins[1]] = ins[2]
        elif ins[0] == "nil":
            ctx.tmps[ins[1]] = None
        elif ins[0] == "bindlit":
            ctx.params[ ins[1] ] = ctx.tmps[ins[2]]
        elif ins[0] == "bind":
            ctx.params[ ins[1] ] = ctx.tmps[ins[2]]
        elif ins[0] == "call":
            reg_res = ins[1]
            reg_fn = ins[2]
            reg_params = ins[3]
            if hasattr(ctx.tmps[ reg_fn ], '__call__'):
                ctx.tmps[reg_fn](ctx.tmps[reg_params])
            else:
                new_ctx = ctx.tmps[reg_fn].clone()
                #new_ctx.tmps = ctx.params
                new_ctx.tmps = ctx.tmps[reg_params]
                new_ctx.parent_call = ctx
                ctx.params = {}
                self.stack.append(new_ctx)
                #log("Going from chunk", ctx.chunk_name, "to", new_ctx.chunk_name)
        elif ins[0] == "lbl":
            pass
        elif ins[0] == "eq":
            a = ctx.tmps[ins[2]]
            b = ctx.tmps[ins[3]]
            #log("compaing", a, "and", b, ":", a==b)
            ctx.tmps[ ins[1] ] = ctx.tmps[ ins[2] ] == ctx.tmps[ ins[3] ]
        elif ins[0] == "neq":
            ctx.tmps[ ins[1] ] = ctx.tmps[ ins[2] ] != ctx.tmps[ ins[3] ]
        elif ins[0] == "jmpt":
            v = ctx.tmps[ ins[2] ]
            if v:
                ctx.ip = self.labels[ ins[1] ]
        elif ins[0] == "jmp":
            ctx.ip = self.labels[ ins[1] ]
        elif ins[0] == "enter":
            new_ctx = ctx.clone()
            new_ctx.parent = ctx
            self.stack.append(new_ctx)
        elif ins[0] == "leave":
            self.stack.pop()
            new_old_ctx = self.stack[-1]
            new_old_ctx.ip = ctx.ip
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
                log(x)
        elif ins[0] == "comment":
            #log("COMMENT", ins[1])
            pass
        else:
            raise Exception("Unknown instruction", ins)
    
    def call(self, label):
        idx = self.labels[label]
        return Context(idx, label)


def run_bytecode(pp):
    #pp = json.load(open(sys.argv[1]))
    i = Interpreter(pp)
    i.stack.append(i.call(0))# "$main"))
    ins_count = 0
    while True:
    #for _ in range(32):
        try:
            i.step()
            ins_count += 1
        except StopInterpreterException:
            break
        #try:
        #    i.step()
        #    ins_count += 1
        #except IndexError:
        #    break
    return i.output.getvalue(), ins_count

if __name__ == '__main__':
    out, cnt = run_bytecode(json.load(open(sys.argv[1])))
    log(f"Programm took {cnt} steps to finish")
