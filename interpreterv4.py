from intbase import InterpreterBase, ErrorType
from brewparse import parse_program

class BrewinObject:
    def __init__(self):
        self.fields = {}

class BrewinReference:
    def __init__(self, env, var=None, obj=None, field=None):
        self.env = env
        self.var = var
        self.obj = obj
        self.field = field

    def get_value(self):
        if self.var is not None:
            val = self.env.get(self.var)
        else:
            val = self.obj.fields.get(self.field, None)

        # Unwrap chained references
        while isinstance(val, BrewinReference):
            val = val.get_value()
        return val

    def set_value(self, value):
        if self.var is not None:
            # If the variable currently holds a reference, delegate
            current = self.env.get(self.var)
            if isinstance(current, BrewinReference):
                current.set_value(value)
            else:
                self.env.set(self.var, value)
        else:
            self.obj.fields[self.field] = value


class Environment:

    def __init__(self):
        self.stack = []

    def enter_func(self):
        # start new frame with just the function scope
        self.stack.append([{}])

    def exit_func(self):
        self.stack.pop()

    def push_block(self):
        self.stack[-1].append({})

    def pop_block(self):
        self.stack[-1].pop()

    def exists(self, name):
        # does this name exist anywhere in the current function frame?
        for scope in reversed(self.stack[-1]):
            if name in scope:
                return True
        return False

    def define(self, name, value, is_block):
        #Define a new name in function scope (var/params) or block scope (bvar)
        frame = self.stack[-1]
        if is_block:
            frame[-1][name] = value
        else:
            frame[0][name] = value

    def get(self, name):
        for scope in reversed(self.stack[-1]):
            if name in scope:
                return scope[name]
        return None

    def set(self, name, value):
        for scope in reversed(self.stack[-1]):
            if name in scope:
                scope[name] = value
                return True
        return False

class Function:
    def __init__(self, ast):
        self.name = ast.get("name")
        self.args = ast.get("args")
        self.statements = ast.get("statements")


class ReturnSignal(Exception):
    def __init__(self, value):
        self.value = value


class Interpreter(InterpreterBase):

    VAR_DEF_NODE = "vardef"
    BVAR_DEF_NODE = "bvardef"

    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)

        self.env = Environment()
        self.funcs = {}  # (name, param_type_tuple) -> Function

        self.NIL = None
        self.VOID = object()  # unique sentinel

        self.suffix_map = {
            'i': 'int',
            's': 'string',
            'b': 'bool',
            'o': 'object',
            'v': 'void'
        }

        self.binary_ops = {
            "+", "-", "*", "/", "==", "!=", "<", "<=", ">", ">=", "&&", "||"
        }

    def declared_type(self, name):
        if not isinstance(name, str) or len(name) == 0:
            return None
        return self.suffix_map.get(name[-1])

    def return_type(self, fname):
        # Return type from function name (main is always void)
        if fname == "main":
            return "void"
        return self.declared_type(fname)

    def runtime_type(self, val):
        # Map runtime Python values to Brewin++ type names
        if val is self.NIL:
            return "object"
        if val is self.VOID:
            return "void"
        if isinstance(val, BrewinObject):
            return "object"
        if isinstance(val, BrewinReference):
            return self.runtime_type(val.get_value())
        if isinstance(val, bool):
            return "bool"
        if isinstance(val, int):
            return "int"
        if isinstance(val, str):
            return "string"
        return "object"

    def default(self, t):
        return {
            "int": 0,
            "string": "",
            "bool": False,
            "object": self.NIL,
            "void": self.VOID
        }[t]

    # Function table construction

    def param_sig(self, args):
        # return tuple of parameter static types from arg nodes
        sig = []
        for a in args:
            t = self.declared_type(a.get("name"))
            if t is None or t == "void":
                self.error(ErrorType.TYPE_ERROR, "invalid formal parameter type")
            sig.append(t)
        return tuple(sig)

    def run(self, program):
        ast = parse_program(program)

        # build function table
        for fn_ast in ast.get("functions"):
            fn = Function(fn_ast)

            # validate function name / return type
            if fn.name != "main":
                rett = self.return_type(fn.name)
                if rett not in ("int", "string", "bool", "object", "void"):
                    self.error(ErrorType.TYPE_ERROR, "invalid function return type")

            sig = self.param_sig(fn.args)

            # reject void-type formal params
            for a in fn.args:
                if self.declared_type(a.get("name")) == "void":
                    self.error(ErrorType.TYPE_ERROR, "formal parameter cannot be void")

            key = (fn.name, sig)
            if key in self.funcs:
                self.error(ErrorType.NAME_ERROR, "duplicate function definition")
            self.funcs[key] = fn

        if ("main", ()) not in self.funcs:
            self.error(ErrorType.NAME_ERROR, "main missing")

        try:
            self.run_function("main", [])
        except ReturnSignal:
            # A return in main just terminates the program.
            pass

    def get_function(self, name, arg_types):
        key = (name, tuple(arg_types))
        fn = self.funcs.get(key)
        if fn is None:
            self.error(ErrorType.NAME_ERROR, "function not found")
        return fn

    # Built-ins + Function calls

    def run_function(self, name, arg_exprs):
        if name in ("print", "inputi", "inputs"):
            return self.run_builtin(name, arg_exprs)

        actual_vals = [self.eval_expr(e) for e in arg_exprs]
        arg_types = [self.runtime_type(v) for v in actual_vals]

        # Passing void as an argument is illegal
        if any(t == "void" for t in arg_types):
            self.error(ErrorType.TYPE_ERROR, "cannot pass void as argument")

        fn = self.get_function(name, arg_types)
        rettype = self.return_type(fn.name)

        caller_env = self.env
        self.env = Environment()
        self.env.enter_func()

        # Bind formal parameters
        for formal, arg_expr, actual_val in zip(fn.args, arg_exprs, actual_vals):
            pname = formal.get("name")
            ptype = self.declared_type(pname)
            is_ref = formal.get("ref")

            # Parameters cannot clash inside this function
            if self.env.exists(pname):
                self.error(ErrorType.NAME_ERROR, "parameter name already defined")

            self.env.define(pname, self.default(ptype), is_block=False)

            if is_ref:
                # build reference into caller's environment
                locator = self.locate_reference(arg_expr, caller_env)
                if self.runtime_type(locator.get_value()) != ptype:
                    self.error(ErrorType.TYPE_ERROR, "reference parameter type mismatch")
                self.env.set(pname, locator)
            else:
                self.env.set(pname, actual_val)

        ret_val = self.default(rettype)

        try:
            for st in fn.statements:
                self.exec_stmt(st)
        except ReturnSignal as r:
            val = r.value
            if rettype == "void":
                # void functions (including main) cannot return a value
                if val is not self.VOID:
                    self.error(ErrorType.TYPE_ERROR, "void function cannot return a value")
                ret_val = self.NIL
            else:
                if val is self.VOID:
                    val = self.default(rettype)
                if self.runtime_type(val) != rettype:
                    self.error(ErrorType.TYPE_ERROR, "return type mismatch")
                ret_val = val

        self.env = caller_env
        return ret_val

    def run_builtin(self, name, arg_exprs):
        actual_vals = [self.eval_expr(e) for e in arg_exprs]
        arity = len(actual_vals)

        def val_to_str(v):
            if isinstance(v, bool):
                return str(v).lower()
            if v is self.NIL:
                return "nil"
            return str(v)

        if name == "print":
            out = ""
            for v in actual_vals:
                out += val_to_str(v)
            self.output(out)
            return self.VOID

        if name == "inputi":
            if arity > 1:
                self.error(ErrorType.NAME_ERROR, "too many arguments for inputi")
            if arity == 1:
                self.output(val_to_str(actual_vals[0]))
            s = self.get_input()
            try:
                return int(s)
            except (ValueError, TypeError):
                self.error(ErrorType.TYPE_ERROR, "invalid integer input")

        if name == "inputs":
            if arity > 1:
                self.error(ErrorType.NAME_ERROR, "too many arguments for inputs")
            if arity == 1:
                self.output(val_to_str(actual_vals[0]))
            return self.get_input()

        self.error(ErrorType.NAME_ERROR, "unknown builtin")

    def locate_reference(self, arg_expr, caller_env):
        if arg_expr.elem_type != self.QUALIFIED_NAME_NODE:
            self.error(ErrorType.TYPE_ERROR, "reference parameter must be a variable or field")

        dotted = arg_expr.get("name")
        path = dotted.split(".")
        base = path[0]

        base_val = caller_env.get(base)
        if base_val is None:
            self.error(ErrorType.NAME_ERROR, "variable not defined")

        # Simple variable reference
        if len(path) == 1:
            return BrewinReference(caller_env, var=base)

        # Dotted reference - base must be object-typed by name
        if not base.endswith("o"):
            self.error(ErrorType.TYPE_ERROR, "base must be object-typed")

        cur = base_val
        if isinstance(cur, BrewinReference):
            cur = cur.get_value()

        if cur is self.NIL:
            self.error(ErrorType.FAULT_ERROR, "nil dereference")
        if not isinstance(cur, BrewinObject):
            self.error(ErrorType.TYPE_ERROR, "base not object")

        # traverse intermediates
        for seg in path[1:-1]:
            if not seg.endswith("o"):
                self.error(ErrorType.TYPE_ERROR, "intermediate must be object-typed")

            nxt = cur.fields.get(seg)
            if nxt is None:
                self.error(ErrorType.NAME_ERROR, "field not found")

            if isinstance(nxt, BrewinReference):
                nxt = nxt.get_value()

            if nxt is self.NIL:
                self.error(ErrorType.FAULT_ERROR, "nil dereference")
            if not isinstance(nxt, BrewinObject):
                self.error(ErrorType.TYPE_ERROR, "intermediate not object")

            cur = nxt

        # final parent object is cur; final field name is path[-1]
        final_field = path[-1]
        return BrewinReference(caller_env, obj=cur, field=final_field)

    def exec_stmt(self, st):
        kind = st.elem_type

        if kind == self.VAR_DEF_NODE:
            self.define_var(st, is_block=False)
        elif kind == self.BVAR_DEF_NODE:
            self.define_var(st, is_block=True)
        elif kind == "=":
            self.assign(st)
        elif kind == self.FCALL_NODE:
            self.run_function(st.get("name"), st.get("args"))
        elif kind == self.RETURN_NODE:
            expr = st.get("expression")
            raise ReturnSignal(self.VOID if expr is None else self.eval_expr(expr))
        elif kind == self.IF_NODE:
            self.do_if(st)
        elif kind == self.WHILE_NODE:
            self.do_while(st)

    def define_var(self, st, is_block):
        name = st.get("name")
        t = self.declared_type(name)
        if t is None or t == "void":
            self.error(ErrorType.TYPE_ERROR, "invalid variable type")

        # no shadowing within function
        if self.env.exists(name):
            self.error(ErrorType.NAME_ERROR, "variable already defined")

        self.env.define(name, self.default(t), is_block=is_block)

    def assign(self, st):
        lhs = st.get("var")
        rval = self.eval_expr(st.get("expression"))
        rtype = self.runtime_type(rval)

        # simple variable assignment
        if "." not in lhs:
            if not self.env.exists(lhs):
                self.error(ErrorType.NAME_ERROR, "variable not defined")
            expected = self.declared_type(lhs)
            if expected is None or expected == "void" or expected != rtype:
                self.error(ErrorType.TYPE_ERROR, "type mismatch in assignment")

            target = self.env.get(lhs)
            if isinstance(target, BrewinReference):
                target.set_value(rval)
            else:
                self.env.set(lhs, rval)
            return

        # dotted assignment
        parent, field = self.resolve_path(lhs)

        # parent must be an object; resolve_path guarantees that
        field_type = self.declared_type(field)
        if field_type is None or field_type == "void":
            self.error(ErrorType.TYPE_ERROR, "field name must end with valid type suffix")

        if field_type != rtype:
            self.error(ErrorType.TYPE_ERROR, "type mismatch in field assignment")

        parent.fields[field] = rval  # creates or updates

    def do_if(self, st):
        cond = self.eval_expr(st.get("condition"))
        if self.runtime_type(cond) != "bool":
            self.error(ErrorType.TYPE_ERROR, "if condition must be boolean")

        block = st.get("statements") if cond else st.get("else_statements")
        if block:
            self.env.push_block()
            try:
                for s in block:
                    self.exec_stmt(s)
            finally:
                self.env.pop_block()

    def do_while(self, st):
        cond_expr = st.get("condition")
        body = st.get("statements")

        while True:
            cond = self.eval_expr(cond_expr)
            if self.runtime_type(cond) != "bool":
                self.error(ErrorType.TYPE_ERROR, "while condition must be boolean")
            if not cond:
                break

            self.env.push_block()
            try:
                for s in body:
                    self.exec_stmt(s)
            finally:
                self.env.pop_block()

    # Dotted name resolution (for reads / parent resolution)

    def resolve_path(self, dotted):
        """
        Returns (parent_object, final_field_name) for dotted names.
        For simple names, returns (None, varname).
        """
        path = dotted.split(".")
        base = path[0]

        val = self.env.get(base)
        if val is None:
            self.error(ErrorType.NAME_ERROR, "variable not defined")

        # Simple name - not dotted
        if len(path) == 1:
            return None, base

        # Dotted - base must be object-typed by name
        if not base.endswith("o"):
            self.error(ErrorType.TYPE_ERROR, "base must be object-typed")

        if isinstance(val, BrewinReference):
            val = val.get_value()

        if val is self.NIL:
            self.error(ErrorType.FAULT_ERROR, "nil dereference")
        if not isinstance(val, BrewinObject):
            self.error(ErrorType.TYPE_ERROR, "base not object")

        cur = val

        # traverse intermediates
        for seg in path[1:-1]:
            if not seg.endswith("o"):
                self.error(ErrorType.TYPE_ERROR, "intermediate must be object-typed")

            nxt = cur.fields.get(seg)
            if nxt is None:
                self.error(ErrorType.NAME_ERROR, "field not found")

            if isinstance(nxt, BrewinReference):
                nxt = nxt.get_value()

            if nxt is self.NIL:
                self.error(ErrorType.FAULT_ERROR, "nil dereference")
            if not isinstance(nxt, BrewinObject):
                self.error(ErrorType.TYPE_ERROR, "intermediate not object")

            cur = nxt

        return cur, path[-1]

    def eval_expr(self, e):
        k = e.elem_type

        # literals
        if k == self.INT_NODE:
            return e.get("val")
        if k == self.STRING_NODE:
            return e.get("val")
        if k == self.BOOL_NODE:
            return e.get("val")
        if k == self.NIL_NODE:
            return self.NIL
        if k == self.EMPTY_OBJ_NODE:
            return BrewinObject()

        # variable / field read
        if k == self.QUALIFIED_NAME_NODE:
            name = e.get("name")
            parent, field = self.resolve_path(name)

            if parent is None:
                # simple variable
                val = self.env.get(field)
                if val is None:
                    self.error(ErrorType.NAME_ERROR, "variable not defined")
            else:
                if field not in parent.fields:
                    self.error(ErrorType.NAME_ERROR, "field not found")
                val = parent.fields[field]

            if isinstance(val, BrewinReference):
                val = val.get_value()
            return val

        # function call expression
        if k == self.FCALL_NODE:
            return self.run_function(e.get("name"), e.get("args"))

        # binary operators
        if k in self.binary_ops:
            l = self.eval_expr(e.get("op1"))
            r = self.eval_expr(e.get("op2"))
            return self.binary_op(k, l, r)

        # unary negation
        if k == self.NEG_NODE:
            v = self.eval_expr(e.get("op1"))
            if self.runtime_type(v) != "int":
                self.error(ErrorType.TYPE_ERROR, "negation expects int")
            return -v

        # logical NOT
        if k == self.NOT_NODE:
            v = self.eval_expr(e.get("op1"))
            if self.runtime_type(v) != "bool":
                self.error(ErrorType.TYPE_ERROR, "NOT expects bool")
            return not v

        # explicit conversion
        if k == self.CONVERT_NODE:
            return self.convert(e)

        self.error(ErrorType.TYPE_ERROR, "invalid expression")

    def binary_op(self, op, l, r):
        tl = self.runtime_type(l)
        tr = self.runtime_type(r)

        # equality/inequality
        if op == "==":
            # different types are never equal
            if tl != tr:
                return False
            if tl == "object":
                # object equality by reference (including nil)
                return l is r
            # primitive equality by value
            return l == r

        if op == "!=":
            return not self.binary_op("==", l, r)

        # string concatenation
        if op == "+" and tl == tr == "string":
            return l + r

        # integer arithmetic
        if op in {"+", "-", "*", "/"}:
            if tl != "int" or tr != "int":
                self.error(ErrorType.TYPE_ERROR, "arithmetic expects ints")
            if op == "+":
                return l + r
            if op == "-":
                return l - r
            if op == "*":
                return l * r
            if op == "/":
                return l // r

        # integer comparisons
        if op in {"<", "<=", ">", ">="}:
            if tl != "int" or tr != "int":
                self.error(ErrorType.TYPE_ERROR, "comparison expects ints")
            if op == "<":
                return l < r
            if op == "<=":
                return l <= r
            if op == ">":
                return l > r
            if op == ">=":
                return l >= r

        # boolean logical ops
        if op in {"&&", "||"}:
            if tl != "bool" or tr != "bool":
                self.error(ErrorType.TYPE_ERROR, "logical ops expect bools")
            if op == "&&":
                return l and r
            if op == "||":
                return l or r

        self.error(ErrorType.TYPE_ERROR, "invalid binary operation")

    def convert(self, e):
        v = self.eval_expr(e.get("expr"))
        src = self.runtime_type(v)
        dst = e.get("to_type")

        if src == "object":
            self.error(ErrorType.TYPE_ERROR, "cannot convert object type")

        if dst == "int":
            if src == "int":
                return v
            if src == "bool":
                return 1 if v else 0
            if src == "string":
                try:
                    return int(v)
                except ValueError:
                    self.error(
                        ErrorType.TYPE_ERROR,
                        "invalid string for int conversion"
                    )

        if dst == "str":
            if src == "string":
                return v
            if src == "int":
                return str(v)
            if src == "bool":
                return str(v).lower()

        if dst == "bool":
            if src == "bool":
                return v
            if src == "int":
                return v != 0
            if src == "string":
                return len(v) > 0

        self.error(ErrorType.TYPE_ERROR, "invalid conversion")