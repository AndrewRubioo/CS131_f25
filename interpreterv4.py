from intbase import InterpreterBase, ErrorType
from brewparse import parse_program

class ReturnSignal(Exception):
    def __init__(self, value):
        self.value = value

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
        for frame in reversed(self.stack):
            for scope in reversed(frame):
                if name in scope:
                    return True
        return False

    def define(self, name, value, is_block):
        frame = self.stack[-1]
        target_scope = frame[-1] if is_block else frame[0]
        target_scope[name] = value

    def get(self, name):
        for frame in reversed(self.stack):
            for scope in reversed(frame):
                    if name in scope:
                        return scope[name]
        return None

    def set(self, name, value):
        for frame in reversed(self.stack):
            for scope in reversed(frame):
                if name in scope:
                    scope[name] = value
                    return True
        return False

    def clone_env(self, env):
        new_env = Environment()
        new_env.stack = []
        for frame in env.stack():
            new_frame = []
            for scope in frame:
                new_frame.append(scope.copy())
            new_env.stack.append(new_frame)
        return new_env

class FunctionValue:
    def __init__(self, fn_ast, closure_env=None, is_lambda=False):
        self.fn_ast = fn_ast
        self.closure_env = closure_env
        self.is_lambda = is_lambda

class Interpreter(InterpreterBase):

    VAR_DEF_NODE = "vardef"
    BVAR_DEF_NODE = "bvardef"

    INTERFACE_NODE = "interface"
    FIELD_VAR_NODE = "field_var"
    FIELD_FUNC_NODE = "field_func"
    CLOSURE_NODE = "closure"

    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)

        self.env = Environment()
        self.current_closure_env = None
        self.funcs = {}  # (name, param_sig) -> FunctionValue
        self.interfaces = {} # "A" -> interface spec
        self.NIL = object()
        self.VOID = object()
        self.suffix_map = {
            'i': 'int',
            's': 'string',
            'b': 'bool',
            'o': 'object',
            'v': 'void',
            'f': 'function'
        }

        self.binary_ops = {
            "+", "-", "*", "/", "==", "!=", "<", "<=", ">", ">=", "&&", "||"
        }

    def declared_type(self, name):
        if not isinstance(name, str) or len(name) == 0:
            return None
        
        last = name[-1]
        if last == 'f':
            return "function"
        
        if last in self.suffix_map:
            return self.suffix_map[last]
        
        if last.isupper():
            return ("interface", last)
        
        return None

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
        if isinstance(val, FunctionValue):
            return "function"
        if isinstance(val, bool):
            return "bool"
        if isinstance(val, int):
            return "int"
        if isinstance(val, str):
            return "string"
        return "object"

    def default(self, t):
        if self.is_interface_type(t):
            return self.NIL
        if t == "function":
            return self.NIL
        return {
            "int": 0,
            "string": "",
            "bool": False,
            "object": self.NIL,
            "void": self.VOID
        }[t]

    def clone_env(self, env):
        new_env = Environment()
        new_env.stack = []
        for frame in env.stack:
            new_frame = []
            for scope in frame:
                new_frame.append(scope.copy())
            new_env.stack.append(new_frame)
        return new_env

    def make_lambda(self, fn_ast):
        closure = self.clone_env(self.env)
        return FunctionValue(fn_ast, closure_env=closure, is_lambda=True)

    # Function table construction

    def process_interfaces(self, iface_node):
        name = iface_node.get("name")

        if not isinstance(name, str) or len(name) != 1 or not name.isupper():
            self.error(ErrorType.NAME_ERROR, "invalid interface name")

        if name in self.interfaces:
            self.error(ErrorType.NAME_ERROR, "interface redeclared")

        fields = iface_node.get("fields") or []

        var_fields = {} 
        func_fields = {} #funcname -> [ (param_name, type_letter, is_ref),]

        seen = set()

        for fld in fields:
            ftype = fld.elem_type
            fname = fld.get("name")

            if fname in seen:
                self.error(ErrorType.NAME_ERROR, "duplicate field in interface")
            seen.add(fname)

            if ftype == self.FIELD_VAR_NODE:
                tletter = fname[-1]
                if tletter not in self.suffix_map and not tletter.isupper():
                    self.error(ErrorType.TYPE_ERROR, "invalid type in interface field")
                
                var_fields[fname] = tletter

            # field is function requriement
            elif ftype == self.FIELD_FUNC_NODE:
                params = []
                for arg in fld.get("params"):
                    pname = arg.get("name")
                    is_ref = arg.get("ref")
                    tletter = pname[-1]

                    if not (tletter in self.suffix_map or tletter.isupper()):
                        self.error(ErrorType.TYPE_ERROR, "invalid param type in interface func field")
                    
                    # check referenced interface types exist
                    if tletter.isupper() and tletter not in self.interfaces:
                        self.error(ErrorType.NAME_ERROR, "referenceed interface not defined")

                    params.append((pname, tletter, is_ref))
            
                func_fields[fname] = params
        
            else:
                self.error(ErrorType.TYPE_ERROR, "invalid field in interface")
        
        # save interface specification
        self.interfaces[name] = {"vars":var_fields, "funcs":func_fields}

    def check_interface(self, iface_name, obj):
        if obj is self.NIL: # allowed per spec
            return True
        
        if not isinstance(obj, BrewinObject):
            self.error(ErrorType.TYPE_ERROR, f"value does not satisfy interface {iface_name}")

        iface = self.interfaces.get(iface_name)
        for varname, tletter in iface["vars"].items():
            if varname not in obj.fields:
                self.error(ErrorType.TYPE_ERROR, f"object missing required field {varname}")

            val = obj.fields[varname]
            actual_type = self.runtime_type(val)

            if tletter.isupper():
                expected_type = "object"
            else:
                expected_type = self.suffix_map[tletter]
            
            if actual_type != expected_type:
                self.error(ErrorType.TYPE_ERROR, f"field {varname} type mismatch")

        # function fields
        for fname, params in iface["funcs"].items():
            if fname not in obj.fields:
                self.error(ErrorType.TYPE_ERROR, f"object missing required method {fname}")

            fnval = obj.fields[fname]

            if not isinstance(fnval, FunctionValue):
                self.error(ErrorType.TYPE_ERROR, f"field {fname} is not a function")
            
            fn_ast = fnval.fn_ast
            formal_params = fn_ast.get("args")

            if len(formal_params) != len(params):
                self.error(ErrorType.TYPE_ERROR, f"object missing required method {fname}")
            
            # check each param type compatibility
            for (iface_param, fn_param) in zip(params, formal_params):
                # iface_param = (pname, tletter, is_ref)
                ip_name, ip_letter, ip_ref = iface_param

                fp_name = fn_param.get("name")
                fp_ref = fn_param.get("ref")
                fp_letter = fp_name[-1]

                if ip_ref != fp_ref: # ref match exactly
                    self.error(ErrorType.TYPE_ERROR, f"method {fname} ref mismatch")

                # interface param wants interface B
                if ip_letter.isupper():
                    if fp_letter != ip_letter:
                        self.error(ErrorType.TYPE_ERROR, f"method {fname} interface param mismatch")

                # interface paran wants any object
                elif ip_letter == 'o':
                    if not (fp_letter == 'o' or fp_letter.isupper()):
                        self.error(ErrorType.TYPE_ERROR, f"Method {fname} must accept object or interface")
                else:
                    if fp_letter != ip_letter:
                        self.error(ErrorType.TYPE_ERROR, f"method {fname} param type mismatch")

        return True
            
    # Lambdas and closures

    def is_interface_type(self, declared_type):
        return isinstance(declared_type, tuple) and (declared_type[0] == "interface")

    def param_sig(self, args):
        # return tuple of parameter static types from arg nodes
        sig = []
        for a in args:
            dtype = self.declared_type(a.get("name"))
            if dtype is None:
                self.error(ErrorType.TYPE_ERROR, "invalid formal parameter type")
            if self.is_interface_type(dtype):
                sig.append("object")
            else:
                if dtype == "void":
                    self.error(ErrorType.TYPE_ERROR, "formal paramter cannot be void")
                sig.append(dtype)
        return tuple(sig)

    def run(self, program):
        ast = parse_program(program)

        interfaces = ast.get("interfaces") or []
        for iface_ast in interfaces:
            self.process_interfaces(iface_ast)

        # build function table
        for fn_ast in ast.get("functions") or []:

            fname = fn_ast.get("name")
            fargs = fn_ast.get("args")

            # validate function name / return type
            if fname != "main":
                rettype = self.return_type(fname)
                if rettype not in ("int", "string", "bool", "object", "void", "function") and not self.is_interface_type(rettype):
                    self.error(ErrorType.TYPE_ERROR, "invalid function return type")

            sig = self.param_sig(fargs)

            # reject void-type formal params
            for a in fargs:
                if self.declared_type(a.get("name")) == "void":
                    self.error(ErrorType.TYPE_ERROR, "formal parameter cannot be void")

            key = (fname, sig)
            if key in self.funcs:
                self.error(ErrorType.NAME_ERROR, "duplicate function definition")
            self.funcs[key] = FunctionValue(fn_ast, is_lambda=False)

        if ("main", ()) not in self.funcs:
            self.error(ErrorType.NAME_ERROR, "main missing")

        try:
            self.run_function("main", [])

        except ReturnSignal:
            pass

    def get_function(self, name, arg_types):
        key = (name, tuple(arg_types))
        fn = self.funcs.get(key)
        if fn is None:
            self.error(ErrorType.NAME_ERROR, "function not found")
        return fn
    
    # Built-ins + Function calls

    def call_function_value(self, fnval, arg_exprs, method_self=None):
        # Evaluate arguments in caller environment
        caller_env = self.env
        caller_closure_env = self.current_closure_env

        actual_vals = [self.eval_expr(e) for e in arg_exprs]
        arg_types = [self.runtime_type(v) for v in actual_vals]

        if any(t == "void" for t in arg_types):
            self.error(ErrorType.TYPE_ERROR, "cannot pass void as argument")

        fn_ast = fnval.fn_ast
        fname = fn_ast.get("name")
        rettype = self.return_type(fname)

        # Lambda uses closure, named functions use new env
        if fnval.is_lambda:
            self.env = self.clone_env(fnval.closure_env)
            self.current_closure_env = fnval.closure_env
        else:
            self.env = Environment()
            self.current_closure_env = None
        self.env.enter_func()

        # Inject selfo for methods
        if method_self is not None:
            self.env.define("selfo", method_self, is_block=False)

        # Bind formal
        fargs = fn_ast.get("args")
        if len(fargs) != len(actual_vals):
            self.env.exit_func()
            self.env = caller_env
            self.current_closure_env = caller_closure_env
            self.error(ErrorType.TYPE_ERROR, "wrong number of arguments")

        for formal, arg_expr, actual_val in zip(fargs, arg_exprs, actual_vals):
            pname = formal.get("name")
            ptype = self.declared_type(pname)
            is_ref = formal.get("ref")
            actual_type = self.runtime_type(actual_val)
                                            
            if self.env.exists(pname):
                self.env.exit_func()
                self.env = caller_env
                self.current_closure_env = caller_closure_env
                self.error(ErrorType.NAME_ERROR, "parameter name already defined")

            if self.is_interface_type(ptype):
                base_type = "object"
            else:
                base_type = ptype
            
            self.env.define(pname, self.default(base_type), is_block=False)

            #Interface-typed parameter
            if self.is_interface_type(ptype):
                iface_letter = ptype[1]
                if actual_type != "object":
                    if actual_val is not self.NIL:
                        self.env.exit_func()
                        self.env = caller_env
                        self.current_closure_env = caller_closure_env
                        self.error(ErrorType.TYPE_ERROR, "passing non-object to interface param")

                # enforce interface
                self.check_interface(iface_letter, actual_val)
            
            # by-reference
            if is_ref:
                # build reference into caller's environment
                locator = self.locate_reference(arg_expr, caller_env)
                if not self.is_interface_type(ptype):
                    if self.runtime_type(locator.get_value()) != base_type:
                        self.env.exit_func()
                        self.env = caller_env
                        self.current_closure_env = caller_closure_env
                        self.error(ErrorType.TYPE_ERROR, "reference parameter type mismatch")

                else: 
                    val_for_ref = locator.get_value()
                    if self.runtime_type(val_for_ref) != "object":
                        if val_for_ref is not self.NIL:
                            self.env.exit_func()
                            self.env = caller_env
                            self.current_closure_env = caller_closure_env
                            self.error(ErrorType.TYPE_ERROR, "reference param to interface must be object/nil")
                    self.check_interface(ptype[1], val_for_ref)

                self.env.set(pname, locator)
            else:
                if not self.is_interface_type(ptype):
                    if actual_type != base_type:
                        self.env.exit_func()
                        self.env = caller_env
                        self.error(ErrorType.TYPE_ERROR, "argument type mismatch")
                self.env.set(pname, actual_val)

        ret_val = self.default(rettype if not self.is_interface_type(rettype) else "object")

        try:
            for st in fn_ast.get('statements'):
                self.exec_stmt(st)
        except ReturnSignal as r:
            val = r.value

            if rettype == "void":
                # void functions (including main) cannot return a value
                if val is not self.VOID:
                    self.env.exit_func()
                    self.env = caller_env
                    self.current_closure_env = caller_closure_env
                    self.error(ErrorType.TYPE_ERROR, "void function cannot return a value")
                ret_val = self.NIL

            elif self.is_interface_type(rettype):
                iface_letter = rettype[1]

                rtype = self.runtime_type(val)
                if rtype != "object":
                    if val is not self.NIL:
                        self.env.exit_func()
                        self.env = caller_env
                        self.current_closure_env = caller_closure_env
                        self.error(ErrorType.TYPE_ERROR, "returning non-object to interface return")
                self.check_interface(iface_letter, val)
                ret_val = val

            elif rettype == "function":
                if self.runtime_type(val) != "function":
                    self.env.exit_func()
                    self.env = caller_env
                    self.current_closure_env = caller_closure_env
                    self.error(ErrorType.TYPE_ERROR, "return type mismatch (expected function)")
                ret_val = val

            else:
                if rettype != self.runtime_type(val):
                    self.env.exit_func()
                    self.env = caller_env
                    self.current_closure_env = caller_closure_env                 
                    self.error(ErrorType.TYPE_ERROR, "return type mismatch")
                ret_val = val

        self.env.exit_func()
        self.env = caller_env
        self.current_closure_env = caller_closure_env
        return ret_val

    def run_function(self, name, arg_exprs):
        if name in ("print", "inputi", "inputs"):
            return self.run_builtin(name, arg_exprs)

        actual_vals = [self.eval_expr(e) for e in arg_exprs]
        arg_types = [self.runtime_type(v) for v in actual_vals]

        # Passing void as an argument is illegal
        if any(t == "void" for t in arg_types):
            self.error(ErrorType.TYPE_ERROR, "cannot pass void as argument")

        fnval = self.get_function(name, arg_types)

        return self.call_function_value(fnval, arg_exprs, method_self=None)

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
            seg_decl = self.declared_type(seg)
            if not (seg_decl == "object" or self.is_interface_type(seg_decl)):
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
            fname = st.get("name")
            args = st.get("args")

            if "." not in fname:
                if self.env.exists(fname):
                    val = self.env.get(fname)
                    if val is self.NIL:
                        self.error(ErrorType.FAULT_ERROR, "calling nil function value")
                    if isinstance(val, FunctionValue):
                        self.call_function_value(val, args, method_self=None)
                        return
                    self.error(ErrorType.TYPE_ERROR, "calling non-function value")

                self.run_function(fname, args)
                return
            parent, field = self.resolve_path(fname)
            if parent is None:
                self.error(ErrorType.TYPE_ERROR, "invalid method call base")
                
            if field not in parent.fields:
                self.error(ErrorType.NAME_ERROR, "field not found in method call")

            fnval = parent.fields[field]

            if fnval is self.NIL:
                self.error(ErrorType.FAULT_ERROR, "calling nil method value")

            if not isinstance(fnval, FunctionValue):
                self.error(ErrorType.TYPE_ERROR, "calling non-function field")

            # method_self is the object that owns the field
            self.call_function_value(fnval, args, method_self=parent)

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

        frame = self.env.stack[-1]
        if is_block: # only forbid duplicates in the current block scope
            current_scope = frame[-1]
            if name in current_scope:
                self.error(ErrorType.NAME_ERROR, "variable already defined")
        else: # forbid duplicates in function-level scope
            func_scope = frame[0]
            if name in func_scope:
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

            if self.is_interface_type(expected):
                iface_letter = expected[1]

                if rtype not in ("object",):
                    if rval is not self.NIL:
                        self.error(ErrorType.TYPE_ERROR, "assigning non-object to interface")
                self.check_interface(iface_letter, rval)

            else:
                if expected != rtype:
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
        field_declaration = self.declared_type(field)

        if self.is_interface_type(field_declaration):
            iface_letter = field_declaration[1]

            if rtype not in ("object",):
                if rval is not self.NIL:
                    self.error(ErrorType.TYPE_ERROR, "assign non-object to interface field")

            self.check_interface(iface_letter, rval)
        
        elif field_declaration == "function":
            if rtype != "function":
                self.error(ErrorType.TYPE_ERROR, "assign non-function to function field")

        else: #regular primitivr/object
            if field_declaration != rtype:
                self.error(ErrorType.TYPE_ERROR, "field type mismatch")

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
        # base_decl = self.declared_type(base)
        # if not (base_decl == "object" or self.is_interface_type(base_decl)):
        #     self.error(ErrorType.TYPE_ERROR, "base must be object-typed")

        if isinstance(val, BrewinReference):
            val = val.get_value()

        if val is self.NIL:
            self.error(ErrorType.FAULT_ERROR, "nil dereference")
        if not isinstance(val, BrewinObject):
            self.error(ErrorType.TYPE_ERROR, "base not object")

        cur = val

        # traverse intermediates
        for seg in path[1:-1]:

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

            if "." not in name:
                # try variable
                val = self.env.get(name)
                if val is not None:
                    if isinstance(val, BrewinReference):
                        val = val.get_value()
                    return val
                
                # # zero arg named functions
                # matches = [fv for (fname, sig), fv in self.funcs.items()
                #            if fname == name]
                # if len(matches) == 1:
                #     return matches[0]

                # named function value
                matches = [fv for (fname, sig), fv in self.funcs.items()
                           if fname == name]
                if len(matches) == 1:
                    return matches[0]
                if len(matches) > 1:
                    self.error(ErrorType.NAME_ERROR, "ambiguous function reference")
                                     
                # undefined
                self.error(ErrorType.NAME_ERROR, "variable not defined")
            
            parent, field = self.resolve_path(name)
            if parent is None:
                self.error(ErrorType.TYPE_ERROR, "invalid field acess")

            if field not in parent.fields:
                self.error(ErrorType.NAME_ERROR, "field not found")

            val = parent.fields[field]
            if isinstance(val, BrewinReference):
                val = val.get_value()
            return val

        # function call expression
        if k == self.FCALL_NODE:
            fname = e.get("name")
            args = e.get("args")

            if "." not in fname:
                # If a variable of this name exists, try function-variable call
                if self.env.exists(fname):
                    val = self.env.get(fname)
                    if val is self.NIL:
                        self.error(ErrorType.FAULT_ERROR, "calling nil function value")
                    if isinstance(val, FunctionValue):
                        return self.call_function_value(val, args, method_self=None)
                    # variable exists but is not a function
                    self.error(ErrorType.TYPE_ERROR, "calling non-function value")

                # No variable treat as named function
                return self.run_function(fname, args)

            parent, field = self.resolve_path(fname)
            if parent is None:
                self.error(ErrorType.TYPE_ERROR, "invalid method call base")

            if field not in parent.fields:
                self.error(ErrorType.NAME_ERROR, "method field not found")

            fnval = parent.fields[field]

            if fnval is self.NIL:
                self.error(ErrorType.FAULT_ERROR, "calling nil method value")

            if not isinstance(fnval, FunctionValue):
                self.error(ErrorType.TYPE_ERROR, "calling non-function field")

            # Perform method call
            return self.call_function_value(fnval, args, method_self=parent)   

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

        if k == "func":
            return self.make_lambda(e)
        
        if k == self.CLOSURE_NODE:
            varname = e.get("args")
            if self.current_closure_env is None:
                self.error(ErrorType.NAME_ERROR, "closure has no environment")
            
            closure_env = self.current_closure_env

            if not closure_env.exists(varname):
                self.error(ErrorType.NAME_ERROR, "closure variable not found")

            val = closure_env.get(varname)
            if isinstance(val, BrewinReference):
                val = val.get_value()
            return val
        
        self.error(ErrorType.TYPE_ERROR, "invalid expression")

    def binary_op(self, op, l, r):
        tl = self.runtime_type(l)
        tr = self.runtime_type(r)

        # equality/inequality
        if op == "==":
            if isinstance(l,FunctionValue) and isinstance(r, FunctionValue):
                return l is r
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