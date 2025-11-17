from intbase import InterpreterBase, ErrorType
from brewparse import parse_program

generate_image = False

class Return_Exception(Exception):
    def __init__(self, value):
        self.value = value

class Environment:
    def __init__(self, nil_value=None):
        self.scopes = []
        self.function_scope = set()
        self.NIL_VALUE = nil_value

    def push_scope(self):
        self.scopes.append({})
    
    def pop_scope(self):
        if len(self.scopes) > 1:
            self.scopes.pop()

    def defined_in_function(self, varname):
        return varname in self.function_scope
    
    # define new variable at function scope
    def fdef(self, var_type, varname, scope_type, default_value):
        if self.defined_in_function(varname):
            return False #no shadowing
        
        var_data = {'type': var_type, 'value': default_value}
        if scope_type == 'func':
            self.scopes[0][varname] = var_data
        elif scope_type == 'block':
            self.scopes[-1][varname] = var_data

        self.function_scope.add(varname)

        return True

    def get_var_info(self, varname):
        for scope in reversed(self.scopes):
            if varname in scope:
                return scope[varname]
        return None
            
    def exists(self, varname):
        return self.get_var_info(varname) is not None
    
    def get_value(self, varname):
        info = self.get_var_info(varname)
        if info:
            return info['value']
        return None
    
    def set_value(self, varname, value):
        for scope in reversed(self.scopes):
            if varname in scope:
                scope[varname]['value'] = value #update value for scope
                return True
        return False # variable not found - defined

class Interpreter(InterpreterBase):
        
    VAR_DEF_NODE = "vardef" #intbase
    BVAR_DEF_NODE = "bvardef"
    FUNCTION_SCOPE = 'func'
    BLOCK_SCOPE = 'block'

    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)

        self.NIL_VALUE = None
        self.VOID_VALUE = object()
        self.funcs = {}  # { (name, arity) : element }
        self.env = Environment(self.NIL_VALUE)
        self.ops = { "-", "+", "*", "/", "==", "!=", "<", ">", "<=", ">=", "&&", "||" }

        self.type_suffixes = { 'i': 'int', 's' : 'string', 'b': 'bool', 'o' : 'object', 'v' : 'void' }

    def get_var_type_from_name(self, name):
        # must be 2 chars since 1 is type suffix
        if type(name) != str or len(name) < 1:
            return None
        
        suffix = name[-1]
        if suffix == 'v': #void function return
            return None

        return self.type_suffixes.get(suffix)
    
    def get_return_type_from_name(self, name):
        if name == 'main':
            return 'void'
        
        if type(name) != str or len(name) < 1:
            return None
        
        suffix = name[-1]
        return self.type_suffixes.get(suffix)

    def get_default_value(self, typename):
        if typename == 'int': return 0
        if typename == 'string' : return ""
        if typename == 'bool' : return False
        if typename == 'object' : return self.NIL_VALUE
        if typename == 'void' : return self.VOID_VALUE
 
        return self.NIL_VALUE
    
    # Lookup and Validation
    
    def get_parameter_type_signature(self, formal_args):
        param_types = []

        for arg in formal_args: # list of AST
            arg_name = arg.get("name")
            var_type = self.get_var_type_from_name(arg_name)

            if var_type is None:
                super().error(ErrorType.TYPE_ERROR, f"Formal parameter '{arg_name}' has invalid or 'v' type suffix")
            param_types.append(var_type)

        return param_types
    
    def get_runtime_type(self, value):
        if type(value) is int:
            return 'int'
        if type(value) is str:
            return 'string'
        if type(value) is bool:
            return 'bool'
        if value is self.NIL_VALUE:
            return 'object'
        if value is self.VOID_VALUE:
            return 'void'
        if isinstance(value, BrewinObject):
            return 'object'
        if isinstance(value, BrewinReference):
            return self.get_runtime_type(value.get_value())
        
        return 'object'
        
    # update get function, no more arity
    def get_function(self, name, arg_types):
        func_key = (name, tuple(arg_types))

        if func_key in self.funcs:
            return self.funcs[func_key]
        
        super().error(ErrorType.NAME_ERROR, f"Function {name} not found")

    def get_locator(self, expr, env):
        if expr.elem_type != self.QUALIFIED_NAME_NODE:
            super().error(ErrorType.TYPE_ERROR, "Reference argument must be a variable or field")

        dotted_name = expr.get("name")
        path = dotted_name.split('.')
        base_name = path[0]

        if len(path) == 1:
            if not env.exists(base_name):
                super().error(ErrorType.NAME_ERROR, f"Variable '{base_name}' not defined")
            return BrewinReference(env, base_name)
        
        # resolve parent object
        base_info = env.get_var_info(base_name)

        if base_info is None or base_info['type'] != 'object':
            super().error(ErrorType.TYPE_ERROR, f"Base variable '{base_name}' is not object")

        current_obj = env.get_value(base_name)

        for i in range(1, len(path) - 1):
            segment = path[i]
            if current_obj is self.NIL_VALUE:
                super().error(ErrorType.FAULT_ERROR, f"Cannot dereference through nil object at '{path[i-1]}' ")

            # intermediate must be object
            if self.get_var_type_from_name(segment) != 'object':
                super().error(ErrorType.TYPE_ERROR, f"Intermediate field '{segment}' must be object typed ")

            if segment not in current_obj.fields:
                super().error(ErrorType.NAME_ERROR, f"Intermediate object field '{segment}' not found.")
                        
            current_obj = current_obj.fields[segment]
        
        if current_obj is self.NIL_VALUE:
            super().error(ErrorType.FAULT_ERROR, f"Cannot derefernece nil object at '{path[-2]}'")

        final_field = path[-1]

        return BrewinReference(self.env, base_name, current_obj, final_field)

    def validate_function_signature(self, name, fomral_args):
        return_type = self.get_return_type_from_name(name)

        if return_type is None:
            super().error(ErrorType.TYPE_ERROR, f"Function name '{name}' must end with valid return type suffic")

        for arg in fomral_args:
            arg_name = arg.get("name")
            if self.get_var_type_from_name(arg_name) is None:
                super().error(ErrorType.TYPE_ERROR, f"Formal parameter '{arg_name}' must end with valid return type suffic")

    def run(self, program):
        ast = parse_program(program, generate_image)
        self.funcs = {}

        for func in ast.get("functions"):
            name = func.get("name")
            formal_args = func.get("args")

            self.validate_function_signature(name, formal_args)

            param_types = self.get_parameter_type_signature(formal_args)
            func_key = (name, tuple(param_types))

            if func_key in self.funcs:
                # check if signature duplicate
                super().error(ErrorType.NAME_ERROR, f"Duplicate function definition fir {name} with type signature {param_types}")
            self.funcs[func_key] = func

        if ("main", ()) not in self.funcs:
            super().error(ErrorType.NAME_ERROR, "main function not found (needed)")
                            
        try:
            self.__run_function("main", []) # using new method to run multiple function scopes + new functions
        except Return_Exception:
            pass
    
    def __run_function(self, name, arg_expressions):

        actual_values = []
        for expr in arg_expressions:
            actual_values.append(self.__eval_expr(expr))

        actual_arg_types = []
        for val in actual_values:
            actual_arg_types.append(self.get_runtime_type(val))
        
        # passing in tuple of types now
        func_ast = self.get_function(name, actual_arg_types)
        return_type = self.get_return_type_from_name(name)

        caller_env = self.env # save environment for function return
        self.env = Environment(self.NIL_VALUE)
        self.env.push_scope()

        formal_args = func_ast.get("args") #initialize args - pass by value
        for i, formal_arg in enumerate(formal_args):
            var_name = formal_arg.get("name")
            is_ref = formal_arg.get("ref")
            var_type = self.get_var_type_from_name(var_name)
            # fdef now stores var type and name
            if not self.env.fdef(var_type, var_name, self.FUNCTION_SCOPE, self.get_default_value(var_type)):
                super().error(ErrorType.NAME_ERROR, f"Paramter {var_name} already defined")

            if is_ref:
                locator = self.get_locator(arg_expressions[i], caller_env)
                ref_value = locator.get_value()
                ref_type = self.get_runtime_type(ref_value)

                if ref_type != var_type:
                    super().error(ErrorType.TYPE_ERROR, f"Cannot pass reference of type {ref_type} to param '{var_name}'" )

                self.env.set_value(var_name, locator)

            else:
                self.env.set_value(var_name, actual_values[i])

        return_value = self.get_default_value(return_type)

        try:
            for statement in func_ast.get("statements"):
                self.__execute_statement(statement)
        
        except Return_Exception as e:
            returned_value = e.value
            returned_type = self.get_runtime_type(returned_value)

            if return_type == 'void':
                if returned_value is not self.NIL_VALUE:
                    #void function or main cannot return value
                    super().error(ErrorType.TYPE_ERROR, f"Void function '{name}' cannot return a value")
            elif returned_type != return_type:
                # value returned doesn't match the function's declared type
                super().error(ErrorType.TYPE_ERROR, f"Function '{name}' returns type {returned_type}, expected {return_type}")

            return_value = returned_value

        self.env = caller_env # restore caller scope

        return return_value

    def __execute_statement(self, statement):
        kind = statement.elem_type

        if kind == self.VAR_DEF_NODE:
            self.__run_vardef(statement)

        elif kind == self.BVAR_DEF_NODE:
            self.__run_bvardef(statement)

        elif kind == "=":
            self.__run_assign(statement)

        elif kind == self.FCALL_NODE:
            self.__run_fcall(statement)

        elif kind == self.RETURN_NODE:
            self.__run_return(statement)

        elif kind == self.IF_NODE:
            self.__run_if(statement)

        elif kind == self.WHILE_NODE:
            self.__run_while(statement)

    def __run_return(self, statement):

        expression = statement.get("expression")
        return_value = self.VOID_VALUE

        if expression:
            return_value = self.__eval_expr(expression)

        raise Return_Exception(return_value)
    
    def __run_if(self, statement):

        condition_expr = statement.get("condition")
        condition_value = self.__eval_expr(condition_expr)

        if not isinstance(condition_value, bool):
            super().error(ErrorType.TYPE_ERROR, "Statement conditiion must be a boolean")

        if condition_value:
            self.env.push_scope()
            try:
                true_statements = statement.get("statements")
                for stmt in true_statements:
                    self.__execute_statement(stmt)
            finally: # pop once block finishes
                self.env.pop_scope()
        else:
            else_statements = statement.get("else_statements")
            if else_statements:
                self.env.push_scope()
                try:
                    for stmt in else_statements:
                        self.__execute_statement(stmt)
                finally:
                    self.env.pop_scope()

    def __run_while(self, statement):
        # similar to checking if - check each statement while true
        condition_expr = statement.get("condition")
        loop_statements = statement.get("statements")

        while True:
            condition_value = self.__eval_expr(condition_expr)

            if not isinstance(condition_value, bool):
                super().error(ErrorType.TYPE_ERROR, "While statement condition must be a boolean")

            if not condition_value:
                break
            
            self.env.push_scope()
            try:
                for stmt in loop_statements:
                    self.__execute_statement(stmt)
            finally:
                self.env.pop_scope()
        
    def __run_vardef(self, statement):
        # if statement evaluates boolean
        name = statement.get("name")
        var_type = self.get_var_type_from_name(name)

        if var_type is None:
            super().error(ErrorType.TYPE_ERROR, f"Variable name '{name}' must end with a valid type suffix (i, s, b, o)")

        if not self.env.fdef(var_type, name, self.FUNCTION_SCOPE, self.get_default_value(var_type)):
            super().error(ErrorType.NAME_ERROR, f"Variable '{name}' already defined")

    def __run_bvardef(self, statement):
        name = statement.get("name")
        var_type = self.get_var_type_from_name(name)

        if var_type is None:
            super().error(ErrorType.TYPE_ERROR, f"Block variable name '{name}' must end with type suffix")
        
        if not self.env.fdef(var_type, name, self.BLOCK_SCOPE, self.get_default_value(var_type)):
            super().error(ErrorType.NAME_ERROR, f"Variable '{name}' already defined")

    def __run_assign(self, statement):
        name = statement.get("var")
        value = self.__eval_expr(statement.get("expression"))
        runtime_type = self.get_runtime_type(value)

        if '.' not in name:
            var_info = self.env.get_var_info(name)
            if var_info is None:
                super().error(ErrorType.NAME_ERROR, f"Variable '{name}' not defined")

            declared_type = var_info['type']
            target_value = var_info['value']
            
            # type check
            if declared_type != runtime_type:
                super().error(ErrorType.TYPE_ERROR, f"Cannot assign value of type {runtime_type} to variable '{name}' of declared type {declared_type}")

            # assignment - check if target is a reference
            if isinstance(target_value, BrewinReference):
                target_value.set_value(value)
            else:
                self.env.set_value(name, value)
                
        else:  # dotted field assignment
            path = name.split('.')
            base_name = path[0]
            final_field_name = path[-1]

            #check base variable exists
            base_info = self.env.get_var_info(base_name)
            if base_info is None:
                super().error(ErrorType.NAME_ERROR, f"Variable '{base_name}' not defined")
            
            current_obj = self.env.get_value(base_name)

            if isinstance(current_obj, BrewinReference):
                current_obj = current_obj.get_value()

            if current_obj is self.NIL_VALUE:
                super().error(ErrorType.FAULT_ERROR, f"Cannot dereference nil object '{base_name}'")

            if not isinstance(current_obj, BrewinObject):
                super().error(ErrorType.TYPE_ERROR, f"Base variable '{base_name}' is not an object")

            for i in range(1, len(path) - 1):
                segment = path[i]

                if segment not in current_obj.fields:
                    super().error(ErrorType.NAME_ERROR, f"Intermediate field '{segment}' not found.")

                # segment must be object-typed
                if current_obj is self.NIL_VALUE: 
                    super().error(ErrorType.FAULT_ERROR, f"Cannot dereference through nil object at intermediate field '{path[i-1]}'")

                if self.get_var_type_from_name(segment) != 'object':
                    super().error(ErrorType.TYPE_ERROR, f"Intermediate field '{segment}' must be object-typed")

                # current_obj must be a real object
                if not isinstance(current_obj, BrewinObject):
                    super().error(ErrorType.FAULT_ERROR, f"Cannot dereference nil object '{path[i-1]}'")

                current_obj = current_obj.fields[segment]

            parent_obj = current_obj

            if not isinstance(parent_obj, BrewinObject):
                super().error(ErrorType.FAULT_ERROR, f"Cannot assign field '{final_field_name}' to nil object")

            # final type check
            declared_field_type = self.get_var_type_from_name(final_field_name)

            if declared_field_type is None:
                super().error(ErrorType.TYPE_ERROR, f"Field name '{final_field_name}' must end with a valid type suffix (i, s, b, o)")

            if declared_field_type != runtime_type:
                super().error(ErrorType.TYPE_ERROR, f"Cannot assign value of type {runtime_type} to field '{final_field_name}' of declared type {declared_field_type}")

            parent_obj.fields[final_field_name] = value

    def __run_fcall(self, statement):
        fcall_name = statement.get("name")
        arg_expressions = statement.get("args")

        # handle built-ins
        if fcall_name in ["inputi", "inputs", "print"]:
            actual_args = [self.__eval_expr(expr) for expr in arg_expressions]
            arity = len(actual_args)

        ################
            if fcall_name == "inputi":
                if arity > 1:
                    super().error(ErrorType.NAME_ERROR, "too many arguments for inputi")

                if arity == 1:
                    if isinstance(actual_args[0], bool):
                        super().output(str(actual_args[0]).lower()) # true or false
                    else: 
                        super().output(str(actual_args[0]))
                try:
                    return int(super().get_input())
                except (ValueError, TypeError):
                    super().error(ErrorType.TYPE_ERROR, "Invalid input of inputi, expected an integer")

            elif fcall_name == "inputs":
                if arity > 1:
                    super().error(ErrorType.NAME_ERROR, "Too many arguments for inputs")

                if arity == 1:
                    if isinstance(actual_args[0], bool):
                        super().output(str(actual_args[0]).lower())
                    else: 
                        super().output(str(actual_args[0]))

                return super().get_input()

            elif fcall_name == "print":
                out = ""
                for arg in actual_args:
                    if isinstance(arg, bool):
                        out += str(arg).lower()
                    elif arg is self.NIL_VALUE:
                        out += "nil"
                    else:
                        out += str(arg)
                super().output(out)
                return self.NIL_VALUE

        return self.__run_function(fcall_name, arg_expressions)
    
    def __run_convert(self, to_type, expr):

        value = self.__eval_expr(expr)
        from_type = self.get_runtime_type(value)

        if from_type == 'object':
            super().error(ErrorType.TYPE_ERROR, f"Cannot convert type '{from_type}' to '{to_type}'.")

        # int conversions
        if to_type == 'int':
            if from_type == 'int':
                return value # int -> int
            if from_type == 'bool':
                return 1 if value else 0
            if from_type == 'string':
                try:
                    return int(value)
                except ValueError:
                    super().error(ErrorType.TYPE_ERROR, f"Invalid string format for conversion to int: '{value}'.")
        
        # string conversions
        elif to_type == 'str':
            if from_type == 'string':
                return value # string -> string
            if from_type == 'int':
                return str(value) # int -> decimal string
            if from_type == 'bool':
                return str(value).lower()
        
        # bool conversions
        elif to_type == 'bool':
            if from_type == 'bool':
                return value # bool -> bool
            if from_type == 'int':
                return value != 0 # int -> false iff 0
            if from_type == 'string':
                return len(value) > 0 # string -> false iff empty string
        
        super().error(ErrorType.TYPE_ERROR, f"Invalid conversion from {from_type} to {to_type}.")

    def __eval_expr(self, expr):
        kind = expr.elem_type

        if kind == self.INT_NODE or kind == self.STRING_NODE:
            return expr.get("val")
        elif kind == 'bool':
            return expr.get("val")
        elif kind == "nil":
            return self.NIL_VALUE 
        # handle variable lookup
        elif kind == self.QUALIFIED_NAME_NODE:
            var_name = expr.get("name")
            value = self.__resolve_dotted_name(var_name)

            if isinstance(value, BrewinReference):
                value = value.get_value()
            return value
        
        # handle function calls
        elif kind == self.FCALL_NODE:
            return self.__run_fcall(expr)
        
        #handle unary ops
        elif kind == 'neg' or kind == '!':
            operand_value = self.__eval_expr(expr.get("op1"))
            return self.__eval_unary_op(kind, operand_value)

        # handle binary ops
        elif kind in self.ops:
            l, r = self.__eval_expr(expr.get("op1")), self.__eval_expr(expr.get("op2"))
            return self.__eval_binary_op(kind, l, r)
        
        elif kind == 'convert':
            return self.__run_convert(expr.get('to_type'), expr.get('expr'))
        
        elif kind == "@":
            return BrewinObject()

    def __resolve_dotted_name(self, dotted_name):
        path = dotted_name.split('.')
        base_name = path[0]

        base_info = self.env.get_var_info(base_name)
        if base_info is None:
            super().error(ErrorType.NAME_ERROR, f"Variable '{base_name}' not defined")

        current_obj = self.env.get_value(base_name)

        if len(path) == 1:
            return current_obj
        
        # if the base is a reference, get the object it points to
        if isinstance(current_obj, BrewinReference):
            current_obj = current_obj.get_value()

        if current_obj is self.NIL_VALUE:
            super().error(ErrorType.FAULT_ERROR, f"Cannot dereference nil object '{base_name}'")

        if not isinstance(current_obj, BrewinObject):
             super().error(ErrorType.TYPE_ERROR, f"Base variable '{base_name}' is not an object.")

        for i in range(1, len(path)):
            segment = path[i]

            if current_obj is self.NIL_VALUE: # previous segment non-nil
                super().error(ErrorType.FAULT_ERROR, f"Cannot derefernece nil object '{path[i-1]}'")

            if i < len(path) - 1: # not last segment so must be object
                if self.get_var_type_from_name(segment) != 'object':
                    super().error(ErrorType.TYPE_ERROR, f"Intermediate field '{segment}' must be object typed")

            # does field exist - segment missing is a name error
            if not isinstance(current_obj, BrewinObject) or segment not in current_obj.fields:
                super().error(ErrorType.NAME_ERROR, f"Field '{segment} not found in object")

            current_obj = current_obj.fields[segment]

        return current_obj
    
    def __eval_binary_op(self, op, l_val, r_val):

        def check_types(type_name, type_class):
            if type(l_val) != type_class or type(r_val) != type_class:
                self.error(ErrorType.TYPE_ERROR, f"Operands for {op} must be two {type_name}s")
        
        # equality
        if op == "==":
            if l_val is self.NIL_VALUE and r_val is self.NIL_VALUE:
                return True
            if type(l_val) != type(r_val): # type check with helper
                return False
            return l_val == r_val
        
        if op == "!=":
            return not self.__eval_binary_op("==", l_val, r_val) # opposite of ==

        if op == "+" and type(l_val) == str:
            if type(r_val) != str:
                super().error(ErrorType.TYPE_ERROR, "Operands for '+' must be two integers or two strings")
            return l_val + r_val

        # integer arithmetic (+, -, *, /)
        if op in ("+", "-", "*", "/"):
            check_types("integer", int)
            if op == "+": 
                return l_val + r_val
            if op == "-": 
                return l_val - r_val
            if op == "*": 
                return l_val * r_val
            if op == "/": # floor division
                return l_val // r_val

        # integer comparison (<, <=, >, >=)
        if op in ("<", "<=", ">", ">="):
            # these are illegal for different types
            check_types("integer", int) 
            if op == "<": 
                return l_val < r_val
            if op == "<=": 
                return l_val <= r_val
            if op == ">": 
                return l_val > r_val
            if op == ">=": 
                return l_val >= r_val

        # boolean logical (&&, ||)
        if op in ("&&", "||"):
            check_types("boolean", bool)
            if op == "&&": 
                return l_val and r_val
            if op == "||": 
                return l_val or r_val

        # if we get here, the types were incompatible for the operator
        super().error(ErrorType.TYPE_ERROR, f"Incompatible types ({type(l_val)}, {type(r_val)}) for operator '{op}'")
            
    def __eval_unary_op(self, op, op_val):

        if op == 'neg':
            if type(op_val) != int:
                super().error(ErrorType.TYPE_ERROR, "Operand for arithmetic negation must be integer")
            return -op_val
        
        elif op == '!': 
            if type(op_val) != bool:
                super().error(ErrorType.TYPE_ERROR, "Operand dor logical NOT '!' must be boolean")
            return not op_val
        
        super().error(ErrorType.TYPE_ERROR, f"{op}")

class BrewinObject:
    def __init__(self):
        self.fields = {}

class BrewinReference:
    def __init__(self, env, varname, parent_obj=None, field_name=None):
        self.env = env
        self.varname = varname
        self.parent_obj = parent_obj
        self.field_name = field_name

    def get_value(self):
        # value from reference location
        if self.parent_obj is None: # variable
            return self.env.get_value(self.varname)
        else: # object
            if self.field_name in self.parent_obj.fields:
                return self.parent_obj.fields[self.field_name]
            return self.env.NIL_VALUE

    def set_value(self, value):
        if self.parent_obj is None:
            target_val = self.env.get_value(self.varname)
            if isinstance(target_val, BrewinReference):
                target_val.set_value(value)
            else:
                if not self.env.set_value(self.varname, value):
                    raise ErrorType.NAME_ERROR(f"Internal error: Cannot set value for reference variable {self.varname}")
        else:
            self.parent_obj.fields[self.field_name] = value

def main():
    interpreter = Interpreter()

    # The code below is meant to help you test your interpreter on your own Brewin programs.
    # To run this main function, create a file test.br in the same directory and put Brewin code in it.
    with open("./test.br", "r") as f:
        program = f.read()

    global generate_image
    generate_image = True
    interpreter.run(program)


if __name__ == "__main__":
    main()