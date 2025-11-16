from intbase import InterpreterBase, ErrorType
from brewparse import parse_program

generate_image = False

class Return_Exception(Exception):
    def __init__(self, value):
        self.value = value

class Environment:
    def __init__(self):
        self.env = {}

    # define new variable at function scope
    def fdef(self, var_type, varname):
        if self.exists(varname):
            return False
        self.env[varname] = { 'type': var_type, 'value': None }
        return True

    def exists(self, varname):
        return varname in self.env

    def get_var_info(self, varname):
        if varname in self.env:
            return self.env[varname]
        return None

    def get_value(self, varname, value):
        info = self.get_var_info(varname)
        if info:
            return info['value']
        return None
    
    def set_value(self, varname, value):
        if not self.exists(varname):
            return False
        self.env[varname]['value'] = value
        return True

class Interpreter(InterpreterBase):
    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)

        self.funcs = {}  # { (name, arity) : element }
        self.env = Environment()
        self.ops = { "-", "+", "*", "/", "==", "!=", "<", ">", "<=", ">=", "&&", "||" }
        self.NIL_VALUE = None

        self.type_suffixes = { 'i': 'int', 's' : 'string', 'b': 'bool', 'o' : 'object', 'v' : 'void' }

    # new type helpermethods to grab suffix, var_type from name, return type, 

    # def get_function(self, name, arity):
    #     func_key = (name, arity)
    #     if func_key in self.funcs:
    #         return self.funcs[func_key]
        
    #     super().error(ErrorType.NAME_ERROR, f"Function {name} with {arity} arguments not found")

    def get_var_type_from_name(self, name):
        # must be 2 chars since 1 is type suffix
        if type(name) != str or len(name) < 2:
            return None
        
        suffix = name[-1]
        if suffix == 'v': #void function return
            return None

        if suffix in self.type_suffixes:
            return self.type_suffixes[suffix]
        
        return None # invalid suffix
    
    def get_return_type_from_name(self, name):
        if name == 'main':
            return 'void'
        
        if type(name) != str or len(name) < 2:
            return None
        
        suffix = name[-1]
        return self.type_suffixes.get(suffix)

    def get_default_value(self, typename):
        if typename == 'int': return 0
        if typename == 'string' : return ""
        if typename == 'bool' : return False
        if typename == 'object' or typename == 'void' : return self.NIL_VALUE
 
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
        
        return 'object'
        
    # update get function, no more arity
    def get_function(self, name, arg_types):
        func_key = (name, tuple(arg_types))

        if func_key in self.funcs:
            return self.funcs[func_key]
        
        super().error(ErrorType.NAME_ERROR, f"Function {name} not found")

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
                super.error(ErrorType.NAME_ERROR, f"Duplicate function definition fir {name} with type signature {param_types}")
            self.funcs[func_key] = func

        if ("main", ()) not in self.funcs:
            super.error(ErrorType.NAME_ERROR, "main function not found (needed)")
                            
        try:
            self.__run_function("main", []) # using new method to run multiple function scopes + new functions
        except Return_Exception:
            pass
    
    def __run_function(self, name, actual_args):
        # function to create new scope for every function call
        actual_arg_types = []
        for arg in actual_args:
            arg_type = self.get_runtime_type(arg)
            actual_arg_types.append(arg_type)

        # passing in tuple of types now
        func_ast = self.get_function(name, actual_arg_types)

        return_type = self.get_return_type_from_name(name)

        caller_env = self.env
        self.env = Environment()
        formal_args = func_ast.get("args") #initialize args - pass by value

        for i, formal_arg in enumerate(formal_args):
            var_name = formal_arg.get("name")
            var_type = self.get_var_type_from_name(var_name)
            # fdef now stores var type and name
            self.env.fdef(var_type, var_name)
            self.env.set(var_name, actual_args[i])

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
                super().error(ErrorType.TYPE_ERROR, f"Function '{name}' returns type {returned_type}, expected {returned_type}")

            return_value = returned_value

        self.env = caller_env # restore caller scope

        return return_value

    def __execute_statement(self, statement):
        kind = statement.elem_type

        if kind == self.VAR_DEF_NODE:
            self.__run_vardef(statement)

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
        return_value = self.NIL_VALUE

        if expression:
            return_value = self.__eval_expr(expression)

        raise Return_Exception(return_value)
    
    def __run_if(self, statement):

        condition_expr = statement.get("condition")
        condition_value = self.__eval_expr(condition_expr)

        if not isinstance(condition_value, bool):
            super().error(ErrorType.TYPE_ERROR, "Statement conditiion must be a boolean")

        if condition_value:
            true_statements = statement.get("statements")
            for stmt in true_statements:
                self.__execute_statement(stmt)
        else:
            else_statements = statement.get("else_statements")
            if else_statements:
                for stmt in else_statements:
                    self.__execute_statement(stmt)
    
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
            
            for stmt in loop_statements:
                self.__execute_statement(stmt)
        
    def __run_vardef(self, statement):
        # if statement evaluates boolean
        name = statement.get("name")
        var_type = statement.get_var_type_from_name(name)

        if var_type is None:
            super().error(ErrorType.TYPE_ERROR, f"Variable name '{name}' must end with a valid type suffix (i, s, b, o)")

        if not self.env.fdef(var_type, name):
            super().error(ErrorType.NAME_ERROR, "variable already defined")

    def __run_assign(self, statement):
        name = statement.get("var")
        value = self.__eval_expr(statement.get("expression"))
        runtime_type = self.get_runtime_type(value)
        
        var_info = self.env.get_var_info(name)
        if var_info is None:
            super().error(ErrorType.NAME_ERROR, "variable not defined")

        declared_type = var_info['type']

        # type mismatch from decalred to assignment
        if declared_type != runtime_type:
            super().error(ErrorType.TYPE_ERROR, f"Cannnot assign value of type {runtime_type} to variable {name} of declared type {declared_type}")

        #assignment
        if not self.env.set_value(name, value): # if var_info successful, should not reach here
            super().error(ErrorType.NAME_ERROR, f'Variable {name} not defined')
    
####
    def __run_fcall(self, statement):
        fcall_name = statement.get("name")
        args = statement.get("args")

        actual_args = []
        for arg_expr in args:
            actual_args.append(self.__eval_expr(arg_expr))
        arity = len(actual_args)

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

        if fcall_name == "inputs":
            if arity > 1:
                super().error(ErrorType.NAME_ERROR, "Too many arguments for inputs")

            if arity == 1:
                if isinstance(actual_args[0], bool):
                    super().output(str(actual_args[0]).lower())
                else: 
                    super().output(str(actual_args[0]))

            return super().get_input()

        if fcall_name == "print":
            out = ""
            for arg in actual_args:
                if isinstance(arg, bool):
                    out += str(arg).lower() # "true" or "false"
                elif arg is self.NIL_VALUE:
                    out += "nil"
                else:
                    out += str(arg)
            super().output(out)
            return self.NIL_VALUE
        
        return self.__run_function(fcall_name, actual_args)

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
            value = self.env.get(var_name)
            # check if undefined
            if value is self.NIL_VALUE and not self.env.exists(var_name):
                super().error(ErrorType.NAME_ERROR, "Variable not defined")
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