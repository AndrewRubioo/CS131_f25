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
    def fdef(self, varname):
        if self.exists(varname):
            return False
        self.env[varname] = None
        return True

    def exists(self, varname):
        return varname in self.env

    def get(self, varname):
        if varname in self.env:
            return self.env[varname]
        return None

    def set(self, varname, value):
        if not self.exists(varname):
            return False
        self.env[varname] = value
        return True

class Interpreter(InterpreterBase):
    def __init__(self, console_output=True, inp=None, trace_output=False):
        super().__init__(console_output, inp)

        self.funcs = {}  # { (name, arity) : element }
        self.env = Environment()
        self.ops = {"-", "+", "*", "/"}
        self.NIL_VALUE = None

    def __get_function(self, name, arity):
        func_key = (name, arity)
        if func_key in self.funcs:
            return self.funcs[func_key]
        
        super().error(ErrorType.NAME_ERROR, f"Function {name} with {arity} arguments not found")

    def run(self, program):
        ast = parse_program(program, generate_image)

        for func in ast.get("functions"):
            name = func.get("name")
            arity = len(func.get("args"))

            if (name, arity) in self.funcs:
                pass #undefined behavoir for functions with same name/arity

            self.funcs[(name, arity)] = func

        try:
            self.__run_function("main", []) # using new method to run multiple function scopes + new functions
        except Return_Exception:
            pass

    def __run_function(self, name, actual_args):
        #look up function def
        func_ast = self.__get_function(name, len(actual_args))

        caller_env = self.env
        self.env = Environment()
        formal_args = func_ast.get("args") #initialize args - pass by value

        for i, formal_arg in enumerate(formal_args):
            var_name = formal_arg.get("name")
            self.env.fdef(var_name)
            self.env.set(var_name, actual_args[i])

        return_value = self.NIL_VALUE #default

        try:
            for statement in func_ast.get("statements"):
                self.__execute_statement(statement)
        
        except Return_Exception as e:
            return_value = e.value

        self.env = caller_env # restore caller scope

        return return_value

    def __execute_statement(self, statement):
        kind = statement.elem_type

        if kind == self.VAR_DEF_NODE:
            self.__run_vardef(statement)

        elif kind == "=":
            self.__run_assign(statement)

        elif kind == self.FCALL_NODE:
            self.__run_assign(statement)

        elif kind == self.RETURN_NODE:
            self.__run_assign(statement)

        elif kind == self.IF_NODE:
            self.__run_if(statement)

        elif kind == self.WHILE_NODE:
            self.__run_while(statement)


    def __run_return(self, statement):

        expression = statement.get("expression")
        return_value = self.NIL_VALUE

        if expression:
            return_value = self.__eval_expression(expression)

        raise Return_Exception(return_value)
    
    def __run_if(self, statement):

        condition_expr = statement.get("condition")
        condition_value = self.__eval_expr(condition_expr)

        if not isinstance(condition_expr, bool):
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
            conditon_value = self.__eval_expr(condition_expr)

            if not isinstance(condition_expr, bool):
                super().error(ErrorType.TYPE_ERROR, "While statement condition must be a boolean")

            if not conditon_value:
                break
            
            for stmt in loop_statements:
                self.__execute_statement(stmt)
        
    def __run_vardef(self, statement):
        # if statement evaluates boolean
        name = statement.get("name")

        if not self.env.fdef(name):
            super().error(ErrorType.NAME_ERROR, "variable already defined")

    def __run_assign(self, statement):
        name = statement.get("var")

        value = self.__eval_expr(statement.get("expression"))
        if not self.env.set(name, value):
            super().error(ErrorType.NAME_ERROR, "variable not defined")



#MAJOR REFACTOR below
    def __run_fcall(self, statement):
        fcall_name, args = statement.get("name"), statement.get("args")

        if fcall_name == "inputi":
            if len(args) > 1:
                super().error(ErrorType.NAME_ERROR, "too many arguments for inputi")

            if args:
                super().output(str(self.__eval_expr(args[0])))

            return int(super().get_input())

        if fcall_name == "print":
            out = ""

            for arg in args:
                out += str(self.__eval_expr(arg))

            super().output(out)

            return 0  # undefined behavior

        super().error(ErrorType.NAME_ERROR, "unknown function")

    def __eval_expr(self, expr):
        kind = expr.elem_type

        if kind == self.INT_NODE or kind == self.STRING_NODE:
            return expr.get("val")

        elif kind == self.QUALIFIED_NAME_NODE:
            var_name = expr.get("name")

            value = self.env.get(var_name)
            if value is None:
                super().error(ErrorType.NAME_ERROR, "variable not defined")

            return value

        elif kind == self.FCALL_NODE:
            return self.__run_fcall(expr)

        elif kind in self.ops:
            l, r = self.__eval_expr(expr.get("op1")), self.__eval_expr(expr.get("op2"))

            if isinstance(l, str) or isinstance(r, str):
                super().error(
                    ErrorType.TYPE_ERROR, "invalid operand types for arithmetic"
                )

            if kind == "-":
                return l - r

            elif kind == "+":
                return l + r
        
    def __eval_unary_op(self, op, op_val):

        if op == 'neg':
            if not isinstance(op_val, int):
                super().error(ErrorType.TYPE_ERROR, "Operand for arithmetic negation must be integer")
            return not op_val
        
        elif op == '!': 
            if not isinstance(op_val, bool):
                super().error(ErrorType.TYPE_ERROR, "operand dor logical NOT '!' must be boolean")
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