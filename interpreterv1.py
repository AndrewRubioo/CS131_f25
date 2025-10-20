from brewparse import parse_program
from intbase import InterpreterBase,ErrorType

# the structure was largely inspired by the pseudocode in the proejct 1 spec
class Interpreter(InterpreterBase):
	def __init__(self, console_output=True, inp=None, trace_output=False):
		super().__init__(console_output, inp) # call InterpreterBase's constructor
		self.functions = {}

	def run(self, program):
		ast = parse_program(program) # parse program into AST
		func_def_nodes = ast.dict['functions'] # dict to hold variables
		
		for func_def in func_def_nodes:
			# function object from node
			func_obj = Function(func_def)
			self.functions[func_obj.name] = func_obj

        # look for a function "main" in self.functions
		if 'main' not in self.functions:
			super().error(ErrorType.NAME_ERROR, "No main() function was found")

		main_func = self.functions['main']
		self.run_function(main_func)

	def run_function(self, func_node):
		# Empty scope for every function run
		self.variables = {}
		for statement_node in func_node.statements:
			self.run_statement(statement_node)

	def run_statement(self, statement_node):

		stmt_type = statement_node.elem_type

		if stmt_type == 'vardef':
			self.do_vardef(statement_node)
		elif stmt_type == '=':
			self.do_assignment(statement_node)
		elif stmt_type == 'fcall':
			self.fcall_statement(statement_node)

	def do_vardef(self, vardef_node):
		#var definition to be stored in dict
		var_name = vardef_node.dict['name']
		
		#previously defined?
		if var_name in self.variables:
			super().error(ErrorType.NAME_ERROR, f"Variable {var_name} defined more than once")

		self.variables[var_name] = None

	def do_assignment(self, assign_node):
		# handle assignment like x = 6 + 7, store in dict
		target_var_name = assign_node.dict['var']

		if target_var_name not in self.variables:
			super().error(ErrorType.NAME_ERROR, f"Variable {target_var_name} has not been defined")

		expression_node = assign_node.dict['expression'] 
		result = self.evaluate_expression(expression_node)
		self.variables[target_var_name] = result
	
	def do_print(self, arg_nodes):
		# handle zero or more args
		string_to_output = ""
		for arg in arg_nodes:
			stringy = self.evaluate_expression(arg)
			string_to_output += str(stringy)

		super().output(string_to_output)

	def fcall_statement(self, fcall_node):
		# function calls will perform action
		func_name = fcall_node.dict['name']
		arg_nodes = fcall_node.dict['args']

		if func_name == 'print':
			self.do_print(arg_nodes)

		else:
			super().error(ErrorType.NAME_ERROR, f"Function {func_name} has not been defined")

	def fcall_expression(self, fcall_node):
		# function calls will return a value
		func_name = fcall_node.dict['name']
		arg_nodes = fcall_node.dict['args']

		if func_name == 'inputi':
			if len(arg_nodes) > 1:
				super().error(ErrorType.NAME_ERROR), "No inputi() function found that takes > 1 parameter"

			if len(arg_nodes) == 1:
				prompt = self.evaluate_expression(arg_nodes[0])
				super().output(prompt)

			user_input = super().get_input()
			return int(user_input)
		else:
			super().error(ErrorType.NAME_ERROR, f"Function {func_name} is not valid in an expression")
		
	def evaluate_expression(self, expression_node):
		#literal values
		node_type = expression_node.elem_type

		if node_type == 'int':
			return expression_node.dict['val']

		if node_type == 'string':
			return expression_node.dict['val']

		#variable names
		if node_type == 'qname':
			var_name = expression_node.dict['name']
			if var_name not in self.variables:
				super().error(ErrorType.NAME_ERROR, f"Variable {var_name} has not been defined")
			return self.variables[var_name]

		# binop expressions
		if node_type == '+' or node_type == '-':
			left_val =self.evaluate_expression(expression_node.dict['op1'])
			right_val = self.evaluate_expression(expression_node.dict['op2'])

			#type check step
			if not isinstance(left_val, int) or not isinstance(left_val, int):
				super().error(ErrorType.TYPE_ERROR, "Incompatible types for arithmetic operation")
			
			if node_type == '+':
				return left_val + right_val
			
			else:
				return left_val - right_val

		if node_type == 'fcall':
				return self.fcall_expression(expression_node)

class Function:
    #Class to represent functions in Brewin
	def __init__(self, func_node):
		self.name = func_node.dict['name']
		self.args = func_node.dict['args'] #for future
		self.statements = func_node.dict['statements']