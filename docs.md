# Functions

- ### args() <br> 
	Returns the command-line arguments given to the program.
- ### ceil(A) <br> 
	Rounds up to the next integer.
- ### gcd(A, B) <br> 
	Returns the greatest common divisor of two values.
- ### abs(A) <br> 
	Returns the absolute value of a number.
- ### root(A, B) <br> 
	Calculates the nth root of a number.
- ### env [3 overloads]
	- ### env(A, B) <br> 
		Sets the value of the specified environment variable.
	- ### env(A) <br> 
		Gets the value of the specified environment variable.
	- ### env() <br> 
		Gets all the environment variables as an array of key-value pairs.
- ### $(A) <br> 
	Prints to stdout with a newline at the end.
- ### type(A) <br> 
	Returns the value type.
- ### lcm(A, B) <br> 
	Returns the least common multiple of two values.
- ### floor(A) <br> 
	Rounds down to the previous integer.
- ### len(A) <br> 
	Returns the length of an array or string.
- ### chr(A) <br> 
	Returns the ascii value associated with a character.
- ### str(A) <br> 
	Converts a value to a string.
- ### rng [2 overloads]
	- ### rng(A, B) <br> 
		Returns an array with values from a to b.
	- ### rng(A) <br> 
		Returns an array with values from 0 to n.
- ### prime(A) <br> 
	Returns the nth prime number.
- ### int(A) <br> 
	Tries to convert a value to an integer, returns an error if it fails.
- ### lprime(A) <br> 
	Finds the previous prime number.
- ### round [2 overloads]
	- ### round(A, B) <br> 
		Rounds to the specified precision.
	- ### round(A) <br> 
		Rounds to the nearest integer.
- ### ord(A) <br> 
	Returns the character associated with the ascii value.
- ### inp [2 overloads]
	- ### inp(A) <br> 
		Read a line from stdin, with a prompt.
	- ### inp() <br> 
		Read a line from stdin.
- ### sqrt(A) <br> 
	Calculates the square root of a number.
- ### open(A) <br> 
	Opens a file.
- ### flt(A) <br> 
	Tries to convert a value to a float, returns an error if it fails.
- ### isprime(A) <br> 
	Checks whether a number is prime.
- ### nprime(A) <br> 
	Finds the next prime number.
- ### run(A) <br> 
	Executes a shell command.
- ### exit [2 overloads]
	- ### exit(A) <br> 
		Terminates the program with the specified exit code.
	- ### exit() <br> 
		Terminates the program with exit code 0.
- ### rnd [3 overloads]
	- ### rnd(A, B) <br> 
		Returns a random number between a and b.
	- ### rnd() <br> 
		Returns a random number.
	- ### rnd(A) <br> 
		Returns a random number between 0 and x.
- ### fib(A) <br> 
	Returns the nth fibonacci number.
- ### $$(A) <br> 
	Prints to stdout without a newline at the end.


# Methods

## float [7 methods]

- ### cos() <br> 
	Computes the cosine.
- ### csc() <br> 
	Computes the cosecant.
- ### sin() <br> 
	Computes the sine.
- ### tan() <br> 
	Computes the tangent.
- ### sec() <br> 
	Computes the secant.
- ### type() <br> 
	Returns the value type.
- ### cot() <br> 
	Computes the cotangent.
## nil [1 methods]

- ### type() <br> 
	Returns the value type.
## str [5 methods]

- ### pop() <br> 
	Pops the last element and returns it.
- ### clear() <br> 
	Removes all the elements from the array or string.
- ### split(A) <br> 
	Returns an array containing the substrings of a string, separated by specified character.
- ### type() <br> 
	Returns the value type.
- ### push(A) <br> 
	Pushes a value to the end of the array or string.
## bool [1 methods]

- ### type() <br> 
	Returns the value type.
## int [7 methods]

- ### cos() <br> 
	Computes the cosine.
- ### cot() <br> 
	Computes the cotangent.
- ### tan() <br> 
	Computes the tangent.
- ### sec() <br> 
	Computes the secant.
- ### type() <br> 
	Returns the value type.
- ### sin() <br> 
	Computes the sine.
- ### csc() <br> 
	Computes the cosecant.
## array [11 methods]

- ### max() <br> 
	Finds the largest element in the array.
- ### sub() <br> 
	Subtracts all the elements from the first one.
- ### join(A) <br> 
	Flattens an array a single value string, placing a given separator between each element.
- ### sum() <br> 
	Calculates the sum of all the values inside an array.
- ### push(A) <br> 
	Pushes a value to the end of the array or string.
- ### sort() <br> 
	Sorts the array.
- ### mul() <br> 
	Calculates the product of all the values of the array.
- ### type() <br> 
	Returns the value type.
- ### clear() <br> 
	Removes all the elements from the array or string.
- ### min() <br> 
	Finds the smallest element in the array.
- ### pop() <br> 
	Pops the last element and returns it.
## file [4 methods]

- ### a(A) <br> 
	Appends the contents to the file.
- ### w(A) <br> 
	Writes the contents to the file.
- ### type() <br> 
	Returns the value type.
- ### r() <br> 
	Reads everything from the file.

