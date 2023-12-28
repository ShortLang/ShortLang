factorial x: {
	x < 2 ? 1 : x * factorial(x - 1)
}

$factorial(30)

// or simply use the built-in postfix operator

$30!