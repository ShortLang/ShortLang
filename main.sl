recurse x: {
	$("number: " + x)
	recurse(x + 1)
}

recurse(0)