get_magic_number: {
	(55 + 14) * 1000 + 410
}

fn_that_returns x: {
	num = get_magic_number()
	num + x
}

res = fn_that_returns(10)
print("value is: " + res)

add a b: a + b

a = 55
b = 14
sum = add(a, b)

$f"{a} + {b} = {sum}"