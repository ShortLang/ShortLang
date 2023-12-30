arr = [1, 2, 3, 4, 5]
$arr

arr += [6, 7]
$arr

arr.push([8, 9])
$arr

$("hello world".push("!"))

$"hello"[0]

chads = "fireplank,wizard,hamza"
$chads.split(",")

// slicing
$get_range("Hello, world!", 1, 5).join()

get_range value start end: {
	data = []
	i = start
	>. i < end {
		data += value[i]
		i++
	}

	data
}
