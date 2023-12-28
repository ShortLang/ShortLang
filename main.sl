arr = [1, 2, 3, 4, 5]
print_each(arr)

print_each arr: {
	len = len(arr)
	i = 0

	>.i < len {
		$("item " + i + ": " + arr[i])
		i++
	}
}
