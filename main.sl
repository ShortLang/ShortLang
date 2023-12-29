arr = [1, 2, 3, 4]
arr.push(10)

println(arr)

push_garbage(arr)

println(arr)

push_garbage arr: {
	arr.push([1, 2, 3, 4, 5, 6])
}
