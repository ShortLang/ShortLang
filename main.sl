impl int {
	// auto define the variable "self" to the value of the object
	// idk how you'll implement it but you got the point @wizard
	field = 5
	addone: self + 1
}
// $5.method()
// $5.addone
mc [1, 2, 3] {
	h::t: {
		$h // 1
		$t // [2, 3]
	}
}

// 5 * -5