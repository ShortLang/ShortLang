impl int {
	square: {
		self * self
	}

	cube: {
		self * self * self
	}

	modify: {
		// this will not work as of now
		self = 1000
	}
}

a = 3

$f"suare of a = $a.square()"
$f" cube of a = $a.cube()"

impl str {
	slice start end: {
		slice = ""

		>. start < end {
			slice += self[start]
			start ++
		}

		slice
	}
}

msg = "Hello, World!"
$msg.slice(0, 5)
