//calculate square root using the babylonian algoritm
sqrt i : {
        precision = 25
        x = i/2
				x < 1 ? {
					x = 1
				}
        >. precision > 0 {
                x = 0.5 * ( x + i / x )
                precision --
        }
        &x
}

a = 1

>. a <= 1000 {
	$sqrt(a)
	a = a + 1
}
