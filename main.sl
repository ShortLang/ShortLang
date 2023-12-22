f x = print("Number is: " + x)
f(10)

// known bug:
// ```
// f x = print("num is " + x)
// g x = print(x + " is num")
//
// r1 = f(10)
// r2 = g(20)
// print("r1: " + r1 + ", " + "r2: " + r2)
// ```
//
// The above code should print "r1: null, r2: null"
// but instead it prints "r1: 10, r2: 20"
