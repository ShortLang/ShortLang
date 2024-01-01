// constants
π=3.14159265358979323846
e=2.71828182845904523536

// functions
istype value type: type(value) == type
print val: $val
gcd a b: b == 0 ? a : gcd(b, a % b)
fib n:n<2?n:fib(n-1)+fib(n-2)