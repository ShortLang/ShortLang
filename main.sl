x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

$"       items in the array: " + (x.join(", "))
$"       items in the array: " + (x[:].join(", "))
$"       items in the array: " + (x[::].join(", "))
$"\n"
$"         items in reverse: " + (x[::-1].join(", "))
$"         items in reverse: " + (x[len(x):0].join(", "))
$"\n"
$" odd numbers in the array: " + (x[::2].join(", "))
$"even numbers in the array: " + (x[1::2].join(", "))
