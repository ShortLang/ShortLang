$$"Welcome to guess the number! Choose your difficulty (1-3): "
diff = inp()
mc diff {
    _1: {tries = 10}
    _2: {tries = 7}
    _3: {tries = 5}
    _: {$"Invalid difficulty. Please try again."; exit()}
}
$f"You have $tries to guess a number between 1 and 100."
answer = rnd(1, 100)
guesses = 0
ev rng(tries) {
    guesses++
    guess = int(inp("Guess a number between 1 and 100: "))
    guess == answer ? {
        $"That's right! You win!"
        exit()
    } : guess > answer ? $f"Too high! You have $tries-guesses guesses left." : $f"Too low! You have $tries-guesses guesses left."
}
$f"You lose! The number was {answer}."