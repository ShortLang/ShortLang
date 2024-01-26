// Credits: https://github.com/lasermtv07/shortlang-brainfuck
prog = inp("Program: ").split()
ii = 0
tape = []
ev rng(300) {
    tape += 0
}
tp = 0
ptr = 0
ls = [] // loop stack
>. ptr < (len(prog)) {
    mc prog[ptr] {
        "]": {
            tape[tp] > 0 ? {
                ptr = ls[len(ls)-1]
            }
            ls--
        }
        ",": {
            tape[tp] = ord(inp())
        }
        ">": {
            tp++
        }
        "<": {
            tape[tp] >= 0 ? tp--
        }
        "+": {
            tape[tp] += 1
        }
        "-": {
            tape[tp] -= 1
        }
        ".": {
            $$chr(tape[tp])
        }
    }

    prog[ptr] == "[" ? {
        ls += ptr
    }

    tape[tp] > 255 ? {
        tape[tp] = 0
    }
    tape[tp] < 0 ? {
        tape[tp] = 255
    }
    tp == len(tape) ? {
        tape += 0
    }
    ptr++
}