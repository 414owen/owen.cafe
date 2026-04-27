---
title: "Fast DFA execution"
date: 2026-04-06T13:55:25+01:00
---

What's the best executable encoding of a DFA?

<!--more-->

## Lexing

Lexing is the task of breaking a string up into lexemes.
Here's an example of lexing the C programming langauge:

```C
//      id  int     com    sta id   obr     int  cbr
//     |--| |-|      |      | |---|  |        |  |
   int main(int charc, char **charv) { return 0; }
// |-|     |    |---|  |--|  |     |   |----|  |
// int     opa   id     cha  sta   cpa  ret   sem
```

Here, I've labelled each lexeme. Here's the key:

* `int`: int
* `id`: identifier
* `com`: comma
* `char`: char
* `sta`: star / asterisk (*)
* `obr`: open brace (`{`)
* `cbr`: close brace (`}`)
* `int`: integer literal
* `opa`: open parenthesis (`(`)
* `cpa`: close parenthesis (`)`)
* `ret`: return keyword
* `sem`: semicolon

## DFAs

A DFA is a [Deterministic Finite Automaton](https://en.wikipedia.org/wiki/Deterministic_finite_automaton).
Which is, in the case of our domain:
* A set of states
* A set of transitions between states
  * Each consuming a character
  * Each with a unique (originating state, character) tuple
* A start state
* A set of final states
  * Each annotated with a lexeme family

## Lexing with DFAs

Let's write a DFA which lexes a toy language.

This language has the following lexemes, defined as regex:

* def: `let`
* if: `if`
* else: `else`
* elif: `elif`
* identifier: `[a-z]+`
* integer: `0|[1-9][0-9]*`
* assign: `=`
* add: `+`

![State machine for token recognition](/img/keyword-recognition-lexemes.svg)

Here `[a-z--eil]` uses unicode regex character class [set subtraction](https://www.unicode.org/reports/tr18/#RL1.3).
It's equivalent to `[a-df-hj-km-z]`.

## Case study - recognizing numbers without leading or trailing zeros

The test problem will be to encode this regex:

```re
^0|-?[1-9][0-9]*|-?(0|[1-9][0-9]*)\.[0-9]*[1-9]$
```

as a function with this signature:

```C
bool is_number(char *input);
```

which will let us test whether a string is a valid number, without leading or trailing zeros.

## Baseline - Wing it

A lot of real programming languages use hand-written lexers.

For example:
* [Rust](https://github.com/rust-lang/rust/blob/345a975e76de74f090c55d5b56f5f6dd41b655a2/compiler/rustc_lexer/src/lib.rs#L5)
* [C](https://github.com/gco-mirror/gcc/blob/e9d43010d8ec98eed9a81ec4535ce446cc0e9c94/libcpp/lex.cc#L3888)
* [Zig](https://codeberg.org/ziglang/zig/src/branch/master/lib/std/zig/tokenizer.zig)
* [Odin](https://github.com/odin-lang/Odin/blob/7ca3b87bd876924bb1023120061d4491563e4ae3/core/odin/tokenizer/tokenizer.odin#L4)
* [D](https://github.com/dlang/dmd/blob/ac3d0cbd023e5c16216bc1a7c6b9a571dd2f0b84/compiler/src/dmd/lexer.d#L10)
* [Roq](https://github.com/roc-lang/roc/blob/6128db318d8ccc2046eb2a2bf27189f3d4889688/src/parse/tokenize.zig#L507)
* [C3](https://github.com/c3lang/c3c/blob/ef7c8790c57c2270a04d275c2fd1966d6b2c73ba/src/compiler/lexer.c#L15)

For our problem, I came up with this fairly boring lexer:

```C
#define one_to_nine \
       '1': case '2': case '3': case '4': \
  case '5': case '6': case '7': case '8': \
  case '9'

#define zero_to_nine '0': case one_to_nine

bool after_dot(char *input) {
  bool valid = false;
  do {
    switch (*input++) {
      case '0':         valid = false; continue;
      case one_to_nine: valid = true; continue;
      case '\0':        return valid;
      default:          return false;
    }
  } while (true);
}

bool after_nonzero_digit(char *input) {
  do {
    switch (*input++) {
      case zero_to_nine: continue;
      case '.':          return after_dot(input);
      case '\0':         return true;
      default:           return false;
    }
  } while (true);
}

bool is_number_manual(char *input) {
  switch (*input++) {
    case '-':
      switch (*input++) {
        case '0':
          switch (*input++) {
            case '.': return after_dot(input);
            default:  return false;
          }
        case one_to_nine: return after_nonzero_digit(input);
        default:          return false;
      }
    case '0':
      switch (*input++) {
        case '\0': return true;
        case '.':  return after_dot(input);
        default:   return false;
      }
    case one_to_nine: return after_nonzero_digit(input);
    default:          return false;
  }
}
```

It's okay. I don't hate it. There's an amount of code reuse, albeit with annoying names, like
`after_nonzero_digit`.

There's a lot of nesting, a little bit of
[defunctionalized](https://blog.sigplan.org/2019/12/30/defunctionalization-everybody-does-it-nobody-talks-about-it/)
state (`after_dot::valid`)...

## The DFA

You can compile the above regex to a minimal DFA with your favourite DFA-backed regex engine.
Rust's `regex-cli` is a good choice.

```text
State 0 (start)
 [-]    -> State 1 
 [0]    -> State 5 
 [1..9] -> State 6 

State 1
 [0]    -> State 2 
 [1..9] -> State 6 

State 2
 [.]    -> State 3 

State 3
 [0]    -> State 3 
 [1..9] -> State 4 

State 4 (accept[0])
 [0]    -> State 3 
 [1..9] -> State 4 

State 5 (accept[0])
 [.]    -> State 3 

State 6 (accept[0])
 [.]    -> State 3 
 [0..9] -> State 6 
```

## Method one - loop switch switch

This is one of the most obvious ways of reifying the above DFA as code.
It's fairly heavy on branching, but light on memory.

```C
#define one_to_nine \
       '1': case '2': case '3': case '4': \
  case '5': case '6': case '7': case '8': \
  case '9'

#define zero_to_nine '0': case one_to_nine

bool is_number(char *input) {
  uint8_t state = 0;
  while (true) {
    switch (state) {
      case 0:
        switch (*input++) {
          case '-':          state = 1; continue;
          case '0':          state = 5; continue;
          case one_to_nine:  state = 6; continue;
          case '\0':         return false;
          default:           return false;
        }
      case 1:
        switch (*input++) {
          case '0':          state = 2; continue;
          case one_to_nine:  state = 6; continue;
          case '\0':         return false;
          default:           return false;
        }
      case 2:
        switch (*input++) {
          case '.':          state = 6; continue;
          case '\0':         return false;
          default:           return false;
        }
      case 3:
        switch (*input++) {
          case '0':          state = 3; continue;
          case one_to_nine:  state = 4; continue;
          case '\0':         return false;
          default:           return false;
        }
      case 4:
        switch (*input++) {
          case '0':          state = 3; continue;
          case one_to_nine:  state = 4; continue;
          case '\0':         return true;
          default:           return false;
        }
      case 5:
        switch (*input++) {
          case '.':          state = 3; continue;
          case '\0':         return true;
          default:           return false;
        }
      case 6:
        switch (*input++) {
          case '.':          state = 3; continue;
          case zero_to_nine: state = 6; continue;
          case '\0':         return true;
          default:           return false;
        }
    }
  }
}
```

## Method two -- table-based

We can encode the same DFA as a 2d array, where the rows represent states, and the
columns represent transitions from that state.

The transitions are indexed by the current byte, and the cells contain the target states.

This table uses some funky C you might not have seen before, including a GNU extension.
You can check out the GCC section on [designated initializers](https://gcc.gnu.org/onlinedocs/gcc/Designated-Inits.html#Designated-Initializers) to get up to speed.

```C
// State 0: Start
// State 7: Failure
// State 8: Success

uint8_t transition_table[7][256] = {
// '\0'                           '.'    0  1  2  3  4  5  6  7  8  9
//----------------------------------------------------
  {  7, [1 ... 255] = 7, ['-'] = 1, 7, 7, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6 },
  {  7, [1 ... 255] = 7, ['-'] = 7, 7, 7, 2, 6, 6, 6, 6, 6, 6, 6, 6, 6 },
  {  7, [1 ... 255] = 7, ['-'] = 7, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7 },
  {  7, [1 ... 255] = 7, ['-'] = 7, 7, 7, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4 },
  {  8, [1 ... 255] = 7, ['-'] = 7, 7, 7, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4 },
  {  8, [1 ... 255] = 7, ['-'] = 7, 3, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7 },
  {  8, [1 ... 255] = 7, ['-'] = 7, 3, 7, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6 }
};

bool is_number_table(unsigned char *input) {
  uint8_t state = 0;
  do {
    state = transition_table[state][*input++];
  } while (state < 7);
  return state == 8;
}
```

This method is very *light* on branching, but the tables are heavy, 256 bytes per state.

While this function is specific to matching our specific regex, I could have made the function
take the table and number of states, making it generic over the DFA in question.
It's not incredibly useful, since the function is so short anyway, but it's kind of neat.

## Method three -- table-based with equivalence classes

Okay, the table above is quite heavy, at 256b per state.
What if we could get that down somewhat?

Well, if we look at the DFA, we can see that there are bytes which are indistinguishable
from the perspective of the transition table.
For example, the bytes '1' and '2' transition to the same target state from every source state.
Similarly, all alphabetic characters `[a-zA-Z]` always lead to the DFA *not* matching.

What would happen if we used this concept of 'globally indistinguishable characters' to compress the transition table?

Well, we'd end up with something a little like this:

```C
uint8_t equivalence_classes[256] = {
  ['\0'] = 1,
  ['-'] = 2,
  ['.'] = 3,
  ['0'] = 4,
  ['1'] = 5, ['2'] = 5,  ['3'] = 5,  ['4'] = 5,  ['5'] = 5,  ['6'] = 5,  ['7'] = 5,  ['8'] = 5,  ['9'] = 5, 
};

uint8_t transition_table_eq[7][6] = {
//   '\0'  '-'  '.'  '0' '1-9'
//----------------------------
  {7,  7,   1,   7,   5,   6},
  {7,  7,   7,   7,   2,   6},
  {7,  7,   7,   3,   7,   7},
  {7,  7,   7,   7,   3,   4},
  {7,  8,   7,   7,   3,   4},
  {7,  8,   7,   3,   7,   7},
  {7,  8,   7,   3,   6,   6},
};

bool is_number_table_equiv(char *input) {
  uint8_t state = 0;
  while (state < 7) {
    state = transition_table_eq[state][equivalence_classes[*input++]];
  }
  return state == 8;
}
```

Neat, we've brought the per-state table size down from 256b, to 6b.

## Method 4 - direct interpretation

Our final way is going to use two forbidden constructs -- labels and gotos.

```C
bool is_number_direct(char *input) {
state_0:
  switch (*input++) {
    case '-':         goto state_1;
    case '0':         goto state_5;
    case one_to_nine: goto state_6;
    case '\0':        return false;
    default:          return false;
  }
state_1:
  switch (*input++) {
    case '0':         goto state_2;
    case one_to_nine: goto state_6;
    case '\0':        return false;
    default:          return false;
  }
state_2:
  switch (*input++) {
    case '.':         goto state_6;
    case '\0':        return false;
    default:          return false;
  }
state_3:
  switch (*input++) {
    case '0':         goto state_3;
    case one_to_nine: goto state_4;
    case '\0':        return false;
    default:          return false;
  }
state_4:
  switch (*input++) {
    case '0':         goto state_3;
    case one_to_nine: goto state_4;
    case '\0':        return true;
    default:          return false;
  }
state_5:
  switch (*input++) {
    case '.':         goto state_3;
    case '\0':        return true;
    default:          return false;
  }
state_6:
  switch (*input++) {
    case '.':          goto state_3;
    case zero_to_nine: goto state_6;
    case '\0':         return true;
    default:           return false;
  }
}
```

Do you see what we did? The state of the DFA is encoded implicitly in the
program counter (PC) of our CPU:

### Let the compiler do it

GCC actually turns the first DFA-encoding method (loop-switch-switch) into the direct-threaded
interpreter, automatically.

[Here's a compiler explorer link](https://godbolt.org/z/3Gcc4GxKE), showing GCC producing
the same assembly structure for both direct and indirect threaded code.

Clang/LLVM supports the same optimization, but it's not enabled by default. You can try it out
with the `-mllvm --enable-dfa-jump-thread` clang options.

<!--
TODO:

* utf8
* hook for all methods, not one
* intro
* add % less table
* Add method 4 to title
* explain direct htreading
* say tables can handle any dfa
* mention "interpreters" ealier
* benchmarks
* end and link to post 2 
-->
