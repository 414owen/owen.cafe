#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <stdint.h>

// Switchy
// -------

#define one_to_nine \
       '1': case '2': case '3': case '4': \
  case '5': case '6': case '7': case '8': \
  case '9'

#define zero_to_nine '0': case one_to_nine

static bool is_number_switchy(char *input) {
  uint8_t state = 0;
  while (true) {
    switch (state) {
      case 0:
        switch (*input++) {
          case '-':         state = 1; continue;
          case '0':         state = 5; continue;
          case one_to_nine: state = 6; continue;
          case '\0':        return false;
          default:          return false;
        }
      case 1:
        switch (*input++) {
          case '0':         state = 2; continue;
          case one_to_nine: state = 6; continue;
          case '\0':        return false;
          default:          return false;
        }
      case 2:
        switch (*input++) {
          case '.':         state = 6; continue;
          case '\0':        return false;
          default:          return false;
        }
      case 3:
        switch (*input++) {
          case '0':         state = 3; continue;
          case one_to_nine: state = 4; continue;
          case '\0':        return false;
          default:          return false;
        }
      case 4:
        switch (*input++) {
          case '0':         state = 3; continue;
          case one_to_nine: state = 4; continue;
          case '\0':        return true;
          default:          return false;
        }
      case 5:
        switch (*input++) {
          case '.':         state = 3; continue;
          case '\0':        return true;
          default:          return false;
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

// Table-based
// -----------

// Table size: 1792b

// State 7 -> reject
// State 8 -> accept

static uint8_t transition_table[7][256] = {
// '\0'           '.'    0  1  2  3  4  5  6  7  8  9
//----------------------------------------------------
  {     ['-'] = 1, 0, 0, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6 },
  {     ['-'] = 0, 0, 0, 2, 6, 6, 6, 6, 6, 6, 6, 6, 6 },
  {     ['-'] = 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
  {     ['-'] = 0, 0, 0, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4 },
  {  7, ['-'] = 0, 0, 0, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4 },
  {  7, ['-'] = 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
  {  7, ['-'] = 0, 3, 0, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6 }
};

// State 0: Failure
// State 1: Start
// State 8: Success
static bool is_number_table(char *input) {
  uint8_t state = 0;
  do {
    state = transition_table[state][*input++];
  } while (state > 0 && state < 7);
  return state > 0;
}

// Table-based with equivalence classes
// ------------------------------------

// Table size: 298b

static uint8_t equivalence_classes[256] = {
  ['\0'] = 1,
  ['-'] = 2,
  ['.'] = 3,
  ['0'] = 4, 5, 5, 5, 5, 5, 5, 5, 5, 5
};

static uint8_t transition_table_eq[7][6] = {
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

static bool is_number_table_equiv(char *input) {
  uint8_t state = 0;
  while (state < 7) {
    state = transition_table_eq[state][equivalence_classes[*input++]];
  }
  return state == 8;
}

// Direct-style
// ------------

#define one_to_nine \
       '1': case '2': case '3': case '4': \
  case '5': case '6': case '7': case '8': \
  case '9'

#define zero_to_nine '0': case one_to_nine

static bool is_number_direct(char *input) {
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

static void run_tests(bool (*test)(char*)) {
    // Valid integers
    assert(test("0") == true);
    assert(test("9") == true);
    assert(test("123") == true);
    assert(test("-1") == true);
    assert(test("-999") == true);

    // Valid decimals
    assert(test("0.1") == true);
    assert(test("9.1") == true);
    assert(test("-0.5") == true);
    assert(test("123.456") == true);

    // Invalid: leading zeros
    assert(test("01") == false);
    assert(test("-01") == false);

    // Invalid: trailing zeros in decimal
    assert(test("1.0") == false);
    assert(test("1.10") == false);

    // Invalid formats
    assert(test("") == false);
    assert(test("-") == false);
    assert(test(".1") == false);
    assert(test("1.") == false);
    assert(test("abc") == false);
    assert(test("1a") == false);
    assert(test("--1") == false);
    assert(test("+-1") == false);

    // Leading zeros
    assert(test("00") == false);
    assert(test("01") == false);
    assert(test("09") == false);
    assert(test("004.12") == false);
    assert(test("-0") == false);

    // Trailing zeros
    assert(test("0.0") == false);
    assert(test("-1.50200") == false);
    assert(test("4.0") == false);
}

int main() {
    printf("Checking switchy implementation\n");
    run_tests(is_number_switchy);
    printf("Checking table implementation\n");
    run_tests(is_number_table);
    printf("Checking table implementation w/ equivalence classes\n");
    run_tests(is_number_table_equiv);
    printf("Checking direct interpretation\n");
    run_tests(is_number_direct);
    printf("All tests passed.\n");
    return 0;
}
