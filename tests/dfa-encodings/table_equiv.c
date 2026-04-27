#include <stdbool.h>
#include <stdint.h>

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

bool is_number_table_equiv(char *input) {
  uint8_t state = 0;
  while (state < 7) {
    state = transition_table_eq[state][equivalence_classes[*input++]];
  }
  return state == 8;
}

