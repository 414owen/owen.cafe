#include <stdbool.h>
#include <stdint.h>

#pragma GCC diagnostic ignored "-Woverride-init"

// Table size: 1792b

// State 0: Start
// State 7: Failure
// State 8: Success

static uint8_t transition_table[7][256] = {
// '\0'                            '.'    0  1  2  3  4  5  6  7  8  9
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

