#include <stdbool.h>
#include <stdint.h>

#define one_to_nine \
       '1': case '2': case '3': case '4': \
  case '5': case '6': case '7': case '8': \
  case '9'

#define zero_to_nine '0': case one_to_nine

bool is_number_switchy(char *input) {
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
