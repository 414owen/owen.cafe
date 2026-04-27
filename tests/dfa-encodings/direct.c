#include <stdbool.h>
#include <stdint.h>

#define one_to_nine \
       '1': case '2': case '3': case '4': \
  case '5': case '6': case '7': case '8': \
  case '9'

#define zero_to_nine '0': case one_to_nine

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
