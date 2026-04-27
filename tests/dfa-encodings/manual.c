#include <stdbool.h>
#include <stdint.h>

#define one_to_nine \
       '1': case '2': case '3': case '4': \
  case '5': case '6': case '7': case '8': \
  case '9'

#define zero_to_nine '0': case one_to_nine

// After '.'
static bool fractional(char *input) {
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

static bool after_nonzero_digit(char *input) {
  do {
    switch (*input++) {
      case zero_to_nine: continue;
      case '.':          return fractional(input);
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
            case '.': return fractional(input);
            default:  return false;
          }
        case one_to_nine: return after_nonzero_digit(input);
        default:          return false;
      }
    case '0':
      switch (*input++) {
        case '\0': return true;
        case '.':  return fractional(input);
        default:   return false;
      }
    case one_to_nine: return after_nonzero_digit(input);
    default:          return false;
  }
}
