#include <stdio.h>
#include <stdlib.h>

// This function returns a char with value > 127, which is invalid UTF-8
// when written as a single byte. This simulates what happens when a
// pointer is truncated to a char on systems where the compiler allows it.
char get_high_byte(void) {
  // Return a high byte value (0x80 = 128) that's invalid as UTF-8
  return (char)0x80;
}
