/* Utilities for binding to RT-WMP */

#include "core/interface/Msg.h"
#include "core/interface/wmp_interface.h"
#include <stddef.h>
#include <string.h>

size_t sancta_rtwmp_max_data_size (void) {
  return 1000;
  return (size_t) wmpGetMTU();
}

void sancta_rtwmp_receive (char * buffer, size_t * last) {
  /* Presumes buffer'first = 0 */

  Msg m;

  wmpPop (&m);
  memcpy (buffer, m.data, m.len);
  *last = m.len - 1;
}

void sancta_rtwmp_send (char * buffer, size_t last, char src, char dst, int * ok)
{
  Msg m = {
    .len      = last + 1,
    .port     = '\0',
    .priority = (char) 64,
    .src      = (unsigned int) src,
    .dest     = (unsigned int) dst
  };

  memcpy (m.data, buffer, m.len);

  *ok = wmpPush (&m);
}
