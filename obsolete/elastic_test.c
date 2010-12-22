#include "libplayerc/playerc.h"
#include <stdio.h>

void compute_springs (playerc_position2d_t *in_pos,
		      playerc_position2d_t *out_pos,
                      int                   num)
{
  int i;

  printf ("Received %d robot data\n", num);
  for (i = 0; i < num; i++) {
    out_pos [i] = in_pos [i];

    printf ("Robot %d has pose (%5.2f %5.2f %5.2f) and vel (%5.2f %5.2f %5.2f)\n",
	    i + 1,
	    in_pos [i].px, in_pos [i].py, in_pos [i].pa,
	    in_pos [i].vx, in_pos [i].vy, in_pos [i].va);

    /* Move in circles */
    out_pos [i].vx = 0.3;
    out_pos [i].va = 0.4;
  }
}
