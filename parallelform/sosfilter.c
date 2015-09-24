
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "sosfilter.h"




void sosfilter (double *input, unsigned inputlen, double *num_coeff0, double *num_coeff1,double *num_coeff2, double *denom_coeff0, double *denom_coeff1, double *denom_coeff2, unsigned nsect, double *output)
{
 double y;
 int j, k;
 double RegX1[nsect], RegX2[nsect], RegY1[nsect], RegY2[nsect];

 for(j=0; j<nsect; j++)
  {
   RegX1[j] = 0.0;
   RegX2[j] = 0.0;
   RegY1[j] = 0.0;
   RegY2[j] = 0.0;
  }

 for(j=0; j<inputlen; j++)
  {
   y = sosform1(0, input[j], num_coeff0, num_coeff1, num_coeff2, denom_coeff0, denom_coeff1, denom_coeff2, RegX1, RegX2, RegY1, RegY2);
   for(k=1; k<nsect; k++)
  {
   y = sosform1(k, y, num_coeff0, num_coeff1, num_coeff2, denom_coeff0, denom_coeff1, denom_coeff2, RegX1, RegX2, RegY1, RegY2);
  }
   output[j] = y;
  }
}


double sosform1(int k, double x, double *num_coeff0, double *num_coeff1, double *num_coeff2, double *denom_coeff0, double *denom_coeff1, double *denom_coeff2, double *RegX1, double *RegX2, double *RegY1, double *RegY2)
{
 double y, point;

 point = x * num_coeff0[k] + num_coeff1[k] * RegX1[k] + num_coeff2[k] * RegX2[k];
 y = denom_coeff0[k] * point - denom_coeff1[k] * RegY1[k] - denom_coeff2[k] * RegY2[k];

 RegX2[k] = RegX1[k];
 RegX1[k] = x;
 RegY2[k] = RegY1[k];
 RegY1[k] = y;

 return(y);
}


