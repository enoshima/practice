



void RunIIRBiquadForm1(double *Input, double *Output, int NumSigPts)
{
 double y;
 int j, k;

 for(j=0; j<REG_SIZE; j++) // Init the shift registers.
  {
   RegX1[j] = 0.0;
   RegX2[j] = 0.0;
   RegY1[j] = 0.0;
   RegY2[j] = 0.0;
  }

 for(j=0; j<NumSigPts; j++)
  {
   y = SectCalcForm1(0, Input[j]);
   for(k=1; k<NumSections; k++)
  {
   y = SectCalcForm1(k, y);
  }
   Output[j] = y;
  }
}



double SectCalcForm1(int k, double x)
{
 double y, CenterTap;

 CenterTap = x * b0[k] + b1[k] * RegX1[k] + b2[k] * RegX2[k];
 y = a0[k] * CenterTap - a1[k] * RegY1[k] - a2[k] * RegY2[k];

 RegX2[k] = RegX1[k];
 RegX1[k] = x;
 RegY2[k] = RegY1[k];
 RegY1[k] = y;

 return(y);
}


