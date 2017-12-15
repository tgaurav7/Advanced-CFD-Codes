*
*        file tdma.f
******************************************************************
*
      SUBROUTINE TDMA(PHI, AP,AE,AW,B,I1,IN,ALFA,BETA,ID)
*
*     REAL PHI(ID)  Dependent variable, returned
*
*     REAL AP(ID)   Central coefficient of PHI, input
*     REAL AE(ID)   East coefficient of PHI, input
*     REAL AW(ID)   West coefficient of PHI,input
*     REAL B(ID)    Source coefficient of PHI, input
*     INTEGER I1    First I-index of PHI, input
*     INTEGER IN    Last I-value of PHI, input
*     REAL ALFA(ID) Work array
*     REAL BETA(ID) Work array
*     INTEGER ID    Dimension of arrays
*     
*     Note:   It is assumed that the PHI-equation is of the form:
*
*            AP(I)*PHI(I)=AW(I)*PHI(I-1)+AE(I)*PHI(I+1)+B(I)
*
*     A forward sweep finds the coefficients ALFA and BETA in the
*     equation:
*
*            ALFA(I)*PHI(I)=AE(I)*PHI(I+1)+BETA(I)
*
**********************************************************************
*
      REAL PHI(ID)
      REAL AP(ID),AE(ID),AW(ID),B(ID),ALFA(ID),BETA(ID)
      INTEGER I1,IN,ID,I
*
*--Define ALFA(I1) and BETA(I1) as starting point for TDMA
*
      ALFA(I1)= AP(I1)
      BETA(I1)= B(I1)
*
*--Forward sweep to calculate ALFA() and BETA()
*
      DO 1 I=I1+1,IN
         ALFA(I)= AP(I) - AW(I)*AE(I-1)/ALFA(I-1)
         BETA(I)= B(I) + AW(I)*BETA(I-1)/ALFA(I-1)
  1   CONTINUE
      PHI(IN)= BETA(IN)/ALFA(IN)
*
*--Backward sweep to calculate PHI()
*
      DO 2 I=IN-1,I1,-1
         PHI(I)= (AE(I)*PHI(I+1) + BETA(I))/ALFA(I)
  2   CONTINUE
*
      RETURN
      END
