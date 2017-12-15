*
***********************************************************************
*
      SUBROUTINE RESID(RSD,AVRSD, PHI,AP,AW,AE,B,IB,IE,ID)
*
*     Subroutine to calculate the residual at each interior c.v and
*     the average of the absolute residuals over all interior c.v.
*
*     RSD(ID) residual array for each interior c.v; output
*     AVRSD average residual for all interior c.v.; output
*
*     PHI(ID) updated estimate of phi field; input
*     AP(ID) active coefficient for P node; input
*     AW(ID) active coefficient for W node; input
*     AE(ID) active coefficient for E node; input
*     B(ID) accumulated fixed source term; input
*     INTEGER IB,IE  first and last interior indices in i; input
*     INTEGER ID  array dimensions; input
*
***********************************************************************
*
      REAL RSD(ID),SUMRSD,AVRSD
      REAL PHI(ID),AP(ID),AW(ID),AE(ID),B(ID),TOLD(ID)
      INTEGER IB,IE,ID,I
*
      AVRSD = 0.0
      SUMRSD = 0.0
      DO 555, I=IB,IE

      RSD(I)=AP(I)*PHI(I)-AW(I)*PHI(I-1)-AE(I)*PHI(I+1)-B(I)
      SUMRSD=SUMRSD+ABS(RSD(I))

 555  CONTINUE

      AVRSD=SUMRSD/(IE-IB+1)

*
      RETURN
      END
