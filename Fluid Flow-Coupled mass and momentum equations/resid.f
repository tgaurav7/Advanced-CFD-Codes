*
***********************************************************************
*
      SUBROUTINE RESID(AVRSD,RSD, PHI,AP,AW,AE,B,IB,IE,ID)
*
*     Subroutine to calculate the residual at each interior c.v and
*     the average of the absolute residuals over all interior c.v.s
*
*     AVRSD   average residual for all interior c.v.; output
*     RSD(ID) residual array for each interior c.v; output
*
*     PHI(ID) updated estimate of phi field; input
*     AP(ID)  active coefficient for P node; input
*     AW(ID)  active coefficient for W node; input
*     AE(ID)  active coefficient for E node; input
*     B(ID)   accumulated fixed source term; input
*
*     IB,IE   first and last interior indices in i; input
*     ID      array dimensions; input
*
***********************************************************************
*
      REAL RSD(ID),SUMRSD,AVRSD
      REAL PHI(ID),AP(ID),AW(ID),AE(ID),B(ID)
      INTEGER IB,IE
*
      SUMRSD= 0.0
      DO 1 I=IB,IE
         RSD(I)= AW(I)*PHI(I-1)+AE(I)*PHI(I+1)-AP(I)*PHI(I)+B(I)
         SUMRSD= SUMRSD + ABS(RSD(I))
  1   CONTINUE
*
      AVRSD= SUMRSD/(IE+1-IB)
*
      RETURN
      END
*
*
***********************************************************************
*
      SUBROUTINE RESIDM(AVRSD,RSD, P,U,IJ,AUP,AUW,AUE,BU,IB,IE,ID)
*
*     Subroutine to calculate the residual at each interior c.v and
*     the average of the absolute residuals over all interior c.v.s
*
*     Note: even though the equations are solved as a coupled set
*           the residuals are checked for each equation separately.
*           For example, to check the residual in the U momentum
*           equation, call RESIDM with IJ=2.
*
*     AVRSD      average residual for all interior c.v.; output
*     RSD(ID)    residual array for each interior c.v; output
*
*     P(ID)      updated estimate of pressure field; input
*     U(ID)      updated estimate of U velocity field; input
*     AP(2,2,ID) active coefficient block for P node; input
*     AW(2,2,ID) active coefficient block for W node; input
*     AE(2,2,ID) active coefficient block for E node; input
*     B(2,ID)    accumulated fixed source term vector; input
*
*     IJ         indicates row to check in coefficient blocks; input
*     IB,IE      first and last interior indices in i; input
*     ID         array dimensions; input
*
***********************************************************************
*
      REAL RSD(ID),SUMRSD,AVRSD,P(ID),U(ID)
      REAL AUP(2,2,ID),AUW(2,2,ID),AUE(2,2,ID),BU(2,ID)
      INTEGER IB,IE,I,IJ
*
      SUMRSD= 0.0
      DO 1 I=IB,IE
         RSD(I)= AUW(IJ,1,I)*P(I-1)+AUW(IJ,2,I)*U(I-1)
     C          +AUE(IJ,1,I)*P(I+1)+AUE(IJ,2,I)*U(I+1)
     C          -AUP(IJ,1,I)*P(I)  -AUP(IJ,2,I)*U(I)
     C          +BU(IJ,I)
         SUMRSD= SUMRSD + ABS(RSD(I))
  1   CONTINUE
*
      AVRSD= SUMRSD/(IE+1-IB)
*
      RETURN
      END
