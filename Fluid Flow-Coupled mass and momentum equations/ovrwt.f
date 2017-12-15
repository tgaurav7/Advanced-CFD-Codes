      SUBROUTINE OVRWT(T,XP,TINF,IB,IE,ID)
*
*     Subroutine to OVERWRITE THE INITIAL SOLUTION 
*
*     REAL DE(ID) diffusion coefficient for east face; output
*
*     REAL GAMA diffusivity * density (kg/s/m); input
*     REAL AREP(ID) c.v. area of face at e point; input
*     REAL DIEP(ID) grid distance in i at e point; input
*     INTEGER IB,IE first and last interior indices in i; input
*     INTEGER ID array dimensions; input     
*
*     Notes: 1) DE is calculated for the e points on
*               interior control volume faces.
*
***********************************************************************
*
      REAL T(ID),XP(ID)
      INTEGER IB,IE,ID,I

      DO 1 I=IB-1,IE+1
         T(I)=100
         T(I)=T(I)*1.1191
         T(I)=T(I)*EXP(0.8603*0.8603*(-0.4535))
         T(I)=T(I)*COS(0.8603*XP(I))

  1   CONTINUE
*
      RETURN
      END
