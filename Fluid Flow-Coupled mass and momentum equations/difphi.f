*
*        file difphi.f
************************************************************************
*
      SUBROUTINE DIFPHI(DE, GAMA,AREP,DIEP,IB,IE,ID)
*
*     Subroutine to calculate the diffusion coefficients for  
*     normal diffusion fluxes through the east face of
*     each control volume.
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
      REAL DE(ID),DIEP(ID),AREP(ID),GAMA
      INTEGER IB,IE,ID,I

*
      DO 1 I=IB-1,IE
         DE(I)= GAMA*AREP(I)/DIEP(I)

  1   CONTINUE
*
      RETURN
      END
