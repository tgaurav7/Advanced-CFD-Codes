*     This file contains 2 subroutines: COEFCN and MASFLX
*
************************************************************************
*
      SUBROUTINE COEFCN(ACUW,ACUE,BC,AREP,RHO,IB,IE,ID)
*
*     Subroutine to calculate the coefficients of the continuity
*     equation for each control volume.
*
*     ACUW(ID)   coefficient for west face uhat; output
*     ACUE(ID)   coefficient for east face uhat; output
*     BC(ID)     mass source term; output
*
*     RHO        fluid density (kg/m^3); input
*     AREP(ID)   c.v. area of face at e point; input
*     IB,IE      first and last interior indices in i; input
*     ID         array dimension; input     
*
*     Notes: 1) The form of the continuity equation is:
*
*               ACUE*UHE(I)+ ACUW*UHE(I-1) + BC(I) = 0
*
***********************************************************************
*
      REAL    ACUE(ID),ACUW(ID),BC(ID)
      REAL    AREP(ID)
      REAL    RHO
      INTEGER IB,IE,ID,I
*
      ACUE(IB-1)=  AREP(IB-1)*RHO
      
      DO 10 I=IB,IE
         ACUE(I)=  AREP(I)*RHO
         ACUW(I)= -AREP(I-1)*RHO
         BC(I)  =  0.0
         
  10  CONTINUE
  
      ACUW(IE+1)= -AREP(IE)*RHO
  
  
  
      RETURN
      END
*
*
************************************************************************
*
      SUBROUTINE MASFLX(ME, UHE,ACUW,ACUE,BC,IB,IE,ID)
*
*     Subroutine to calculate the mass flow for the 
*     normal advection fluxes through the east face of
*     each control volume. 
*
*     REAL ME(ID) normal mass flux for east face; output
*
*     UHE(ID)   x component of mass velocity at e point; input
*     ACUW(ID)  coefficient for west face uhat; input
*     ACUE(ID)  coefficient for east face uhat; input
*     BC(ID)    mass source term; input
*
*     IB,IE     first and last interior indices in i; input
*     ID        array dimension; input     
*
*     Notes: 1) ME is calculated for the e points on all control-
*               volume faces.
*
*            2) This subroutine must be preceded by a call to COEFCN.
*
***********************************************************************
*
      REAL    ME(ID)
      REAL    UHE(ID),ACUE(ID),ACUW(ID),BC(ID)
      INTEGER IB,IE,ID,I
      
      DO 100 I=IB-1,IE
      
         ME(I)= ACUE(I)*UHE(I) 

  100 CONTINUE
      

      RETURN
      END
