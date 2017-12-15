*            file uhat.f
*******************************************************************
*
      SUBROUTINE UHAT(UHE, U,P,DHUE,RHO,DPDX,DIEP,IB,IE,ID)
*     
*     Subroutine to calculate velocity to use in advecting term.
*
*     UHE(ID)    advecting velocity; output
*
*     U(ID)      current velocity at nodal points; input
*     P(ID)      current pressure at nodal points; input
*     DHUE(ID)   coefficients for UHE(ID); input
*     DIEP(ID)   x distance between nodes P and E; input
*     RHO        fluid density
*     DTIME      time step; input
*     DPDX(ID)   nodal values of pressure gradient
*
*     IB,IE      first and last interior indices; input
*     ID         array size; input
*
*******************************************************************
*
      REAL UHE(ID),U(ID),P(ID),DHUE(ID),DPDX(ID),DIEP(ID)
      REAL RHO
      INTEGER IB,IE,I
        
          UHE(IB-1)=U(IB-1)
*
      DO 100 I=IB,IE-1
      
          UHE(I)=0.5*(U(I)+U(I+1))
     C          -DHUE(I)*((P(I+1)-P(I))/DIEP(I)-0.5*(DPDX(I)+DPDX(I+1)))
     
  100 CONTINUE 
  
          UHE(IE)=U(IE+1)
          
      RETURN
      END

