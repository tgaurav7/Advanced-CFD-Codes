*            file srcup.f
*
******************************************************************
      SUBROUTINE SRCUP(AUP,AUW,AUE,BU, DIEP,VOLP,IB,IE,ID)
*
*     Subroutine to add the pressure gradient source term to the 
*     momentum equation for each interior control volume:
*
*     AUP(2,2,ID) active coefficient for P node; output
*     AUW(2,2,ID) active coefficient for W node; output
*     AUE(2,2,ID) active coefficient for E node; output
*     BU(2,ID)  accumulated fixed source term; output
*
*     IB,IE     first and last interior indices in i; input
*     ID,JD     array dimensions; input
*
******************************************************************
*
      REAL AUP(2,2,ID),AUW(2,2,ID),AUE(2,2,ID),BU(2,ID)
      REAL DIEP(ID),VOLP(ID)
      INTEGER IB,IE,I
*
      DO 100 I=IB,IE
      
         AUP(2,1,I)= 0.5*(1/DIEP(I-1)-1/DIEP(I))*VOLP(I)
         AUW(2,1,I)= 0.5*(1/DIEP(I-1))*VOLP(I)
         AUE(2,1,I)=-0.5*(1/DIEP(I))*VOLP(I)
         
         
  100 CONTINUE      
      
      RETURN
      END

