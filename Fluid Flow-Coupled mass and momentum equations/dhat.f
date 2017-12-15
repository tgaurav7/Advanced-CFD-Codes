*             file dhat.f
*     
************************************************************************
*
      SUBROUTINE DHAT(DHUE, AUP,VOLP,RHO,IB,IE,ID)
*
*     Subroutine to calculate the diffusion coefficients for  
*     the special momentum equation required to compute UHAT.
*
*     DHUE(ID)  diffusion coefficient for east face; output
*
*     AUP(ID)   active coefficient on node P; input
*     IB,IE     first and last interior indices in i; input
*     ID        array dimensions; input     
*
***********************************************************************
*
      REAL DHUE(ID),AUP(2,2,ID),VOLP(ID),RHO
      INTEGER IB,IE,I
*
      DO 100 I=IB,IE-1
      
          DHUE(I)=0.5*(VOLP(I)+VOLP(I+1))/
     C            (0.5*(AUP(2,2,I)+AUP(2,2,I+1))+1E-20)
     
  100 CONTINUE  
      RETURN
      END

