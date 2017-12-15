*                  file bndcu.f
********************************************************************
*
      SUBROUTINE BNDCU(AUP,AUW,AUE,BU,INLETU,IB,IE,ID)
*
*     Subroutine to put the boundary condition information for U
*     at each boundary node into equation coefficients.
*
*     AUP(2,2,ID)   active coefficient for P node; output
*     AUW(2,2,ID)   active coefficient for W node; output
*     AUE(2,2,ID)   active coefficient for E node; output
*     BU(2,ID)    accumulated fixed source term; output
*
*     INTEGER IB,IE  first and last interior indices in i; input
*     INTEGER ID     array dimensions; input
*
*
********************************************************************
*
      REAL AUP(2,2,ID),AUW(2,2,ID),AUE(2,2,ID),BU(2,ID)
      REAL INLETU
      INTEGER IB,IE
*
*     FOR IB-1 NODE:

      AUP(2,1,IB-1)=0.0
      AUP(2,2,IB-1)=1.0
      
      AUE(2,1,IB-1)=0.0
      AUE(2,2,IB-1)=0.0
      
      AUW(2,1,IB-1)=0.0
      AUW(2,2,IB-1)=0.0
      
      BU(2,IB-1)= INLETU
      
*	  FOR IE+1 NODE:
      
      AUP(2,1,IE+1)=0.0
      AUP(2,2,IE+1)=1.0
      
      AUE(2,1,IE+1)=0.0
      AUE(2,2,IE+1)=0.0
      
      AUW(2,1,IE+1)=0.0
      AUW(2,2,IE+1)=1.0
      
      BU(2,IE+1)=0.0
      
      
      RETURN
      END
