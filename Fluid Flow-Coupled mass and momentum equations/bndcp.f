*                  file bndcp.f
********************************************************************
*
      SUBROUTINE BNDCP(AUP,AUW,AUE,BU,P,DPDX,OUTLETP,DIEP,IB,IE,ID)
*
*     Subroutine to put the boundary condition information for P
*     at each boundary node into equation coefficients.
*
*     AUP(2,2,ID)   active coefficient for P node; output
*     AUW(2,2,ID)   active coefficient for W node; output
*     AUE(2,2,ID)   active coefficient for E node; output
*     BU(2,ID)    accumulated fixed source term; output
*
*     IB,IE     first and last interior indices in i; input
*     ID        array dimensions; input
*
*
********************************************************************
*
      REAL AUP(2,2,ID),AUW(2,2,ID),AUE(2,2,ID),BU(2,ID)
      REAL P(ID),DIEP(ID),DPDX(ID)
      REAL OUTLETP
      INTEGER IB,IE
      
*
*      FOR IB-1 NODE:

      AUP(1,1,IB-1)=1.0
      AUP(1,2,IB-1)=0.0
      
      AUE(1,1,IB-1)=1.0
      AUE(1,2,IB-1)=0.0
      
      AUW(1,1,IB-1)=0.0
      AUW(1,2,IB-1)=0.0
      
      BU(1,IB-1)   =-(P(IB+1)-P(IB))*DIEP(IB-1)/DIEP(IB)
      
*	  FOR IE+1 NODE:
      
      AUP(1,1,IE+1)=1.0
      AUP(1,2,IE+1)=0.0
      
      AUE(1,1,IE+1)=0.0
      AUE(1,2,IE+1)=0.0
      
      AUW(1,1,IE+1)=0.0
      AUW(1,2,IE+1)=0.0
      
      BU(1,IE+1)=OUTLETP
      
      
      RETURN
      END

