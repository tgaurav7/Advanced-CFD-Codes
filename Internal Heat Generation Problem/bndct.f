*
*        file bndct.f
***********************************************************************
*
      SUBROUTINE BNDCT(ATP,ATW,ATE,BT,
     C                 HCONV,DE,AREP,TINFW,TINFE,
     C                 IB,IE,ID)
*
*     Subroutine to put the boundary condition information for T
*     at each boundary node into equation coefficients.
*
*     REAL ATP(ID) active coefficient for P node; output
*     REAL ATW(ID) active coefficient for W node; output
*     REAL ATE(ID) active coefficient for E node; output
*     REAL BT(ID)  accumulated fixed source term; output
*
*     INTEGER IB,IE  first and last interior indices in i; input
*     INTEGER ID     array dimensions; input
*
***********************************************************************
*
      REAL ATP(ID),ATW(ID),ATE(ID),BT(ID),AREP(ID),DE(ID)
      REAL HCONV,TINFW,TINFE
      INTEGER ID,IE,IB
*
      ATP(IB-1)=DE(IB-1)+HCONV*AREP(IB-1)
      ATP(IE+1)=DE(IE)+HCONV*AREP(IE)

*
      BT(IB-1)=HCONV*AREP(IB-1)*TINFW
      BT(IE+1)=HCONV*AREP(IE)*TINFE

*
      ATE(IB-1)=DE(IB-1)
      ATW(IB-1)=0
*      
      ATE(IE+1)=0
      ATW(IE+1)=DE(IE)
*
      RETURN
      END
