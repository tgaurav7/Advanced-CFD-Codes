*
*        file coeff.f
***********************************************************************
*
      SUBROUTINE COEFF(AP,AW,AE,B,
     C                 DE,Q,R,VOLP,RHO,CP,
     C                 IB,IE,ID)
*
*     Subroutine to calculate the transport coefficients for the
*     variable PHI on a co-located non-orthogonal grid.
*
*     REAL AP(ID) active coefficient for P node; output
*     REAL ASUM(ID) sum of active neighbour coefficients; output
*     REAL AW(ID) active coefficient for W node; output
*     REAL AE(ID) active coefficient for E node; output
*     REAL B(ID)  accumulated fixed source term; output
*
*     REAL DE(ID) diffusion coefficient for east face; input
*     REAL Q(ID) fixed source term; input
*     REAL R(ID) linearized source term; input
*     REAL PHI(ID) estimate of phi at old time; input
*     REAL VOLP(ID) c.v. volume; input
*     REAL RHO  fluid density; input
*     REAL specific heat of fluid; input
*     REAL DTIME  time step for implicit scheme; input
*     INTEGER IB,IE first and last interior indices in i; input
*     INTEGER ID array dimensions; input     
*
**********************************************************************
* 
      REAL AP(ID),AE(ID),AW(ID),B(ID),DE(ID),CP,Q(ID)
      INTEGER IB,IE,ID,I
*
      DO 10 I=IB,IE
         AE(I)= DE(I)
         AW(I)= DE(I-1)
         B(I) = Q(I)
  10   CONTINUE
*
      DO 20 I=IB,IE
         AP(I)=AE(I)+AW(I)
  20   CONTINUE            
*
      RETURN
      END
