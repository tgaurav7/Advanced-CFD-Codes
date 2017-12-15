*           file coeff.f
*
***********************************************************************
      SUBROUTINE COEFF(AP,AW,AE,B,
     C            ME,DE,Q,R,PHI,VOLP,ALFAE,RHO,DTIME,
     C            IB,IE,ID)
*     
*     Subroutine to calculate the transport coefficients for the
*     variable PHI on a co-located non-orthogonal grid.
*
*     REAL AP(ID) active coefficient for P node; output
*     REAL ASUM sum of active neighbour coefficients; output
*     REAL AW(ID) active coefficient for W node; output
*     REAL AE(ID) active coefficient for E node; output
*     REAL B(ID)  accumulated fixed source term; output
*
*     REAL ME(ID) mass flux for the east face;  input
*     REAL DE(ID) diffusion coefficient for east face; input
*     REAL Q(ID) fixed source term; input
*     REAL R(ID) linearized source term; input
*     REAL PHI(ID) estimate of phi at old time; input
*     REAL VOLP(ID) c.v. volume; input
*     REAL ALFAE(ID) advection weight factor for east face; input
*     REAL RHO  fluid density; input
*     REAL DTIME  time step for implicit scheme; input
*     INTEGER IB,IE first and last interior indices in i; input
*     INTEGER ID array dimensions; input     
*
*     Notes: 1) This subroutine should be proceeded by calls to
*                 NULL(B, ...
*                 DIFPHI(DE, ...
*                 MASFLX(ME,..
*                 WEIGHT(ALFAE,...
*                 SRC(Q,R, ...
*
**********************************************************************
*
      REAL AP(ID),ASUM,AW(ID),AE(ID),B(ID)
      REAL ME(ID),DE(ID),Q(ID),R(ID)
      REAL PHI(ID),VOLP(ID),ALFAE(ID)
      REAL RHO,DTIME
      INTEGER IB,IE,ID,I
*
      DO 1 I=IB,IE
      
         AW(I)= DE(I-1)+0.5*(ME(I-1)+ABS(ME(I-1))*ALFAE(I-1))
         AE(I)= DE(I)-0.5*(ME(I)-ABS(ME(I))*ALFAE(I))
         ASUM= AE(I) + AW(I)
         AP(I)= RHO*VOLP(I)/DTIME + ASUM - R(I)
         B(I)= B(I) + RHO*VOLP(I)*PHI(I)/DTIME + Q(I)
         
  1   CONTINUE
*
      RETURN
      END
*
***********************************************************************
      SUBROUTINE COEFFM(AP,AW,AE,B,
     C            ME,DE,Q,R,PHI,VOLP,ALFAE,
     C            RHO,DTIME,UOLD,
     C            IB,IE,ID)
*
*     Subroutine to calculate the transport coefficients for the
*     variable PHI on a co-located non-orthogonal grid.  In this 
*     case the coefficients are inserted directly into coefficient
*     matrix blocks.
*
*     REAL AP(2,2,ID) active coefficient for P node; output
*     REAL ASUM sum of active neighbour coefficients; output
*     REAL AW(2,2,ID) active coefficient for W node; output
*     REAL AE(2,2,ID) active coefficient for E node; output
*     REAL B(2,ID)  accumulated fixed source term; output
*
*     REAL ME(ID) mass flux for the east face;  input
*     REAL DE(ID) diffusion coefficient for east face; input
*     REAL Q(ID) fixed source term; input
*     REAL R(ID) linearized source term; input
*     REAL PHI(ID) estimate of phi at old time; input
*     REAL VOLP(ID) c.v. volume; input
*     REAL ALFAE(ID) advection weight factor for east face; input
*     REAL RHO  fluid density; input
*     REAL DTIME  time step for implicit scheme; input
*     INTEGER IB,IE first and last interior indices in i; input
*     INTEGER ID array dimensions; input     
*
*     Notes: 1) This subroutine should be proceeded by calls to
*                 NULL(B, ...
*                 DIFPHI(DE, ...
*                 MASFLX(ME,..
*                 WEIGHT(ALFAE,...
*                 SRC(Q,R, ...
*
**********************************************************************
*
      REAL AP(2,2,ID),AW(2,2,ID),AE(2,2,ID),B(2,ID)
      REAL ME(ID),DE(ID),Q(ID),R(ID),PHI(ID),VOLP(ID),ALFAE(ID)
      REAL UOLD(ID)
      REAL ASUM,RHO,DTIME
      INTEGER IB,IE,I
* 
      DO 100 I=IB,IE
      
          AW(2,2,I)= DE(I-1)+0.5*(ME(I-1)+ALFAE(I-1)*ME(I-1))
          AE(2,2,I)= DE(I)-0.5*(ME(I)-ALFAE(I)*ME(I))
          ASUM     = AW(2,2,I)+AE(2,2,I)
          AP(2,2,I)= ASUM+RHO*VOLP(I)/DTIME-R(I)
          B(2,I)   = RHO*VOLP(I)*UOLD(I)/DTIME+Q(I)
          
  100 CONTINUE
  
      RETURN
      END
