*
*       file solup.f
********************************************************************
*
      SUBROUTINE SOLUP(AUP,AUW,AUE,BU,P,U,IB,IE,ID)
*
*     Subroutine to solve the coupled mass and momentum
*     equations using LUD decomposition.
*
*
*     REAL AUP(2,2,ID)  active coefficient block for P node; input
*     REAL AUW(2,2,ID)  active coefficient block for W node; input
*     REAL AUE(2,2,ID)  active coefficient block for E node; input
*     REAL BU(2,ID)     accumulated fixed source term vector; input
*
*     GLOBM(M,M)        global coefficient matrix
*     RHS(M)            right hand side vector
*     SOL(M)            solution vector
*
*     INTEGER IB,IE  first and last interior indices in i; input
*     INTEGER ID     array dimensions; input
*
*********************************************************************
*
      PARAMETER(M=240)
      REAL AUP(2,2,ID),AUW(2,2,ID),AUE(2,2,ID),BU(2,ID),P(ID),U(ID)
      REAL GLOBM(M,M),RHS(M),SOLN(M),WK4(M)
      INTEGER WK5(M)
      INTEGER IB,IE,ID,K
*
*--Assemble global coefficient matrix using coefficient blocks
*
      CALL GLOBMS(GLOBM,RHS,
     C            AUP,AUW,AUE,BU,
     C            IB-1,IE+1,2,ID,M,E)
*
*--Compute solution for U P set using a direct solver 
*
      CALL LUD(GLOBM,RHS, SOLN,WK4,WK5,E,M)
*
*--Insert the new solution into the U() and P() arrays
*
      CALL ASSIGN(P,U, SOLN,IB,IE,ID,M)
*
      RETURN
      END
