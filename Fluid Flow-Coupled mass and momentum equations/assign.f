*
********************************************************************
      SUBROUTINE ASSIGN(P,U, SOLN,IB,IE,ID,M)
*
*     Subroutine to insert the solved values of P U from SOLN into
*     the P() and U() arrays.
*     
*     SOLN(M)   solution vector of the direct solution
*
********************************************************************
*
      REAL P(ID),U(ID)
      REAL SOLN(M)
      INTEGER IB,IE,ID,M
      INTEGER I,E
*
      E=1
      DO 1 I=IB-1,IE+1
        P(I)= SOLN(E  )
        U(I)= SOLN(E+1)
        E=E+2
  1   CONTINUE
*
      RETURN
      END

