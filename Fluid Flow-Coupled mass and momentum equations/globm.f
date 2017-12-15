*      file globm.f
*********************************************************************
      SUBROUTINE GLOBMS(GLOBM,RHS,
     C                  AUP,AUW,AUE,BU,
     C                  IB,IE,N,ID,M,E)
*
*     Subroutine to construct the coefficient array in the structure 
*     required by LUD_comp.  Used for simultaneous variable solutions.
*
*     GLOBM(M,M)   global array for LUD_comp; output
*     RHS(M)       right-hand-side vector; output
*     
*     IB,IE        starting and ending indices in 'x' direction
*     ID           array dimensions; input
*     N            number of simutaneous variables; input
*
*********************************************************************
*
      REAL GLOBM(M,M),RHS(M)
      REAL AUP(N,N,ID),AUW(N,N,ID),AUE(N,N,ID),BU(N,ID)
      INTEGER IB,IE,E,N,ID,M
      INTEGER SIZE,W,I,J,K,L
*
*  Initialize GLOBM(M,M)
*
      SIZE= (IE+1-IB+2)*N
      CALL NULLM(GLOBM,1,SIZE,1,SIZE,M,M)
*
*  Set non-zero matrix elements
*
      I=IB
      E=1
      DO 23 K=1,N
       DO 24 L=1,N
         GLOBM(E+K-1,E+L-1)= AUP(K,L,I)
         IF(IE.GT.IB) GLOBM(E+K-1,E+N+L-1)= -AUE(K,L,I)
  24   CONTINUE
       RHS(E+K-1)= BU(K,I)
  23  CONTINUE
      DO 25 I=IB+1,IE-1
        E=E+N
        DO 26 K=1,N
         DO 27 L=1,N
           GLOBM(E+K-1,E-N+L-1)= -AUW(K,L,I)
           GLOBM(E+K-1,E+L-1  )=  AUP(K,L,I)
           GLOBM(E+K-1,E+N+L-1)= -AUE(K,L,I)
  27     CONTINUE
         RHS(E+K-1)= BU(K,I)
  26    CONTINUE
  25  CONTINUE
      IF(IE.GT.IB) THEN
        I=IE
        E=E+N
        DO 28 K=1,N
         DO 29 L=1,N
           GLOBM(E+K-1,E-N+L-1)= -AUW(K,L,I)
           GLOBM(E+K-1,E+L-1  )=  AUP(K,L,I)
  29     CONTINUE
         RHS(E+K-1)= BU(K,I)
  28    CONTINUE
      ENDIF
*
      E=E+N-1
*
      RETURN
      END
