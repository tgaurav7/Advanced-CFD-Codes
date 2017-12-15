C234567
      SUBROUTINE LUD(A,C, X,S,O,N,ID)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Definition of variables:
c
c        A(,)- matrix of coefficients
c        C() - right-hand-side vector
c        X() - unknowns
c        O() - order vector
c        S() - scale vector
c
c        N   - number of equations
c        ID  - array dimensions
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C234567
      REAL A(ID,ID),S(ID),C(ID),X(ID)
      INTEGER O(ID)
      CALL ORDER(S,O,A,N,ID)
      CALL DECOMP(A,O,S,N,ID)
      CALL SOLVE(A,C,X,O,N,ID)
      RETURN
      END
c
c  Subroutines for LUD
c
      SUBROUTINE ORDER(S,O,A,N,ID)
      REAL A(ID,ID), S(ID)
      INTEGER O(ID)
      INTEGER I,J
      DO 10 I=1, N
         O(I)=I
         S(I)=ABS(A(I,1))
         DO 20 J=2, N
            IF(ABS(A(I,J)).GT.S(I)) THEN
              S(I)=ABS(A(I,J))
            END IF
  20     CONTINUE
  10  CONTINUE
      RETURN
      END
c
      SUBROUTINE DECOMP(A,O,S,N,ID)
      REAL A(ID,ID),S(ID)
      REAL SUM
      INTEGER O(ID),N,ID
      INTEGER I,J,K
      J=1
      CALL PIVOT(S,A,O,N,J,ID)
      DO 10 J=2,N
         A(O(1),J) = A(O(1),J)/A(O(1),1)
  10  CONTINUE
      DO 20 J=2,N-1
         DO 30 I=J,N
            SUM = 0.
            DO 40 K=1,J-1
               SUM = SUM+A(O(I),K)*A(O(K),J)
  40        CONTINUE
            A(O(I),J) = A(O(I),J) - SUM
  30     CONTINUE
         CALL PIVOT(S,A,O,N,J,ID)
         DO 50 K=J+1,N
            SUM = 0.
            DO 60 I=1,J-1
               SUM = SUM + A(O(J),I)*A(O(I),K)
  60        CONTINUE
            A(O(J),K) = (A(O(J),K)-SUM)/A(O(J),J)
  50     CONTINUE
  20  CONTINUE
      SUM =0.
      DO 70 K=1,N-1
         SUM = SUM + A(O(N),K)*A(O(K),N)
  70  CONTINUE
      A(O(N),N) = A(O(N),N) - SUM
      RETURN
      END
c
      SUBROUTINE PIVOT(S,A,O,N,K,ID)
      REAL A(ID,ID),S(ID)
      REAL BIG,DUMMY
      INTEGER O(ID)
      INTEGER K,PIVIT,II,IDUM
      PIVIT= K
      BIG=ABS(A(O(K),K)/S(O(K)))
      DO 10 II=K+1, N
         DUMMY= ABS(A(O(II),K)/S(O(II)))
         IF(DUMMY.GT.BIG) THEN
            BIG= DUMMY
            PIVIT= II
         END IF
  10  CONTINUE
      IDUM= O(PIVIT)
      O(PIVIT)= O(K)
      O(K)= IDUM
      RETURN
      END
c
      SUBROUTINE SOLVE(A,C,X,O,N,ID)
      REAL A(ID,ID),C(ID),X(ID) 
      REAL SUM
      INTEGER O(ID)
      INTEGER I,J
      X(1) = C(O(1))/A(O(1),1)
      DO 10 I=2,N
         SUM = 0.
         DO 20 J=1,I-1
            SUM = SUM + A(O(I),J)*X(J)
  20     CONTINUE
         X(I) = (C(O(I))-SUM)/A(O(I),I)
  10  CONTINUE
      DO 30 I=N-1,1,-1
         SUM = 0.
         DO 40 J=I+1,N
            SUM = SUM + A(O(I),J)*X(J)
  40     CONTINUE
         X(I) = X(I) - SUM
  30  CONTINUE
      RETURN
      END
