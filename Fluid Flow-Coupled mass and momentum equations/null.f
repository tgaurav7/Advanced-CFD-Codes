*
*        file null.f
************************************************************************
*
      SUBROUTINE NULL(A, IB,IE,ID)
*
*     Subroutine to null a vector.
*
*     REAL    A(ID)  Vector
*     INTEGER IB,IE  Starting and ending indices in I
*     INTEGER ID     Array dimension
*
*
************************************************************************
*
      REAL A(ID)
      INTEGER IB,IE,ID,I
*
      DO 1 I=IB,IE
        A(I)= 0.0
  1   CONTINUE
*
      RETURN
      END
*
************************************************************************
*
      SUBROUTINE NULLVB(A, ISTRT,IFIN,N,ID)
*
*     Subroutine to null a vector block.
*
*     REAL    A(N,ID)  matrix
*     INTEGER IB,IE  Starting and ending indices in I
*     INTEGER ID     Array dimension
*
*
************************************************************************
*
      REAL A(N,ID)
      INTEGER ISTRT,IFIN,ID,I,J
*
      DO 1 I=ISTRT,IFIN
	    DO 2 J=1,N
          A(J,I)= 0.0
  2     CONTINUE
  1   CONTINUE
*
      RETURN
      END
*
************************************************************************
      SUBROUTINE NULLMB(A, ISTRT,IFIN,N,ID)
*
*     Subroutine to null a matrix block.
*
*     A(N,N,ID)  matrix
*     ISTRT,IFIN  beginning and ending indices in I direction
*     ID,JD       array dimensions
*
************************************************************************
*
      REAL A(N,N,ID)
      INTEGER ISTRT,IFIN,I,J,K,N,ID
*
      DO 1 I=ISTRT,IFIN
        DO 2 J=1,N
          DO 3 K=1,N
            A(J,K,I)= 0.0
  3       CONTINUE
  2     CONTINUE
  1   CONTINUE
*
      RETURN
      END
*
************************************************************************
*
      SUBROUTINE NULLM(A, ISTRT,IFIN,JSTRT,JFIN,ID,JD)
*
*     Subroutine to null a matrix.
*
*     REAL    A(ID,JD)  matrix
*     INTEGER IB,IE  Starting and ending indices in I
*     INTEGER ID     Array dimension
*
*
************************************************************************
*
      REAL A(ID,JD)
      INTEGER ISTRT,IFIN,JSTRT,JFIN,ID,I,J
*
      DO 1 I=ISTRT,IFIN
	    DO 2 J=JSTRT,JFIN
          A(I,J)= 0.0
  2     CONTINUE
  1   CONTINUE
*
      RETURN
      END