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
