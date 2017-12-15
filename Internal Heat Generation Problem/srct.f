*
*        file srct.f
******************************************************************
*
      SUBROUTINE SRCT(QT,RT, T,VOLP,ARO,HCONV,TINFC,IB,IE,ID)
*
*     Subroutine to calculate the net source of T in each interior
*     control volume/ unit volume.
*     Net Source = QT + RT*T (energy source/unit volume/cp )
*
*     REAL QT(ID) fixed source coefficient; output
*     REAL RT(ID) linearized source coefficient; output
*
*     INTEGER IB,IE first and last interior indices in i; input
*     INTEGER ID,JD array dimensions; input
*
*     NOTE: For solution with radiation, temperature must be
*           absolute!!!
*
******************************************************************
*
      REAL QT(ID),RT(ID),ARO(ID),VOLP(ID),HCONV,TINFC
      INTEGER IE,IB,ID,I
*
      QT(IB-1)=0
      RT(IB-1)=0
*
      QT(IE+1)=0
      RT(IE+1)=0 
*
      DO 1 I=IB,IE
      QT(I)=50000*VOLP(I)
      RT(I)=0

  1   CONTINUE

      RETURN
      END
