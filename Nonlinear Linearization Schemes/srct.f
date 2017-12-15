*
*        file srct.f
******************************************************************
*
      SUBROUTINE SRCT(QT,RT, T,VOLP,ARO,HCONV,TINFC,EMIS,IB,IE,ID)
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
      REAL QT(ID),RT(ID),ARO(ID),T(ID)
      REAL EMIS,SBC,TINFC
      INTEGER IE,IB,ID,I
*
      PARAMETER(SBC=5.67E-08)
*
      QT(IB-1)=0
      RT(IB-1)=0
*
      QT(IE+1)=0
      RT(IE+1)=0 
*
      DO 1 I=IB,IE
      QT(I)=-EMIS*SBC*ARO(I)*(T(I)**4-TINFC**4)
      RT(I)=0

  1   CONTINUE

      RETURN
      END
