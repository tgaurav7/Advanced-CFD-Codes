*
*        file srct.f
******************************************************************
*
      SUBROUTINE SRCT(QT,RT,T,VOLP,ARO,TOLD,DE,DCCE,
     C                HCONV,TINF,RHO,DTIME,CP,  
     C                IB,IE,ID)

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
      REAL QT(ID),RT(ID),ARO(ID),VOLP(ID),DE(ID),DCCE(ID)
      REAL T(ID),TOLD(ID)
      REAL OMEG,HCONV,TINF,RHO,DTIME,CP
      INTEGER IE,IB,ID,I
      OMEG=1
*
      DO 1 I=IB,IE
      QT(I) = VOLP(I)*RHO*TOLD(I)/DTIME+HCONV*ARO(I)*TINF/CP
      QT(I) = QT(I) - DCCE(I) + DCCE (I-1)
      
      
      RT(I)=0

  1   CONTINUE

      RETURN
      END
