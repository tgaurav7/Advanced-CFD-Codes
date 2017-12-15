*            file srcu.f
*
******************************************************************
      SUBROUTINE SRCU(QU,RU, U,UHE,DCCE,RHO,VISC,XE,ARO,AREP,IB,IE,ID)
*
*     Subroutine to calculate the net source of U in each interior
*     control volume:
*     Net Source = QU + RU*U 
*
*     QU(ID) fixed source coefficient; output
*     RU(ID) linearized source coefficient; output
*
*     IB,IE  first and last interior indices in i; input
*     ID,JD  array dimensions; input
*
*     DH     hydraulic diameter, Dh= 4A/P; computed
*     CF     drag coefficient; computed
*
******************************************************************
      REAL QU(ID),RU(ID),U(ID),UHE(ID),DCCE(ID)
      REAL XE(ID),ARO(ID),AREP(ID)
      REAL RHO,VISC
      REAL CF,PO,RE
      INTEGER IB,IE,I
      
*
      DO 100 I=IB,IE
        PO   = ARO(I)/(XE(I)-XE(I-1))
        RE   = 4*RHO*AREP(I)*U(I)/(PO*VISC)
        CF   = 1/(1.58*LOG(RE)-3.28)**2.0
        CF   = 0.0
        QU(I)= 0.5*CF*RHO*U(I)*U(I)*ARO(I)
        QU(I)= QU(I)- DCCE(I) + DCCE (I-1)
        RU(I)= -CF*RHO*U(I)*ARO(I)
      
  100 CONTINUE

      RETURN
      END

