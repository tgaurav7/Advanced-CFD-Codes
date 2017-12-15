*             file gradp.f
*     
************************************************************************
*
      SUBROUTINE GRADP(DPDX, P,DIEP,IB,IE,ID)
*
*     Subroutine to calculate the pressure gradient at each nodal
*     location using the most current pressure field.
*
*     DPDX(ID)  derivative of pressure at nodal location; output
*
*     P(ID)     current pressure field; input
*     DIEP(IE)  distance from P to E nodes; input
*     IB,IE     first and last interior indices in i; input
*     ID        array dimensions; input     
*
***********************************************************************
*
      REAL DPDX(ID),P(ID),DIEP(ID)
      INTEGER IE,IB,I
      
*
      DO 100 I=IB,IE
      
        DPDX(I) = 0.5*(1/DIEP(I-1)-1/DIEP(I))*P(I)+
     C            0.5/DIEP(I)*P(I+1)-0.5/DIEP(I-1)*P(I-1)
      
  100 CONTINUE
  
      RETURN
      END

