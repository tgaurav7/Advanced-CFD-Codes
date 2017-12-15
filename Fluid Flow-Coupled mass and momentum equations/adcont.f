*
*             file adcont.f
*********************************************************************
*
      SUBROUTINE ADCONT(AP,AW,AE,B,ACUW,ACUE,BC,DHUE,DPDX,
     C                  RHO,DIEP,DISE,IB,IE,ID)
*
*     Subroutine to construct coefficients for all terms in the
*     conservation of mass equation.  The form of the mass equation
*     is:
*                   ACUE*Ue + ACUW*Uw + BC = 0
*
*     The coefficients are stored in the first line of the 
*     coefficient blocks for mass and momentum.  The form of the 
*     equation is:
*
*               [A]p*{}p = [A]w*{}w + [A]e*{}e + [b]p
*
*********************************************************************
*
      REAL AP(2,2,ID),AW(2,2,ID),AE(2,2,ID),B(2,ID)
      REAL ACUW(ID),ACUE(ID),BC(ID),DHUE(ID),DPDX(ID)
      REAL DIEP(ID),DISE(ID)
      REAL RHO
      INTEGER IB,IE,I
*
*DEFINING THE COEFFICIENTS FOR IB NODE          
          AP(1,1,IB)= DHUE(IB)/DIEP(IB)*ACUE(IB)
          AW(1,1,IB)= 0.0
          AE(1,1,IB)= DHUE(IB)/DIEP(IB)*ACUE(IB)
          
          AP(1,2,IB)= 0.5*ACUE(IB)
          AW(1,2,IB)=-ACUW(IB)
          AE(1,2,IB)=-0.5*ACUE(IB)
          B(1,IB)   =-ACUE(IB)*0.5*DHUE(IB)*(DPDX(IB)+DPDX(IB+1))
     C               -BC(IB)

*DEFINING THE COEFICIENTS FOR IB+1 TO IE-1 NODES:

      DO 100 I=IB+1,IE-1
      
          AP(1,1,I)= DHUE(I)/DIEP(I)*ACUE(I)-DHUE(I-1)/DIEP(I-1)*ACUW(I)
          AW(1,1,I)=-DHUE(I-1)/DIEP(I-1)*ACUW(I)
          AE(1,1,I)= DHUE(I)/DIEP(I)*ACUE(I)
          
          AP(1,2,I)= 0.5*ACUE(I)+0.5*ACUW(I)
          AW(1,2,I)=-0.5*ACUW(I)
          AE(1,2,I)=-0.5*ACUE(I)
          B(1,I)   =-ACUE(I)*(DHUE(I)*0.5*(DPDX(I)+DPDX(I+1)))
     C              -ACUW(I)*(DHUE(I-1)*0.5*(DPDX(I-1)+DPDX(I)))
     C              -BC(I)
     
 
     
  100 CONTINUE

*DEFINING THE COEFFICIENTS FOR IE NODE  
          AP(1,1,IE)=-DHUE(IE-1)/DIEP(IE-1)*ACUW(IE)
          AW(1,1,IE)=-DHUE(IE-1)/DIEP(IE-1)*ACUW(IE)
          AE(1,1,IE)= 0.0
  
          AP(1,2,IE)= 0.5*ACUW(IE)
          AE(1,2,IE)=-ACUE(IE)
          AW(1,2,IE)=-0.5*ACUW(IE)
          B(1,IE) =-ACUW(IE)*0.5*DHUE(IE-1)*(DPDX(IE)+DPDX(IE-1))-BC(IE)
     
  
  
      RETURN
      END
