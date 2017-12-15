*
*     file hoconv.f
**********************************************************************
*
      SUBROUTINE HOCONV(DCCE,XP,XE,ME,PHI,ALFAE,ADVSCM,IB,IE,ID)
*
*     Routine to calculate deferred corrections on convection based
*     on the selection of a higher order convection scheme.
*
*     DCCE(ID)  Deferred Correction on Convection at the East face
*
**********************************************************************
*     
      REAL XP(ID),XE(ID)
      REAL DCCE(ID),PHI(ID),ME(ID),ALFAE(ID)
      REAL PHEHOS(ID),PHEUDS(ID)
      REAL BETA
      INTEGER ADVSCM,IB,IE,ID,I
      
      BETA = 1
      CALL NULL(DCCE, IB,IE,ID)
*
*----------------------------
* No higher-order Scheme
*----------------------------
*
      IF(ADVSCM.LE.0.5) THEN
*
      CALL NULL(DCCE, IB,IE,ID)
*
*----------------------------
* CDS Scheme 
*----------------------------
*
      ELSEIF(ADVSCM.EQ.1) THEN
*
      
      DO 10 I=IB,IE-1
      
        PHEHOS(I)=0.5*(PHI(I)+PHI(I+1))
        PHEUDS(I)=0.5*(1+ALFAE(I))*PHI(I)+
     C           0.5*(1-ALFAE(I))*PHI(I+1)
      
        DCCE(I) = BETA * (ME(I)*PHEHOS(I)-ME(I)*PHEUDS(I))
       
 10   CONTINUE
      
*----------------------------
*QUICK scheme 
*----------------------------
*
      ELSEIF(ADVSCM.EQ.2)THEN
*      
      
      DO 100 I=IB,IE-1
        PHEUDS(I)=0.5*(1+ALFAE(I))*PHI(I)+
     C             0.5*(1-ALFAE(I))*PHI(I+1)
        IF (ME(I).GE.0) THEN

          PHEHOS(I)=(XE(I)-XP(I))*(XE(I)-XP(I+1))
     C           /(XP(I-1)-XP(I))/(XP(I-1)-XP(I+1))*PHI(I-1)+
     C           (XE(I)-XP(I-1))*(XE(I)-XP(I+1))
     C           /(XP(I)-XP(I-1))/(XP(I)-XP(I+1))*PHI(I)  +
     C           (XE(I)-XP(I-1))*(XE(I)-XP(I))
     C           /(XP(I+1)-XP(I-1))/(XP(I+1)-XP(I))*PHI(I+1)
        
        ELSEIF (ME(I).LT.0) THEN
      
*         TEHOS(I)=6.0/8*T(I+1)-1.0/8*T(I+2)+3.0/8*T(I)
          PHEHOS(I)=(XE(I)-XP(I+2))*(XE(I)-XP(I+1))
     C           /(XP(I)-XP(I+1))/(XP(I)-XP(I+2))*PHI(I)+
     C            (XE(I)-XP(I))*(XE(I)-XP(I+2))
     C           /(XP(I+1)-XP(I))/(XP(I+1)-XP(I+2))*PHI(I+1)+
     C            (XE(I)-XP(I))*(XE(I)-XP(I+1))
     C           /(XP(I+2)-XP(I))/(XP(I+2)-XP(I+1))*PHI(I+2)
          
        ENDIF 
        DCCE(I) = BETA * (ME(I)*PHEHOS(I)-ME(I)*PHEUDS(I))
       
 100  CONTINUE
      
      
      ENDIF
*     
      
      RETURN
      END
