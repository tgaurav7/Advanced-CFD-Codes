*
*        file main.f
*********************************************************************
*
*     Course:  MME 9710 - Advanced CFD
*
*     Mainline for computations of one-dimensional diffusion 
*     problems.
*
********************************************************************
*
*============================
*  Declaration of Variables
*============================
*
      PARAMETER(ID=70)
      REAL YNE(ID),ZUE(ID),XP(ID),XE(ID)
      REAL DIEP(ID),DISW(ID),DISE(ID),DSXY(ID),DSXZ(ID)
      REAL AREP(ID),ARO(ID),VOLP(ID)
      REAL WORK1(ID),WORK2(ID)
      REAL DE(ID),QT(ID),RT(ID)
      REAL ATW(ID),ATE(ID),ATP(ID),BT(ID)
      REAL T(ID),TOLD(ID),DEOLD(ID),RSD(ID)
      REAL U(ID),UHE(ID),P(ID),DPDX(ID)
      REAL ACUE(ID), ACUW(ID),BC(ID),ME(ID)
      REAL ALFAE(ID),DCCE(ID)
      REAL QU(ID),RU(ID)
      REAL AUP(2,2,ID),AUW(2,2,ID),AUE(2,2,ID),BU(2,ID)
      REAL DHUE(ID),UOLD(ID)
      
*
      INTEGER I,IE1,KNTTM,KNTNL
      INTEGER IDATI,IRSI,IDATO,IRSO,ITERMO
      INTEGER IB,IE,IDTYP
      INTEGER LVLGEO,LVLCOF
      INTEGER ADVSCM,BCTE,BCTW,CDSA
      INTEGER NON,TSTP,RW,REPT
      REAL DI,RHO,COND,CP,EMIS,VISC,DTIME,CRIT
      REAL AVRSD
      REAL HCONV,TINF
      REAL INLETU,OUTLETP,T0,U0,UHE0,P0
      INTEGER TEMPSOLV
*
*
*============================
*  Initialization and input
*============================
*
*--Read input parameters
*
      CALL FILDEF(IDATI,IRSI,IDATO,IRSO,ITERMO)
      CALL INPUT(IB,IE,IDTYP,DI,
     C          ADVSCM,BCTE,BCTW,CDSA,
     C          RHO,COND,CP,VISC,EMIS,
     C          T0,DTIME,KNTTM,KNTNL,CRIT,
     C          LVLGEO,LVLCOF,HCONV,TINF,
     C          U0,UHE0,P0,INLETU,OUTLETP,IDATI)
     
      CALL ECHO(IB,IE,IDTYP,DI, 
     C          ADVSCM,BCTE,BCTW,CDSA,
     C          RHO,COND,CP,VISC,EMIS,
     C          T0,DTIME,KNTTM,KNTNL,CRIT,
     C          LVLGEO,LVLCOF,HCONV,TINF,
     C          U0,UHE0,P0,INLETU,OUTLETP,IDATO)
*
*--Define the integration points at the C.V. corners.
*
      CALL MAKGRD(XE,YNE,ZUE, DI,IDTYP,
     C            IB,IE,ID,ITERMO,IDATO)
*
*--Compute geometry parameters.
*
      CALL GRDGEO(XP,DIEP,DISE,DISW,AREP,ARO,VOLP,
     C     XE,YNE,ZUE,IDTYP,DSXY,DSXZ,IB,IE,ID,IDATO,ITERMO,LVLGEO)
*
*--Check integration points and geometry (set LVLGEO=1 to check)
*
      IF( LVLGEO .GE. 1) THEN
         CALL OUT1D(XE,  ' XE     ',IDATO,IB-1,IE+1,1,ID)
         CALL OUT1D(YNE, ' YNE    ',IDATO,IB-1,IE+1,1,ID)
         CALL OUT1D(ZUE, ' ZUE    ',IDATO,IB-1,IE+1,1,ID)
         CALL OUT1D(XP,  ' XP     ',IDATO,IB-1,IE+1,1,ID)
         CALL OUT1D(DIEP,' DIEP   ',IDATO,IB-1,IE  ,1,ID)
         CALL OUT1D(DISE,' DISE   ',IDATO,IB-1,IE  ,1,ID)
         CALL OUT1D(AREP,' AREP   ',IDATO,IB-1,IE+1,1,ID)
         CALL OUT1D(ARO, ' ARO    ',IDATO,IB  ,IE  ,1,ID)
         CALL OUT1D(VOLP,' VOLP   ',IDATO,IB-1,IE+1,1,ID)
      END IF
*
*--Initialize T , U , UHE field
*
      CALL INITAL(T,T0,U,U0,UHE,UHE0,P,P0,IRSI,IB,IE,ID) 
    
**
*--Print initialized field
*
      CALL OUT1D(T  ,'T(init)',IDATO,IB-1,IE+1,1,ID)
      
      CALL GRADP(DPDX, P,DIEP,IB,IE,ID)

**********************************************
!--start the time loop:
**********************************************

      DO 20 TSTP=1,KNTTM
        
        WRITE(*,*)'TIME STEP NO:',TSTP
       
***************************************
!store the previous temperature field 
***************************************

        DO 30 RW=IB-1,IE+1
        
          UOLD(RW)=U(RW)
          TOLD(RW)=T(RW)
          DEOLD(RW)=DE(RW)
          
 30     CONTINUE 
******************************************
       TEMPSOLV = 0
*      SOLVE THE TEMPERATURE FIELD       
******************************************
      
      IF (TEMPSOLV.EQ.1) THEN
*     START THE NONLINEAR LOOP FOR TEMPERATURE

        DO 50 NON=1,KNTNL
*
*--Compute active coefficients for T
*
     
          CALL NULL(BT, IB,IE,ID)
 
          CALL DIFPHI(DE, COND,AREP,DIEP,IB,IE,ID)
          CALL COEFCN(ACUW,ACUE,BC,AREP,RHO,IB,IE,ID)
          CALL MASFLX(ME,UHE,ACUW,ACUE,BC,IB,IE,ID)
          CALL WEIGHT(ALFAE, ME,DE,CDSA,IB,IE,ID)
          CALL HOCONV(DCCE,XP,XE,ME,T,ALFAE,ADVSCM,IB,IE,ID)
          CALL SRCT(QT,RT,T,VOLP,ARO,TOLD,DE,DCCE,
     C                HCONV,TINF,RHO,DTIME,CP,  
     C                IB,IE,ID)
          CALL COEFF(ATP,ATW,ATE,BT,TOLD,DTIME,
     C                 DE,QT,RT,ARO,VOLP,ME,ALFAE,
     C                 RHO,CP,HCONV,
     C                 IB,IE,ID)
          
*          
*--Set boundary conditions
*
          CALL BNDCT(ATP,ATW,ATE,BT,DE,AREP, 
     C             HCONV,TINF,BCTE,BCTW,
     C             IB,IE,ID)
 
*
*--Check computed, active coefficients (LVLCOF=1 to check)
*
          IF (LVLCOF .GE. 1) THEN
              CALL OUT1D(DE,   ' DE     ',IDATO,IB-1, IE  ,1,ID)
              CALL OUT1D(ATW , ' ATW    ',IDATO,IB-1, IE+1,1,ID)
              CALL OUT1D(ATP , ' ATP    ',IDATO,IB-1, IE+1,1,ID)
              CALL OUT1D(ATE , ' ATE    ',IDATO,IB-1, IE+1,1,ID)
              CALL OUT1D(BT  , ' BT     ',IDATO,IB-1, IE+1,1,ID)
          ENDIF
*
! to skip checking the residuals for first iteration:
*
          IF(NON.EQ.1)GOTO 502
*
*check residuals:
*           
          CALL RESID(RSD,AVRSD,T,ATP,ATW,ATE,BT,IB,IE,ID)
         
          WRITE(*,*)'ITERATION NUMBER:',NON-1  
          WRITE(*,*)'AVERAGE RESIDUAL:',AVRSD      
*
! to check the residual from the second iteration:
*
          IF (AVRSD.LT.CRIT.OR.NON.GT.KNTNL) GOTO 501
*
!--Compute solution using direct solver 
*
 502      CONTINUE
 
          CALL TDMA(T,ATP,ATE,ATW,BT,IB-1,IE+1,WORK1,WORK2,ID)
        
*--END OF NONLINEAR ITERATIONS LOOP

 50     CONTINUE 
 
 501     CONTINUE
 
        WRITE(*,*) 'TEMPERATURE FIELD'
        DO 503 REPT=IB-1,IE+1
          WRITE(*,*) T(REPT)
 503    CONTINUE   
      
      
      ENDIF

******************************************
!!!!  FLOW FIELD SOLVING 
******************************************
        CALL NULLVB(BU,IB,IE,2,ID)
        CALL NULLMB(AUP,IB-1,IE+1,2,ID)
        CALL NULLMB(AUE,IB-1,IE+1,2,ID)
        CALL NULLMB(AUW,IB-1,IE+1,2,ID)

!start the nonlinear iterations 
******************************************
       DO 10 NON=1,KNTNL
      
*
*--Compute active coefficients for T
*
    
 
        CALL DIFPHI(DE,VISC,AREP,DIEP,IB,IE,ID)
        
        CALL COEFCN(ACUW,ACUE,BC,AREP,RHO,IB,IE,ID)
        
        CALL MASFLX(ME,UHE,ACUW,ACUE,BC,IB,IE,ID)
        
        CALL WEIGHT(ALFAE, ME,DE,CDSA,IB,IE,ID)
        
        CALL HOCONV(DCCE,XP,XE,ME,U,ALFAE,ADVSCM,IB,IE,ID)
        
        CALL GRADP(DPDX, P,DIEP,IB,IE,ID)
        
*       IF (NON.GT.1) THEN
*
*         CALL UHAT(UHE, U,P,DHUE,RHO,DPDX,DIEP,IB,IE,ID)
*         
*        ENDIF  
 
        CALL SRCU(QU,RU,U,UHE,DCCE,RHO,VISC,XE,ARO,AREP,IB,IE,ID)
        
        CALL COEFFM(AUP,AUW,AUE,BU,ME,DE,QU,RU,PHI,VOLP,ALFAE,RHO,DTIME,
     C            UOLD,IB,IE,ID)
        CALL SRCUP(AUP,AUW,AUE,BU,DIEP,VOLP,IB,IE,ID)
        
        CALL BNDCU(AUP,AUW,AUE,BU,INLETU,IB,IE,ID)
        
        CALL DHAT(DHUE,AUP,VOLP,RHO,IB,IE,ID)
      
        CALL ADCONT(AUP,AUW,AUE,BU,ACUW,ACUE,BC,DHUE,DPDX,
     C                  RHO,DIEP,DISE,IB,IE,ID)
        CALL BNDCP(AUP,AUW,AUE,BU,P,DPDX,OUTLETP,DIEP,IB,IE,ID)
      
          
*
! to skip checking the residuals for first iteration:
*
        IF(NON.EQ.1)GOTO 102

*
*check residuals:
*
           
         CALL RESIDM(AVRSD,RSD, P,U,2,AUP,AUW,AUE,BU,IB,IE,ID)
         
         WRITE(*,*)'ITERATION NUMBER:',NON-1  
         WRITE(*,*)'AVERAGE RESIDUAL:',AVRSD
      
*
! to check the residual from the second iteration:
*
         IF (AVRSD.LT.CRIT.OR.NON.GT.KNTNL) GOTO 101
*
!--Compute solution using direct solver 
*
 102  CONTINUE
              
         CALL SOLUP(AUP,AUW,AUE,BU,P,U,IB,IE,ID)
         
         CALL GRADP(DPDX, P,DIEP,IB,IE,ID)

         CALL UHAT(UHE, U,P,DHUE,RHO,DPDX,DIEP,IB,IE,ID)
 
        
*--END OF NONLINEAR ITERATIONS LOOP OF FLOW FIELD
 10   CONTINUE 

*--EXIT FROM THE NONLINEAR ITRATIONS IF THE SOLUTION CONVERGED 
 101     CONTINUE
 
          WRITE(*,*) '  VELOCITY FIELD' , '    PRESSURE FIELD' , ' uhat'
          
          DO 103 REPT=IB-1,IE+1
           WRITE(*,*) U(REPT) , P(REPT), UHE(REPT)
 103      CONTINUE 

*--END OF TIME STEP 
 20   CONTINUE
 
*
*--Print final solution
*
        CALL OUT1D(U  , ' T      ',IDATO,IB-1, IE+1,1,ID)
*
*--Save result to unformatted output file
*
      CALL SAVE(U,IRSO,IB,IE,ID)
*
      STOP
      END
