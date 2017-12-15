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
      PARAMETER(ID=10)
      REAL YNE(ID),ZUE(ID),XP(ID),XE(ID)
      REAL DIEP(ID),DISW(ID),DISE(ID),DSXY(ID),DSXZ(ID)
      REAL AREP(ID),ARO(ID),VOLP(ID)
      REAL WORK1(ID),WORK2(ID)
      REAL DE(ID),QT(ID),RT(ID)
      REAL ATW(ID),ATE(ID),ATP(ID),BT(ID)
      REAL T(ID),TOLD(ID)
*
      INTEGER I,KNTIN,IE1
      INTEGER IDATI,IRSI,IDATO,IRSO,ITERMO
      INTEGER IB,IE,IDTYP,KNTTM
      INTEGER LVLGEO,LVLCOF,KNTOUT
      REAL DI,RHO,COND,CP,EMIS,VISC,T0,DTIME,CRIT
      REAL HCONV,TINF
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
     C          RHO,COND,CP,VISC,EMIS,
     C          T0,DTIME,KNTTM,KNTNL,CRIT,
     C          LVLGEO,LVLCOF,HCONV,TINF,IDATI)
      CALL ECHO(IB,IE,IDTYP,DI,
     C          RHO,COND,CP,VISC,EMIS,
     C          T0,DTIME,KNTTM,KNTNL,CRIT,
     C          LVLGEO,LVLCOF,HCONV,TINF,IDATO)
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
*--Initialize T field
*
      CALL INITAL(T, T0,IRSI,IB,IE,ID)
*
*--Print initialized field
*
      CALL OUT1D(T  ,'T(init)',IDATO,IB-1,IE+1,1,ID)
*
*--Compute active coefficients for T
*
        CALL NULL(BT, IB,IE,ID)
        CALL DIFPHI(DE, COND,AREP,DIEP,IB,IE,ID)
        CALL SRCT(QT,RT, T,VOLP,ARO,HCONV,TINF,IB,IE,ID)
        CALL COEFF(ATP,ATW,ATE,BT,
     C             DE,QT,RT,ARO,VOLP,RHO,CP,HCONV,
     C             IB,IE,ID)
*
*--Set boundary conditions
*
        CALL BNDCT(ATP,ATW,ATE,BT, DE,AREP,IB,IE,ID)
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
*--Compute solution using direct solver 
*
        CALL TDMA(T, ATP,ATE,ATW,BT,IB-1,IE+1,WORK1,WORK2,ID)
*
*--Print final solution
*
        CALL OUT1D(T  , ' T      ',IDATO,IB-1, IE+1,1,ID)
*
*--Save result to unformatted output file
*
      CALL SAVE(T,IRSO,IB,IE,ID)
*
      STOP
      END
