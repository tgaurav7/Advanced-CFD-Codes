*
*        File input.f contains 3 subroutines: FILDEF, INPUT and ECHO
********************************************************************
*
      SUBROUTINE FILDEF(IDATI,IRSI,IDATO,IRSO,ITERMO)
*
*     Subroutine to define unit numbers and open data files.
*
*     INTEGER IDATI   unit number for formatted input data; output
*     INTEGER IRSI    unit number for binary restart data; output
*     INTEGER IDATO   unit number for formatted output;output
*     INTEGER IRSO    unit number for binary output; output
*     INTEGER ITERMO  unit number for terminal output; output
*
*********************************************************************
*
      INTEGER IDATI,IRSI,IDATO,IRSO,ITERMO
*     
*--File unit numbers
*
      IDATI=71
      IDATO=72
      ITERMO=6
      IRSI=75
      IRSO=76
*
      OPEN(IDATI,FILE='in.dat')
      OPEN(IDATO,FILE='out.dat')
      OPEN(IRSI,FORM='UNFORMATTED',FILE='rsi.bin')
      OPEN(IRSO,FORM='UNFORMATTED',FILE='rso.bin')
*
      RETURN
      END
*
*
*********************************************************************
*
      SUBROUTINE INPUT(IB,IE,IDTYP,DI,
     C                ADVSCM,BCTE,BCTW,CDSA,
     C                 RHO,COND,CP,VISC,EMIS,
     C                 T0,DTIME,KNTTM,KNTNL,CRIT,
     C                 LVLGEO,LVLCOF,HCONV,TINF,
     C                 U0,UHE0,P0,INLETU,OUTLETP,IDATI)
*
*     Subroutine to read in input variable from a data file.
*
*     INTEGER IB,IE  first and last interior indices in i; output
*     REAL DI        length between pins in i direction; output
*     REAL RHO       fluid density; output
*     REAL COND      thermal conductivity; output
*     REAL CP        specific heat at constant pressure; output
*     REAL VISC      viscosity of fluid; output
*     REAL EMIS      emissivity of surface; output
*     REAL T0        initial constant temperature; output
*     REAL DTIME     time step ; output
*     INTEGER KNTTM  maximum number of time steps;  output
*     INTEGER KNTNL  maximum number of iterations;  output
*     REAL CRIT      convergence criteria for non-linearities; output
*     INTEGER LVLGEO parameter to control output of geometry; output
*     INTEGER LVLCOF parameter to control output of coef. ; output
*
*     REAL HCONV     convective heat transfer coefficient.
*     REAL TINF      ambient temperature.
*     INTEGER IDATI  unit number for file with input data; input
*
**********************************************************************
*
      INTEGER IB,IE,IDTYP,KNTTM,KNTNL,LVLGEO,LVLCOF,IDATI
      INTEGER ADVSCM,BCTE,BCTW,CDSA
      REAL DI
      REAL RHO,COND,CP,VISC,EMIS,T0,DTIME,CRIT,HCONV,TINF
      REAL INLETU,OUTLETP
*
      READ(IDATI,5000) IB,IE,IDTYP,DI
      READ(IDATI,5050) ADVSCM,BCTE,BCTW,CDSA 
      READ(IDATI,5010) RHO,COND,CP,VISC,EMIS
      READ(IDATI,5020) T0,DTIME,KNTTM,KNTNL,CRIT
      READ(IDATI,5030) LVLGEO,LVLCOF,HCONV,TINF
      READ(IDATI,5040) U0,UHE0,P0,INLETU,OUTLETP
*
 5000 FORMAT(/I5/I5/I5/E12.5)
 5010 FORMAT(E12.5/E12.5/E12.5/E12.5/E12.5)
 5020 FORMAT(E12.5/E12.5/I5/I5/E12.5)
 5030 FORMAT(I5/I5/E12.5/E12.5)
 5040 FORMAT(E12.5/E12.5/E12.5/E12.5/E12.5)
 5050 FORMAT(I5/I5/I5/I5)
*
      RETURN
      END
*
*
********************************************************************
*
      SUBROUTINE ECHO(IB,IE,IDTYP,DI,
     C                ADVSCM,BCTE,BCTW,CDSA,
     C                RHO,COND,CP,VISC,EMIS,
     C                T0,DTIME,KNTTM,KNTNL,CRIT,
     C                LVLGEO,LVLCOF,HCONV,TINF,
     C                U0,UHE0,P0,INLETU,OUTLETP,IDATO)
*
*     Subroutine to echo the input parameters read in from file.
*
*     INTEGER IDATO  unit number for formatted output file; input
*
*     See INPUT for other variable definitions
*
*********************************************************************
*
      INTEGER IB,IE,IDTYP,KNTTM,KNTNL,LVLGEO,LVLCOF,IDATO
      INTEGER ADVSCM,BCTE,BCTW,CDSA
      REAL INLETU,OUTLETP
      REAL DI
      REAL RHO,COND,CP,VISC,EMIS,T0,DTIME,CRIT,HCONV,TINF
*
      WRITE(IDATO,6000) IB,IE,IDTYP,DI
      WRITE(IDATO,6060) ADVSCM,BCTE,BCTW,CDSA
      WRITE(IDATO,6010) RHO,COND,CP,VISC,EMIS
      WRITE(IDATO,6020) T0,DTIME,KNTTM,KNTNL,CRIT
      WRITE(IDATO,6030) LVLGEO,LVLCOF,HCONV,TINF
      WRITE(IDATO,6040) U0,UHE0,P0,INLETU,OUTLETP
*
 6000 FORMAT('              IB = ',I5,/
     C      ,'              IE = ',I5,/
     C      ,'           IDTYP = ',I5,/
     C      ,'              DI = ',1PD13.5,/,' ')
 6010 FORMAT('             RHO = ',1PD13.5,/
     C      ,'            COND = ',1PD13.5,/
     C      ,'              CP = ',1PD13.5,/
     C      ,'            VISC = ',1PD13.5,/
     C      ,'            EMIS = ',1PD13.5,' ')
 6020 FORMAT('              T0 = ',1PD13.5,/
     C      ,'           DTIME = ',1PD13.5,/
     C      ,'           KNTTM = ',I5,/
     C      ,'           KNTNL = ',I5,/
     C      ,'            CRIT = ',1PD13.5/,' ')
 6030 FORMAT('          LVLGEO = ',I5,/
     C      ,'          LVLCOF = ',I5,/
     C      ,'           HCONV = ',1PD13.5,/
     C      ,'            TINF = ',1PD13.5,/,' ')
*
 6040 FORMAT('              U0 = ',1PD13.5,/
     C      ,'            UHE0 = ',1PD13.5,/
     C      ,'              P0 = ',1PD13.5,/
     C      ,'          INLETU = ',1PD13.5,/
     C      ,'         OUTLETP = ',1PD13.5,/,' ')
 6060 FORMAT('          ADVSCM = ',I5,/
     C      ,'            BCTE = ',I5,/
     C      ,'            BCTW = ',I5,/
     C      ,'            CDSA = ',I5,/ ' ')     
     
      RETURN
      END
*******************************************************

