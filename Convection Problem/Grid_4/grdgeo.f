*            file grdgeo.f
*
*********************************************************************
*
      SUBROUTINE GRDGEO(XP,DIEP,DISE,DISW,AREP,ARO,VOLP,
     C       XE,YNE,ZUE,IDTYP,DSXY,DSXZ,IB,IE,ID,IDATO,ITERMO,LVLGEO)
*     
*     Generate volumes, areas and distances associated with a 1D
*     volume-based grid.
*
*     Output:
*     
*     REAL XP        x-value at node location 
*     REAL DIEP      x-distance between nodes P and E 
*     REAL DISE      x-distance from node P to integration point e
*     REAL AREP      area of the C.V. face through integration point e
*     REAL ARO       surface area of duct for C.V. centered at P
*     REAL VOLP      volume of C.V. associated with node P
*
*     Input:
*
*     REAL XE        value of x at the east face of a C.V.
*     REAL YNE       value of y at the east corner of a C.V.
*     REAL ZUE       value of z at the east corner of a C.V.
*     INTEGER IDTYP  =1 rectangular duct Y=Y(X), Z=Z(X)
*                    =2 2D-duct Y=Y(X), width in z-direction=1.0
*                    =3 axisymmetric duct of radius Y(X)  
*                    =4 graded duct of radius Y(X)  
*     INTEGER ID     I-dimension of arrays
*     INTEGER IDATO  unit number for output to file
*     INTEGER ITERMO unit number for output to screen
*     INTEGER LVLGEO .GE. 1-> output geometry ;=0, no output
*
********************************************************************
*
      REAL PI
      INTEGER IDTYP,IB,IE,IDATO,ITERMO,ID,LVLGEO
      INTEGER I,IBM1,IEP1
      REAL AREP(ID),ARO(ID),DIEP(ID),DISE(ID),DISW(ID),VOLP(ID),XP(ID)
      REAL XE(ID),YNE(ID),ZUE(ID),DSXY(ID),DSXZ(ID)
      PARAMETER(PI=3.1415927)
*
*  Define first and last nodes
*     
      IBM1=IB-1
      IEP1=IE+1
*
*  Define node locations
*
      XP(1)=XE(1)
      DO 10 I=IB,IE
         XP(I)=(XE(I-1)+XE(I))*0.5
   10 CONTINUE
      XP(IEP1)=XE(IE)
*
*  Distances between nodes
*
      DIEP(IBM1)=XP(IB)-XP(IBM1)
      DISE(IBM1)=XE(IBM1)-XP(IBM1)
      DO 20 I=IB,IE
         DIEP(I)=XP(I+1)-XP(I)
         DISE(I)=XE(I)-XP(I)
         DISW(I)=XP(I)-XE(I-1)
 20   CONTINUE
      DISW(IEP1)=XP(IEP1)-XE(IE)
*
*  Arc length in x-y and x-z planes
*
      DO 40 I=IB,IE
         DSXY(I)=SQRT( (XE(I)-XE(I-1))**2 + (YNE(I)-YNE(I-1))**2 )
         DSXZ(I)=SQRT( (XE(I)-XE(I-1))**2 + (ZUE(I)-ZUE(I-1))**2 )
   40 CONTINUE
*
*  Compute areas and volumes 
*
      GO TO (50,60,70),IDTYP
*
   50 CONTINUE
*
*  (1) Rectangular duct geometry
* 
      IF(LVLGEO .GT. 1)THEN
       WRITE(IDATO,950)
 950   FORMAT(' ',//,
     C      'GENERATE GEOMETRY IN GRDGEO',/,
     C      'RECTANGULAR DUCT GEOMETRY')
      ENDIF
      DO 51 I=IBM1,IE
         AREP(I)=4.0*ZUE(I)*YNE(I)
   51 CONTINUE
      DO 52 I=IB,IE
         ARO(I)=4.*(DSXY(I)*(ZUE(I)+ZUE(I-1))/2.) +
     C                  4.*(DSXZ(I)*(YNE(I)+YNE(I-1))/2.)
         VOLP(I)=4.*(XE(I)-XE(I-1))/3. *
     C         (YNE(I-1)*ZUE(I-1)+YNE(I)*ZUE(I)+
     C               0.5*(YNE(I-1)*ZUE(I)+YNE(I)*ZUE(I-1)))
   52 CONTINUE
      RETURN
*
   60 CONTINUE
*
*  (2) 2D Duct geometry - Unit width in z direction
* 
      IF(LVLGEO .GT. 1)THEN
       WRITE(IDATO,951)
 951   FORMAT(' ',//,
     C      'GENERATE GEOMETRY IN GRDGEO',/,
     C      ' 2D-DUCT GEOMETRY')
      ENDIF
      DO 61 I=IBM1,IE
         AREP(I)=(2.*YNE(I))
   61 CONTINUE
      DO 62 I=IB,IE
         ARO(I)=2.*DSXY(I)
         VOLP(I) =2.*(XE(I)-XE(I-1))*(YNE(I-1)+YNE(I))*0.5
   62 CONTINUE
      RETURN
   70 CONTINUE
*
*  (3) Axisymmetric duct geometry
*
      IF(LVLGEO .GT. 1)THEN
       WRITE(IDATO,952)
 952   FORMAT(' ',//,
     C      'GENERATE GEOMETRY IN GRDGEO',/,
     C      ' AXISYMMETRIC DUCT WITH RADIUS Y')
      ENDIF
      DO 71 I=IBM1,IE
         AREP(I)=PI*YNE(I)**2
   71 CONTINUE
      DO 72 I=IB,IE
         ARO(I)=2.0*PI*DSXY(I)*(YNE(I)+YNE(I-1))*0.5
         VOLP(I)=PI*(XE(I)-XE(I-1))/3. *
     C           (YNE(I-1)**2+YNE(I)*YNE(I-1)+YNE(I)**2)
   72 CONTINUE
      RETURN
*
*     ERROR IN WORK ARRAY DIMENSIONS
 900  WRITE(ITERMO,901)ID,ID
 901  FORMAT(' ',/,' DIMENSION OF WORK ARRAYS ID=',I3,
     C     ' MUST EQUAL OR EXCEED ID=',I3,/,
     C     ' EXECUTION STOPPED IN SUBROUTINE GRDGEO')
      STOP
      END


