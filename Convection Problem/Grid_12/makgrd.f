*
*          file makgrd.f
*************************************************************************
*
      SUBROUTINE MAKGRD(XE,YNE,ZUE, DI,IDTYP,
     C                  IB,IE,ID,ITERMO,IDATO)
*
*     Subroutine to generate XE,YNE,ZUE to define the northeast corner of 
*     of each control-volume strung out along the x-axis.    
*     
*     Output:
*
*     REAL XE         value of X at the east face of control-volume  
*     REAL YNE        value of Y at the east corner of control-volume  
*     REAL ZUE        value of Z at the east corner of control-volume  
*
*     Input:
*
*     REAL DI         length of control-volumes in x direction
*     INTEGER IDTYP   =1 rectangular duct Y=Y(X), Z=Z(X)
*                     =2 2D-duct Y=Y(X), width in z-direction=1.0
*                     =3 axisymmetric duct of radius Y(X)  
*     Other:
*
*     INTEGER ID      I-dimension of arrays 
*     INTEGER ITERMO  unit number for output to screen
*     INTEGER IDATO   unit number for output to data file
*     
************************************************************************
*
      REAL X,Y,Z,XE(ID),YNE(ID),ZUE(ID),DI,PI,DY1,GRADE
      INTEGER IDTYP,ITERMO,IDATO,IB,IE,I,ID,NCV
      PARAMETER(PI=3.1415927)
*     
*  Statement functions for Y(X) and Z(X) profile of the duct
*
      Y(X)= 0.005/2
      Z(X)= 0.005/2
*
*-----------------------------------------------------------------------
*  No changes required below this line
*
      IF((IE+1).GT.ID) GOTO 111
*
*  Define XE(), YNE() & ZUE() based on IDTYP
*
* (1) Rectangular Duct geometry
*
      IF(IDTYP.EQ.1) THEN
         DO 1 I=IB-1,IE
            XE(I)= DI*(I-1)
            YNE(I)= Y(XE(I))
            ZUE(I)= Z(XE(I))
  1      CONTINUE
*
* (2) 2D Duct geometry
*
      ELSE IF(IDTYP.EQ.2) THEN
         DO 2 I=IB-1,IE
            XE(I)= DI*(I-1)
            YNE(I)= Y(XE(I))
            ZUE(I)= 0.5
  2      CONTINUE
*
* (3) Axisymmetric Duct geometry
*
      ELSE IF(IDTYP.EQ.3) THEN
         DO 3 I=IB-1,IE
            XE(I)= DI*(I-1)
            YNE(I)= Y(XE(I))
            ZUE(I)= 0.0
  3      CONTINUE
*
* (4) Graded Duct with a grid interface
*
c      ELSE IF(IDTYP.EQ.4) THEN
*
*  check for input errors
*
c	 IF(IE1.GT.IE) THEN
c	   WRITE(ITERMO,903)
c	   WRITE(IDATO,903)
c           GOTO 111
c         ENDIF
*
*  bottom portion of grid
*
c         XE(IB-1)= 0.0
c	 YNE(IB-1)= Y(XE(IB-1))
c	 ZUE(IB-1)= Z(XE(IB-1))
*
c	 NCV= IE1-IB+1
c	 SUMDX= 0.0
c	 DO 4 I=1,NCV
c	   SUMDX= SUMDX+(1.0-(I-1)*(1.0-1.0/IRAT1)/(NCV-1))
c  4      CONTINUE
*
c	 DY1= GRDI/SUMDX
c	 GRADE= DY1/(NCV-1)*(1.0-1.0/IRAT1)
c	 XE(IB)= XE(IB-1)+DY1
c	 YNE(IB)= Y(XE(IB))
c	 ZUE(IB)= Z(XE(IB))
*
c	 DO 5 I=2,NCV
c	   XE(I+1)= XE(I)+(DY1-(I-1)*GRADE)
c	   YNE(I+1)= Y(XE(I+1))
c	   ZUE(I+1)= Z(XE(I+1))
c  5      CONTINUE
*
*  top portion of grid
*
c	 NCV= IE-IE1
c	 SUMDX=0.0
c	 DO 6 I=1,NCV
c	   SUMDX= SUMDX+(1.0-(I-1)*(1.0-IRAT2)/(NCV-1))
c  6      CONTINUE
*
c	 DY1= (GRDH-GRDI)/SUMDX
c	 GRADE= DY1/(NCV-1)*(1.0-IRAT2)
c	 XE(IE1+1)= XE(IE1)+DY1 
c	 YNE(IE1+1)= Y(XE(IE1+1))
c	 ZUE(IE1+1)= Z(XE(IE1+1))
c	 DO 7 I=2,NCV
c	   XE(IE1+I)= XE(IE1+I-1)+(DY1-(I-1)*GRADE)
c           YNE(IE1+I)= Y(XE(IE1+I))
c	   ZUE(IE1+I)= Z(XE(IE1+I))
c  7      CONTINUE
*
*  Error check
*
      ELSE
         WRITE(ITERMO,902) IDTYP
         WRITE(IDATO,902) IDTYP
      END IF
*
      XE(IE+1)= 0.0
      YNE(IE+1)= 0.0
      ZUE(IE+1)= 0.0
      RETURN
*
*  Error Trap      
*
 111  WRITE(ITERMO,901)IE+1,ID
      WRITE(IDATO, 901)IE+1,ID
 901  FORMAT(' ','The number of nodes, IEP1=',I3,
     C     ' exceeds the array dimension',' ID=',I3,/,
     C     '  EXECUTION STOPPED IN MAKGRD')
 902  FORMAT(/,'Invalid IDTYP (',I3,
     C                 ') used for control Volume Specification')
 903  FORMAT(/,'Error in grid number specification, IE1 > IE ',/)
      STOP
      END

