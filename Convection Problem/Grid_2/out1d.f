*
************************************************************************
*
      SUBROUTINE OUT1D(A,CVAR,IUNIT,IB,IE,ISKP,ID)
*
*     Print the header and output for a 1d array formatted
*
*     REAL A(ID)        The array to be output
*     CHARACTER*8 CVAR  Name of the variable A to be printed
*     INTEGER IUNIT     Unit number to which array is sent
*     INTEGER IB,IE,ISKP begin, end, and skip index of output
*     INTEGER ID        Dimension of output array A(I)      
*
*     NOTES:
*     NWORD columns are printed accross the page, and this
*     is repeated as many times as necessary to print the
*     required entries of A.
*     Integer NWORD is set as a parameter (default NWORD=11)
*     CVAR must contain 8 characters, including blanks.
*
************************************************************************
*
      REAL A(ID)
      INTEGER IUNIT,IB,IE,ISKP,ID
      INTEGER NWORD,IPNT(100),LI,LINE,I1,IN,IMAX
      CHARACTER*8 CVAR
      INTEGER I
*     
      PARAMETER (NWORD=7)
*
      LI=0
      DO 10 I=IB,IE,ISKP
         LI=LI+1
         IPNT(LI)=I
 10   CONTINUE
      IMAX=LI
*
      WRITE(IUNIT,104)CVAR
      LINE=0
 30   LINE=LINE+1
      IN=LINE*NWORD
      I1=IN-NWORD+1
      IN=AMIN0(IN,IMAX)
      WRITE(IUNIT,100)(A(IPNT(I)),I=I1,IN)
      WRITE(IUNIT,101)(IPNT(I),I=I1,IN)
      IF(IMAX .GT. IN)GO TO 30
 100  FORMAT(' ',100(1PD11.3))
 101  FORMAT(' I-> ',3X,100(I4,7X))
 104  FORMAT(' ',/,' ',A8)
      RETURN
      END
