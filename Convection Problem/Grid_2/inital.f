*
*        file inital.f contains two subroutines: INITAL and SAVE
***********************************************************************
*
      SUBROUTINE INITAL(T, T0,IRSI,IB,IE,ID)
*
*     Subroutine to set initial T fields by fixing values at T0
*
*     T(ID)   T array for interior and boundary nodes; output
*     T0      initial guess for uniform T-distribution; input
*     
*     IRSI    file number for restart data file; input
*     IB,IE   first and last interior indices in i; input
*     ID      array dimensions; input
*
***********************************************************************
*
      REAL T(ID),T0
      INTEGER IRSI,IB,IE,ID,I 
*
      DO 10 I=IB-1,IE+1
         T(I)= T0
 10   CONTINUE
*
      RETURN
      END
*
*
************************************************************************
*
      SUBROUTINE SAVE(T, IRSO,IB,IE,ID)
*
*     Routine to save solution arrays in unformatted form to restart
*     code.
*          
*     T(ID)   T array to be saved;  input
*
*     IRSO    unit number of file for saved data; input
*     IB,IE   first and last interior indices in i; input
*     ID      array dimensions; input
*
************************************************************************
*
      REAL T(ID)
      INTEGER IRSO,IB,IE,ID,I
*     
      WRITE(IRSO) (T(I),I=IB-1,IE+1)
*     
      RETURN
      END
