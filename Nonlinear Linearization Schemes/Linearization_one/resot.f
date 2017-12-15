      SUBROUTINE RESOT(ITER,AVRSD)


      INTEGER ITER
      REAL AVRSD

      WRITE(IDATO,6000) ITER
      WRITE(IDATO,6010) AVRSD

 6000 FORMAT('              IB = ',I5,/,')

 6010 FORMAT('             RHO = ',1PD13.5,/,' ')

      
      RETURN
      END
