c23456789012345678901234567890123456789012345678901234567890123456789012
      PROGRAM MC2
      IMPLICIT NONE
      INTEGER*4 NRAND, I, J, L, N, IRAND, IR, JR, IPAS, IMC, MCTOT,T
      INTEGER*4 SUMA, MCINI, MCD, ILLAV, LLAV0,NLLAV,ENERDIF, LLAV1
      INTEGER*4 S(1:256,1:256), PBC(0:257), DELTAENER,TPAS
      REAL*4 RRAND(1:196608), TIME1, TIME2, TIMEPAST
      REAL*8 ENE, ENERG, MAG, MAGNE, SUME0, TEMP0
      REAL*8 SUM0, SUME, SUME2, SUMM, SUMM2, SUMAM, VARE,VARM
      REAL*8 TEMP,TINC, DE, DELTA, EXPTEMP(1:8)
      CHARACTER*29 NOM
      CHARACTER*8 DATE
      CHARACTER*17 FILENAME
      NAMELIST /DADES/ NOM,L,TEMP,TPAS,TINC,NLLAV,LLAV0,MCTOT,MCINI,MCD

c     Cridem CPU_TIME per calcular el temps d'execució
      CALL CPU_TIME(TIME1)

c     Llegim el nom del fitxer de dades d'entrada incorporat
c     al executar el programa ./MC2.exe /data/MC2-Lxxx.dat
c     L màxima 256
      DO I = 1, IARGC()
        CALL GETARG(i, filename)
        WRITE (*,*) filename
      END DO



c      Lectura de parametres inicials a partir del fixer de dades
c      L=16
c      TEMP=4.5D0
c      TPAS=10
c      TINC=0.2D0
c      NLLAV=10
c      LLAV0=117654
c      MCTOT=10000
c      MCINI=1000
c      MCD=10
c      NOM="SIMUL-TEMP4500-MCTOT10000"
      OPEN(UNIT=10,FILE=filename)
      READ(10,DADES)
      CLOSE(10)


c     DIMENSIONS
      N=L*L
      NRAND=3*N
c     valors inicials per calcular el primer punt de d<e>/dT
      SUME0=-2
      TEMP0=1.51d0

c     inicialització PeriodicBoundaryConditions
      PBC(0)=L
      PBC(L+1)=1
      DO I=1,L
        PBC(I)=I
      ENDDO

c      DO I=0,257
c        WRITE(*,*) PBC(I)
c      ENDDO
c     Obrim els fitxers en els que escriurem els output i els resultats
      OPEN(UNIT=12,FILE="output/"//NOM//".out") !fitxer d'escriptura d'outputs
      OPEN(UNIT=13,FILE="res/"//NOM//".res") !fitxer d'escriptura de resultats


c     Bucle de temperatures
      DO T=0,TPAS

c       inicialitzem els SUMATORIS
        SUM0=0.0D0
        SUME=0.0D0
        SUME2=0.0D0
        SUMM=0.0D0
        SUMM2=0.0D0
        SUMAM=0.0D0

c       vector de l'exponencial per calcular-la un sol cop

        EXPTEMP(1)=0d0
        EXPTEMP(2)=0d0
        EXPTEMP(3)=0d0
        EXPTEMP(4)=dexp(-4.0d0/TEMP)
        EXPTEMP(5)=0d0
        EXPTEMP(6)=0d0
        EXPTEMP(7)=0d0
        EXPTEMP(8)=dexp(-8.0d0/TEMP)

c       BUCLE Promitjos llavor
        DO ILLAV=LLAV0, LLAV0+NLLAV-1, 1
c          WRITE(*,*) "Llavor=", ILLAV

c         Cridem RCARIN per inicialitzar el calcul dels nombres aleatoris
          CALL RCARIN(ILLAV, RRAND, NRAND)
          CALL RCARRY(RRAND, NRAND) !omplim el vector de nombres aleatoris
          IRAND=1

c         Creem la matriu d'spins aleatoriament amb 1 i -1
          DO I=1,L
            DO J=1,L
              IF (RRAND(IRAND).lt.0.5E0) THEN
                S(I,J)=1
c                WRITE(*,*) I, J
              ELSE
                S(I,J)=-1
              ENDIF
              IRAND=IRAND+1
            ENDDO
          ENDDO

c         Inicialitzem l'index de MC
          IMC=0
c         Calculem la magnetització i la energia inicials
          MAG=MAGNE(S,L)      !M/N
          ENE=ENERG(S,L,PBC)  !E/N
          WRITE(12,*) IMC, ENE, MAG

c         BUCLE MONTECARLO
          DO IMC=1,MCTOT
c           Tornem a omplir el vector de nous nombres aleatoris
            CALL RCARRY(RRAND, NRAND)
            IRAND=1
c           Bucle de le N propostes aleatòries de flip d'spin
            DO IPAS=1,N
c             escollim els index IR i JR aleatoriament
              IR=INT(RRAND(IRAND)*L)+1
              IRAND=IRAND+1
c             WRITE(*,*) "IR", IR
              JR=INT(RRAND(IRAND)*L)+1
              IRAND=IRAND+1
c             WRITE(*,*) "JR", JR
c             Cridem la funció DELTAENER, calculem el canvi energetic proposat
              ENERDIF=DELTAENER(IR,JR,S,PBC)
c             WRITE(*,*) ENERDIF
              IF (ENERDIF.LE.0.D0) THEN
                S(IR, JR)=-S(IR, JR)
              ELSE
                DELTA=RRAND(IRAND)
                IRAND=IRAND+1
                IF (DELTA.LE.EXPTEMP(ENERDIF)) THEN
                  S(IR, JR)=-S(IR, JR)
                ENDIF
              ENDIF
            ENDDO

c           Calculem variables pels multimples de MC dessitjats
            IF((IMC.GT.MCINI).AND.(MCD*(IMC/MCD).EQ.IMC)) THEN
              MAG=MAGNE(S,L)            !magnetització
              ENE=ENERG(S,L,PBC)        !energia
              SUM0=SUM0+1d0             !comtador per normalitzar
              SUME=SUME+ENE             !sumem e
              SUME2=SUME2+(ENE*ENE)     !sumem e^2
              SUMM=SUMM+MAG             !sumem m
              SUMM2=SUMM2+(MAG*MAG)     !sumem m^2
              SUMAM=SUMAM+ABS(MAG)      !sumem |m|
              WRITE(12,*) IMC, ENE, MAG !Escrivim els outputs
            ENDIF
          ENDDO
        ENDDO
c       Mostrem per pantalla la temperatura calculada
        WRITE(*,*) 'Temperatura calculada', TEMP

c       Normalitzem i calculem variances
        SUME=SUME/SUM0            !<e>
        SUME2=SUME2/SUM0          !<e^2>
        SUMM=SUMM/SUM0            !<m>
        SUMM2=SUMM2/SUM0          !<m^2>
        SUMAM=SUMAM/SUM0          !<|m|>
        VARE=SUME2-(SUME*SUME)    !Var(E)
        VARM=SUMM2-(SUMAM*SUMAM)  !Var(M)

c       Escribim els resultats al fitxer
c                   N,T,SUM,<e>,<e2>,VAR<e>,<m>,<|m|>,<m2>,VAR<m>,
        WRITE(13,*) N,TEMP,SUM0,SUME,SUME2,VARE,SUMM,SUMAM,SUMM2,VARM,
c                   sqrt<m2>, c_v
     +              sqrt(SUMM2),N*VARE/(TEMP*TEMP),
c                   d<e>/dT
     +              ((SUME-SUME0)/(TEMP-TEMP0)),
c                   chi
     +              N*VARM/TEMP

c       calculem el temps del pas de temperatures
        CALL CPU_TIME(TIMEPAST)
        WRITE(*,*)"CPUTIME T =",TIMEPAST-TIME1
c       Guardem resultats per calcular la derivada d<e>/dT
        SUME0=SUME
        TEMP0=TEMP
c       modifiquem l'increment de temperatura per T~Tc
        IF ((TEMP.GT.2).AND.(TEMP.LT.2.5d0)) THEN
          TEMP=TEMP+(TINC/2)
        ELSE
          TEMP=TEMP+TINC
        ENDIF
c        WRITE(*,*) "Magnetització;",MAG
c        WRITE(*,*) "Energia", ENE
      ENDDO

c     Tanquem els fitxers d'escriptura
      CLOSE(12)
      CLOSE(13)

c     CONTROL de temps CPU final
      CALL CPU_TIME(TIME2)
c     Data i hora final
      CALL DATE_AND_TIME(DATE=DATE)
c     Mostrem per pantalla la data i el temps total d'execució
      WRITE(*,*) DATE
      WRITE(*,*) "CPUTIME =",TIME2-TIME1

      END

c
c
c     ******************************************************************
c     *                     FUNCTION MAGNE                             *
c     *                                                                *
c     *             calcula la magnnetització per spin                 *
c     *                          S,L                                   *
c     *                                                                *
c     ******************************************************************
c

      REAL*8 FUNCTION MAGNE(S,L)

      INTEGER*4 S(1:256,1:256)
      INTEGER*4 I,J,L, N
      REAL*8 MAG

      MAG=0.0D0
      N=L*L

      DO I =1,L
        DO J=1,L
          MAG=MAG+S(i,j)
        ENDDO
      ENDDO

      MAGNE=MAG/REAL(N)

      RETURN
      END


c
c     ******************************************************************
c     *                     FUNCTION ENERG                             *
c     *                                                                *
c     *                calcula l'energia per spin                      *
c     *                         S,L,PBC                                *
c     *                                                                *
c     ******************************************************************
c

      REAL*8 FUNCTION ENERG(S,L, PBC)

      INTEGER*4 S(1:256,1:256)
      INTEGER*4 I,J,L,N
      INTEGER*4 PBC(0:257)
      REAL*8 ENE

      ENE=0.0D0
      N=L*L

      DO I =1,L
        DO J=1,L
          ENE=ENE-S(I,J)*S(PBC(I+1),J)-S(I,J)*S(I,PBC(J+1))
        ENDDO
      ENDDO

      ENERG=ENE/REAL(N)

      RETURN
      END


c     ******************************************************************
c     *                        Funció DELTAENER                        *
c     *                                                                *
c     *                  calcula l'increment de l'energia              *
c     *                            i, j, s(), PBC()                    *
c     *                                                                *
c     ******************************************************************
c


      INTEGER*4 FUNCTION DELTAENER(i, j, S,PBC)

      INTEGER*4 S(1:256,1:256)
      INTEGER*4 SUMVEINS, PBC(0:257), i, j

      SUMVEINS=0

      SUMVEINS=(S(i,PBC(j+1))+S(i,PBC(j-1)))
      SUMVEINS=SUMVEINS+(S(PBC(i+1),j)+S(PBC(i-1),j))

      DELTAENER=2*S(i,j)*SUMVEINS

c      Write(*,*) S(i,j), S(i,PBC(j+1)), S(i,PBC(j-1))
c     +    ,S(PBC(i+1),j), S(PBC(i-1),j), SUMVEINS, DELTAENER

      RETURN
      END


c
c     ******************************************************************
c     *                     SUBROUTINE RCARIN                          *
c     ******************************************************************
c

      SUBROUTINE RCARIN(IJKL,RVEC,LENV)

C----------------------------------------------------------------------
C Inicializa valores antes de llamar a la subrutina RCARRY.
C IJKL debe estar en el rango 0<IJKL<900 000 000.
C Para conseguir los valores standar usados por Marsaglia y Zaman en su
C articulo poner IJKL = 54217137 (I=12, J=34, K=56, L=78)
C Version modificada (mas rapida que el original). (2/9/91)
C----------------------------------------------------------------------
      COMMON /RAN1/ CARRY
      DIMENSION RVEC(LENV+24)

      IJ = IJKL/30082
      KL = IJKL - 30082*IJ
      I = MOD(IJ/177,177) + 2
      J = MOD(IJ,177)     + 2
      K = MOD(KL/169,178) + 1
      L = MOD(KL,169)

      DO 2 II=24,1,-1
        S = 0.0
        T = 0.5
        DO 3 JJ=1,24
          M = MOD(MOD(I*J,179)*K,179)
          I = J
          J = K
          K = M
          L = MOD(53*L+1,169)
          IF (MOD(L*M,64).GE.32) S = S+T
          T = 0.5*T
3       CONTINUE
        RVEC(II) = S
2     CONTINUE

      CARRY = 0.0

      RETURN
      END
c
c     ******************************************************************
c     *                     SUBROUTINE RCARRY                          *
c     ******************************************************************
c
      SUBROUTINE RCARRY(RVEC,LENV)
C----------------------------------------------------------------------
C Generador de numeros pseudo-aleatorios. Algoritmo de G. Marsaglia y
C A. Zaman. Genera numeros reales de 32-bits con mantisas de 24 bits,
C comprendidos entre 0 y 1 (1, explicitamente excluido).
C Periodo aproximado : 10**171.
C Admite la generacion de subsecuencias disjuntas.
C                   F. James, 1989
C Version modificada (mas rapida que el original). (2/9/91)
C----------------------------------------------------------------------
      DIMENSION RVEC(LENV+24)
      COMMON /RAN1/ CARRY
      PARAMETER (TWOM24=1.0/16777216.0)
C
      DO 100 IVEC=25,LENV+24
        UNI = RVEC(IVEC-24) - RVEC(IVEC-10) - CARRY
        IF (UNI.LT.0.) THEN
          UNI = UNI + 1.0
          CARRY = TWOM24
        ELSE
          CARRY = 0.0
        ENDIF

        IF(UNI.EQ.0.)THEN
          UNI=RVEC(IVEC-24)*TWOM24
            in48=-48
          IF(UNI.EQ.0.)UNI=2**(in48)
        ENDIF

        RVEC(IVEC) = UNI
100   CONTINUE

      DO 200 I=1,24
200   RVEC(I)=RVEC(LENV+I)

      RETURN
      END
