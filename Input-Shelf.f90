! =====================================================================C
! =====================================================================C
      PARAMETER (MM=451,NN=10001)
      DIMENSION Z(MM),ZP(NN,MM),R(NN)
      DIMENSION BRM(5),HP(5),DHP(5)
      DIMENSION DHX(NN),P(NN)
      DIMENSION R2(NN,MM),W2(NN,MM),PS2(NN,MM),V2(NN,MM)
      CHARACTER*22 DIR
      DIR='C:\ALL-MY\ACTIVE-RUNS\'
! =================================================================
! =================================================================  
      PRINT 306
  306 FORMAT(24X,'INITIAL DATA')
      OPEN(22,FILE=DIR//'INPUT\PARM1BEAM.DAT')
      READ(22,*) H0,AL,HG,PSI0,PER,FI,HUPPER,DELH0,TET,EPS,SEPS,&
      BRM(1),HP(1),DHP(1),BRM(2),HP(2),DHP(2),BRM(3),HP(3),DHP(3),&
      BRM(4),HP(4),DHP(4),BRM(5),HP(5),DHP(5),&
      AX,AZ,ROMIXW,COFMIXWX,COFMIXWZ,&
      RX,RZ,ROMIXR,COFMIXRX,COFMIXRZ,COFSPON,COFDIFM
      CLOSE(22)
      OPEN(42,FILE=DIR//'INPUT\PARM2BEAM.DAT')  
      READ(42,*) M,N,KPER,KT,KPRGRD,KPRVEL,NDNO1,NDNO2,KDIF1,KDIF2,&
      KDIF3,KDIF4,LDP,KGRD1,KGRD2,KGRDSTEP,KPRGRDZ,MONOT,MBUSS,NBOTFR
      CLOSE(42)
! ======================================================================
! DEFINITION OF CONSTANTS  =============================================
! ======================================================================
      NN1=NN-1
      PI=3.141592
!      DX=1./(NN-1) 
      DZ=1./(MM-1)
! ======================================================================
! ========== BOTTOM FUNCTION IN THE INITIAL VARIABLES ==================
! ======================================================================
      DO 46 I=1,NN
      DHX(I)=H0
   46 CONTINUE
      DO 56 I=NDNO1,NDNO2
      ARG=0.5*PI*(I-NDNO1)/(NDNO2-NDNO1)
      DHX(I)=H0-HG*(SIN(ARG))**2
   56 CONTINUE
      DO 556 I=NDNO2,NN
      DHX(I)=H0-HG
  556 CONTINUE
      R(1)=1.
      P(1)=0.
      DO 5 I=2,NN
      R(I)=H0/DHX(I)
      P(I)=(NN-1)*(DHX(I)-DHX(I-1))/DHX(I)
    5 CONTINUE
      PRINT 302
  302 FORMAT(15X,'BOTTOM FUNCTIONS WERE DEFINED')
! ======================================================================
! DEFINITION OF THE BOTTOM FUNCTIONS PQ(I,J), ZP(I,J), ZPQ(I,J)  =======
! ======================================================================
      DO 45 I=1,NN
      DO 45 J=1,MM
      Z(J)=-(J-1)*DZ 
      ZP(I,J)=Z(J)*P(I)
   45 CONTINUE

      OPEN(77,FILE=DIR//'OUTPUT.DAT\dno.dat')
      DO 600 I=1,NN
      WRITE(77,43) I,DHX(I),R(I),P(I)
  600 CONTINUE
      CLOSE(77)
   43 FORMAT(1x,I7,F9.2,2E12.4)


! ====================================================================
! ================ DEFINITION OF CONSTANTS ============================
! =====================================================================
      TSCALE=3600.*PER
      SIGMA=2*PI/TSCALE
      VSCALE=1.
      FF=2.*0.0000729*SIN(PI*FI/180.)
      FF1=2.*0.0000729*COS(PI*FI/180.)
! ====================================================================
! =====================================================================

      DO 1 I=1,NN
      DO 1 J=1,MM
      PS2(I,J)=0.
      W2(I,J)=0.
      R2(I,J)=0.
      V2(I,J)=PSI0*(R(I)*FF/H0-FF1*ZP(I,J)*SIN(TET)/AL)/VSCALE/SIGMA 
    1 CONTINUE
                    
      TBEG=0.

      OPEN(43,FILE=DIR//'OUTPUT.DAT\W2.DAT')
      WRITE(43,231) TBEG
      WRITE(43,201) ((W2(I,J),J=1,MM),I=1,NN)
      CLOSE(43)
      PRINT 105
  105 FORMAT(/10X,'VORT.FIELD W2.DAT WAS WRITTEN IN OUTPUT.DAT')
      OPEN(45,FILE=DIR//'OUTPUT.DAT\PS2.DAT')
      WRITE(45,231) TBEG
      WRITE(45,201) ((PS2(I,J),J=1,MM),I=1,NN)
      CLOSE(45)
      PRINT 106
  106 FORMAT(/10X,'STREAM FUNCTION PS2.DAT WAS WRITTEN IN OUTPUT.DAT')
      OPEN(44,FILE=DIR//'OUTPUT.DAT\R2.DAT')
      WRITE(44,231) TBEG
      WRITE(44,201) ((R2(I,J),J=1,MM),I=1,NN)
      CLOSE(44)
      OPEN(46,FILE=DIR//'OUTPUT.DAT\V2.DAT')
      WRITE(46,231) TBEG
      WRITE(46,201) ((V2(I,J),J=1,MM),I=1,NN)
      CLOSE(46)
      PRINT 107
  107 FORMAT(/10X,'DENS.FIELD R2.DAT WAS WRITTEN IN OUTPUT.DAT')
  201 FORMAT(5E14.7)
  231 FORMAT(F8.4)
      PRINT 122
  122 FORMAT(//10X,'H A P P Y    E N D ,   B A B Y ! ! !')
      STOP
      END
    
