! =====================================================================C
! =====================================================================C
      PARAMETER (MM=451,NN=10001)
      DIMENSION A(NN),B(NN),RO0(MM),ARGUM(MM)
      DIMENSION Z(MM),ZP(NN,MM),ZPQ(NN,MM),R(NN)
      DIMENSION BRM(5),HP(5),DHP(5)
      DIMENSION BRW(MM),BRW2(MM),AJ(5),BJ(5),CJ(5)
      DIMENSION XX(NN,MM),U(NN,MM),W(NN,MM),U2max(NN,MM),W2max(NN,MM)
      DIMENSION PS(NN,MM),F(NN,MM),r2x(nn,mm),XX0(NN,MM)
      DIMENSION PS0(NN,MM),W0(NN,MM),R0(NN,MM)
      DIMENSION DENS(NN,MM),RI(NN,MM),BUY2(NN,MM)
      DIMENSION Z20(NN,MM),Z21(NN,MM),Z22(NN,MM),Z33(NN,MM),Z40(NN,MM)
      DIMENSION Z13(NN,MM),Z14(NN,MM),Z15(NN,MM),Z16(NN,MM),Z24(NN,MM)
      DIMENSION DHX(NN),P(NN),Q(NN)
      DIMENSION SPONCH(NN)
      DIMENSION V1(NN,MM),V(NN,MM)
      COMMON /B/KGRD1,KGRD2,KGRDSTEP,KPRGRDZ
      COMMON /D/R2(NN,MM),W2(NN,MM),PS2(NN,MM),V2(NN,MM)
      COMMON /C/DIR
      CHARACTER*12 UU,VV,WW,RR,DENSITY,RICH,BUY,U2AMPL
      CHARACTER*12 AMPCENTR,UCENTR,WCENTR,RPQ,W2AMPL
      CHARACTER*1 LETA,LETU,LETW,LETRPQ
!     CHARACTER*12 Z022,BUSS,RXX
!     CHARACTER*5 TTT
      CHARACTER*22 DIR
      DIR='C:\ALL-My\ACTIVE-RUNS\'
      PRINT 123
  123 FORMAT(10X,'H A L L 0 ,  B A B Y !  L E T  U S  B E G I N .')
                
! =================================================================
! =================================================================
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
      READ(42,*) M,N,KPER,KT,KPRGRD,KPRVEL,NDNO1,NDNO2,&
	  KDIF1,KDIF2,KDIF3,KDIF4,LDP,KGRD1,KGRD2,KGRDSTEP,KPRGRDZ,&
	  MONOT,MBUSS,NBOTFR
      CLOSE(42)
      OPEN(23,FILE=DIR//'INPUT\PARM3BEAM.DAT')
      READ(23,*) MIXPAKAN,AZ0,ALFA,NPAKAN
      CLOSE(23)
! =================================================================
! =================================================================
! =================================================================
! =================================================================  
      WRITE(*,763) H0,AL,HG,PSI0,PER,FI,HUPPER,DELH0,TET,EPS,SEPS,&
      BRM(1),HP(1),DHP(1),BRM(2),HP(2),DHP(2),BRM(3),HP(3),DHP(3),&
      BRM(4),HP(4),DHP(4),BRM(5),HP(5),DHP(5),&
      AX,AZ,ROMIXW,COFMIXWX,COFMIXWZ,&
      RX,RZ,ROMIXR,COFMIXRX,COFMIXRZ,COFSPON,COFDIFM
  763 FORMAT(2X,7HH0=====,E11.4,2X,7HAL=====,E11.4,2X,7HHG=====,E11.4/&
      2X,7HPSI0===,E11.4,2X,7HPER====,E11.4,2X,7HFI=====,E11.4/&
      2X,7HHUPPER=,E11.4,2X,7HDELH0==,E11.4,2X,7HTET====,E11.4/&
      2X,7HEPS====,E11.4,2X,7HSEPS===,E11.4/&
      2X,7HBRM(1)=,E11.4,2X,7HHP(1)==,E11.4,2X,7HDHP(1)=,E11.4/&
      2X,7HBRM(2)=,E11.4,2X,7HHP(2)==,E11.4,2X,7HDHP(2)=,E11.4/&
      2X,7HBRM(3)=,E11.4,2X,7HHP(3)==,E11.4,2X,7HDHP(3)=,E11.4/&
      2X,7HBRM(4)=,E11.4,2X,7HHP(4)==,E11.4,2X,7HDHP(4)=,E11.4/&
      2X,7HBRM(5)=,E11.4,2X,7HHP(5)==,E11.4,2X,7HDHP(5)=,E11.4/&
      2X,7HAX=====,E11.4,2X,7HAZ=====,E11.4/&
      2X,7HROMIXW=,E11.4,2X,7HCMIXWX=,E11.4,2X,7HCMIXWZ=,E11.4/&
      2X,7HRX=====,E11.4,2X,7HRZ=====,E11.4/&
      2X,7HROMIXR=,E11.4,2X,7HCMIXRX=,E11.4,2X,7HCMIXRZ=,E11.4/&
      2X,7HCOFSPN=,E11.4,2X,7HCOFDIFM,E11.4/) 
      WRITE(*,784) M,N,KPER,KT,KPRGRD,KPRVEL,NDNO1,NDNO2,NDNO1,NDNO2,&
	  KDIF1,KDIF2,KDIF3,KDIF4,LDP,KGRD1,KGRD2,KGRDSTEP,KPRGRDZ,MONOT,&
	  MBUSS,NBOTFR
  784 FORMAT(2X,7HM======,I4,2X,7HN======,I4,2X,6HKPER==,I5,2X,6HKT====,I5/&
      2X,7HKPRGRD=,I4,2X,7HKPRVEL=,I4,2X,7HKDNO1==,I4,2X,7HKDNO2==,I4/&
	  2X,7HKDNO3==,I4,2X,7HKDNO4==,I4/&
      2X,7HKDIF1==,I4,2X,7HKDIF2==,I4,2X,7HKDIF3==,I4,2X,7HKDIF4==,I4,2X,7HLDP====,I4/&
      2X,7HKGRD1==,I4,2X,7HKGRD2==,I4,2X,7HKGRDX==,I4,2X,7HKGRDZ==,I4/&
      2X,7HMONOT==,I4,2X,7HMBUSS==,I4,2X,7HNBOTFR=,I4/)
      WRITE(*,6) MIXPAKAN,AZ0,ALFA,NPAKAN
    6 FORMAT(2X,10HMIXPAKAN==,I1,&
      2X,7HAZ0====,E11.4,2X,7HALFA===,E11.4,2X,7HNPAKAN=,I4)
! =================================================================
! =================================================================
! =================================================================
! =================================================================  
      PRINT 309        
  309 FORMAT(20X,'READING OF THE INITIAL FIELDS ')
  201 FORMAT(5E14.7)
  231 FORMAT(F8.4)
      OPEN(31,FILE=DIR//'INPUT\PS2.DAT')
      READ(31,231) TBEG
      READ(31,201) ((PS2(I,J),J=1,MM),I=1,NN)
      CLOSE(31)
      OPEN(33,FILE=DIR//'INPUT\W2.DAT')
      READ(33,231) TBEG
      READ(33,201) ((W2(I,J),J=1,MM),I=1,NN)
      CLOSE(33)
      OPEN(35,FILE=DIR//'INPUT\R2.DAT')
      READ(35,231) TBEG
      READ(35,201) ((R2(I,J),J=1,MM),I=1,NN)
      CLOSE(35)
      OPEN(37,FILE=DIR//'INPUT\V2.DAT')
      READ(37,231) TBEG
      READ(37,201) ((V2(I,J),J=1,MM),I=1,NN)
      CLOSE(37)
      PRINT 303
  303 FORMAT(20X,'INITIAL FIELDS WERE DEFINED')
      TBEG1=TBEG                                                                                 
      TBEG2=TBEG
! ======================================================================
! DEFINITION OF CONSTANTS  =============================================
! ======================================================================
      MM1=MM-1
      MM2=MM-2
      NN1=NN-1
      PI=3.141592
      DX=1./(NN-1) 
      DZ=1./(MM-1)
      DT=1./KPER
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
      Q(1)=0.   
      R(NN)=H0/(H0-HG)
      P(NN)=0.
      Q(NN)=0.   
      DO 5 I=2,NN1
      R(I)=H0/DHX(I)
      P(I)=(NN-1)*(DHX(I)-DHX(I-1))/DHX(I)
      Q(I)=(NN-1)**2*(DHX(I+1)-2.*DHX(I)+DHX(I-1))/DHX(I)
    5 CONTINUE
      PRINT 302
  302 FORMAT(15X,'BOTTOM FUNCTIONS WERE DEFINED')
      OPEN(77,FILE=DIR//'OUTPUT.DAT\dno.dat')
      DO 60 I=1,NN
      WRITE(77,43) I,DHX(I),R(I),P(I),Q(I)
   60 CONTINUE
      CLOSE(77)
   43 FORMAT(1x,I7,F9.2,3E12.4)
! ======================================================================
! DEFINITION OF THE BOTTOM FUNCTIONS PQ(I,J), ZP(I,J), ZPQ(I,J)  =======
! ======================================================================
      DO 45 I=1,NN
!      PQ(I)=2.*P(I)**2-Q(I)
      DO 45 J=1,MM
      Z(J)=-(J-1)*DZ 
      ZP(I,J)=Z(J)*P(I)
      ZPQ(I,J)=Z(J)*(2.*P(I)**2-Q(I))
   45 CONTINUE
! ======================================================================
! DEFINITION OF THE UNDISTURBED DENSITY PROFILE ========================
! ======================================================================
      DO 8 K=1,5
      AJ(K)=4./(DHP(K)**2*BRM(K))
      BJ(K)=-HP(K)
      CJ(K)=1./BRM(K)
    8 CONTINUE
      OPEN(76,file=dir//'W-BRUNT.dat')
      MUPPER=INT(HUPPER/DZ/H0)+1
      DO 47 J=1,MUPPER
      BRW2(J)=0.
      WRITE(76,411) argum(j),brw(j) 
   47 CONTINUE            
      MUPPER1=MUPPER+1
      DO 909 J=MUPPER1,MM
      ARGUM(J)=(J-1)*DZ*H0
      BRW2(J)=0.
      DO 10 K=1,5
      ABC=AJ(K)*(ARGUM(J)+BJ(K))**2+CJ(K)
      BRW2(J)=BRW2(J)+1./ABC**2
   10 CONTINUE
      BRW(J)=SQRT(BRW2(J))
      WRITE(76,411) argum(j),brw(j) 
  909 CONTINUE
  411 FORMAT(F8.2,F15.6)
      CLOSE(76)
      WBRMAX=0.                  
      DO 1 J=1,MM
      IF(BRW(J).GT.WBRMAX) WBRMAX=BRW(J)
    1 CONTINUE
      DO 192 J=1,MUPPER
      RO0(J)=1000.      
  192 CONTINUE
      DO 92 J=MUPPER1,MM
      GOR1=(J-1)*H0/(MM-1)
      XXX=0.
      DO 98 K=1,5
!      XXX=XXX+(-BJ(K)/(AJ(K)*BJ(K)**2+CJ(K))-&
!      (-BJ(K)-GOR1)/(AJ(K)*(BJ(K)+GOR1)**2+CJ(K))-&
!      SQRT(1./AJ(K)/CJ(K))*&
!      (ATAN(SQRT(AJ(K)/CJ(K))*(-BJ(K)-GOR1))-&
!      ATAN(SQRT(AJ(K)/CJ(K))*(-BJ(K)))))/CJ(K)
      XXX=XXX+((-BJ(K)-HUPPER)/(AJ(K)*(BJ(K)+HUPPER)**2+CJ(K))-&
               (-BJ(K)-GOR1)/(AJ(K)*(BJ(K)+GOR1)**2+CJ(K))-&
               SQRT(1./AJ(K)/CJ(K))*&
              (ATAN(SQRT(AJ(K)/CJ(K))*(-BJ(K)-GOR1))-&
               ATAN(SQRT(AJ(K)/CJ(K))*(-BJ(K)-HUPPER))))/CJ(K)
   98 CONTINUE
      RO0(J)=1000.+0.5*1000.*XXX/9.81
!      RO0(J)=RO0(J-1)+1000.*BRW2(J)*H0/(MM-1)/9.81
      WRITE(*,*) J,GOR1,RO0(J)
   92 CONTINUE   
      OPEN(78,FILE=DIR//'R0-init.dat')
      DO 59 J=1,MM
      GOR1=(J-1)*H0/(MM-1)
      WRITE(78,411) GOR1,RO0(J)
   59 CONTINUE
      CLOSE(78)   
      DO 910 J=1,MM
      BRW2(J)=BRW2(J)/WBRMAX**2
  910 CONTINUE
! ======================================================================
! DEFINITION OF ARRAY DENS(I,J) IN THE NEW VARIABLES OVER THE BOTTOM ==
! ======================================================================
      DO 91 I=1,NN      
      DENS(I,1)=1000.
      DO 97 J=2,MM
      GOR1=(J-1)*H0/(MM-1)/R(I)
      IF(GOR1.LT.HUPPER) DENS(I,J)=1000. 
      IF(GOR1.LT.HUPPER) GOTO 97 
      XXX=0.
      DO 96 K=1,5
!      XXX=XXX+(-BJ(K)/(AJ(K)*BJ(K)**2+CJ(K))-&
!      (-BJ(K)-GOR1)/(AJ(K)*(BJ(K)+GOR1)**2+CJ(K))-&
!      SQRT(1./AJ(K)/CJ(K))*&
!      (ATAN(SQRT(AJ(K)/CJ(K))*(-BJ(K)-GOR1))-&
!      ATAN(SQRT(AJ(K)/CJ(K))*(-BJ(K)))))/CJ(K)
      XXX=XXX+((-BJ(K)-HUPPER)/(AJ(K)*(BJ(K)+HUPPER)**2+CJ(K))-&
               (-BJ(K)-GOR1)/(AJ(K)*(BJ(K)+GOR1)**2+CJ(K))-&
               SQRT(1./AJ(K)/CJ(K))*&
              (ATAN(SQRT(AJ(K)/CJ(K))*(-BJ(K)-GOR1))-&
               ATAN(SQRT(AJ(K)/CJ(K))*(-BJ(K)-HUPPER))))/CJ(K)
   96 CONTINUE
      DENS(I,J)=1000.+0.5*1000.*XXX/9.81        
   97 CONTINUE        
   91 CONTINUE   
      DMIN=1000.
      DMAK=0.          
      DO 93 I=1,NN
      DO 93 J=1,MM
      IF(DENS(I,J).GT.DMAK) DMAK=DENS(I,J)
      IF(DENS(I,J).LT.DMIN) DMIN=DENS(I,J)
   93 CONTINUE
      DENSITY='dens-xz0.grd'
      CALL PRINTGRD(DENSITY,DENS,DMIN,DMAK)
!      WRITE(*,*) WBRMAX    

! ======================================================================
! DEFINITION OF ARRAY BUY2(I,J) IN THE NEW VARIABLES OVER THE BOTTOM ==
! ======================================================================
      DO 691 I=1,NN      
      BUY2(I,1)=0.
      DO 697 J=2,MM
      GOR1=(J-1)*H0/(MM-1)/R(I)
      IF(GOR1.LT.HUPPER) BUY2(I,J)=0. 
      IF(GOR1.LT.HUPPER) GOTO 697 
      BUY2(I,J)=0.
      DO 696 K=1,5
      ABC=AJ(K)*(GOR1+BJ(K))**2+CJ(K)
      BUY2(I,J)=BUY2(I,J)+1./ABC**2    
  696 CONTINUE
      BUY2(I,J)=BUY2(I,J)/WBRMAX**2        
  697 CONTINUE        
  691 CONTINUE   
      BUY2MIN=0.
      BUY2MAK=0.          
      DO 693 I=1,NN
      DO 693 J=1,MM
      IF(BUY2(I,J).GT.BUY2MAK) BUY2MAK=BUY2(I,J)
      IF(BUY2(I,J).LT.BUY2MIN) BUY2MIN=BUY2(I,J)
  693 CONTINUE
      WRITE(*,*) BUY2MIN     
      WRITE(*,*) BUY2MAK      

      BUY='BUY2-xz0.grd'
      CALL PRINTGRD(BUY,BUY2,BUY2MIN,BUY2MAK)
      WRITE(*,*) WBRMAX    


! ====================================================================
! ================ DEFINITION OF CONSTANTS ============================
! =====================================================================
      TSCALE=3600.*PER
      SIGMA=2*PI/TSCALE
      VSCALE=1.
!     OMEG0=PSI0/H0/H0
      DENS0=1000.*PSI0*TSCALE*WBRMAX**2/9.81/AL
      FF=2.*0.0000729*SIN(PI*FI/180.)
      FF1=2.*0.0000729*COS(PI*FI/180.)
      VBOUND=PSI0*FF/(H0*VSCALE*SIGMA)
! ====================================================================
! ============== REDEFINITION OF CRITERIUM CR0  =======================
! =( CR0=0 if we work with full density, then there is no term =======
! === with Vaisala-Brunt frequency in the equation for density) ======
! =====================================================================
      CR0=1.                              ! if we work with pertrubations
      CR0=0. !because the density equations is solved for fool density!!!
! ====================================================================
! =====================================================================
      CR1=(H0/AL)**2
      CR2=PSI0*TSCALE/(H0*AL)
      CR3=(TSCALE*WBRMAX)**2
      CR4=AX*TSCALE/AL**2
      CR5=AZ*TSCALE/H0**2
      CR6=RX*TSCALE/AL**2
      CR7=RZ*TSCALE/H0**2
      CR8=TSCALE*FF
      CR9=VSCALE*H0/PSI0
      CR10=FF1*TSCALE*H0*SIN(PI*TET/180.)/AL
      Z1=2./DT
      Z3=1./2./DX
      Z4=1./2./DZ
      Z5=1./DZ**2
      Z6=CR1/2./DZ
      Z7=CR1/DX/DX
      Z8=CR1/2./DZ/DX
      Z9=CR1/DZ/DZ
      Z10=CR1*CR3/2./DX
      Z11=CR1*CR3/2./DZ
      Z12=CR2/4./DX/DZ
      PRINT 301
  301 FORMAT(20X,'CONSTANTS WERE DEFINED')
!      WRITE(*,*) '4'
! =====================================================================
! =====================================================================
!      WRITE(*,*) CR1,CR2,CR3,CR4,CR5,CR6,CR7
!      WRITE(*,*) Z1,Z3,Z4,Z5,Z6,Z7,Z8,Z9,Z10,Z11,Z12 

!      GOTO 222
                                                                                                                                                                                                                                                                                                                                    
      PRINT 304
  304 FORMAT(3X,'BEGINING OF THE TIME BLOCK')
!      IF(PI.GT.1.) GOTO 222
! ====================================================================
! ==== Initial Richardson Numbers, PS-field and nonBoussinesq terms F=
! ====================================================================
      DO 358 I=1,NN
      DO 358 J=1,MM
      RI(I,J)=1000.
      PS(I,J)=PS2(I,J)
      F(I,J)=0.
      r2x(i,j)=0.
      U2max(I,J)=0.
      W2max(I,J)=0.
      V(I,J)=0.
  358 CONTINUE   

! =====================================================================
! =====================================================================
! ======== DISSIPATION: SPONCH AREAS (LEFT AND RIGHT EDGES) ===========
! ======== DISSIPATION: ABOVE THE MOUNTAIN  ===========================
! =====================================================================
! =====================================================================
      DO 787 I=1,NN
      SPONCH(I)=1.
 787 CONTINUE

      DO 788 I=1,LDP
      SPONCH(I)=1.+COFSPON*(SIN(0.5*PI*(LDP-I+1)/LDP))**2      
 788 CONTINUE

      DO 790 I=KDIF1,KDIF2
      SPONCH(I)=1.+COFDIFM*(SIN(0.5*PI*(I-KDIF1)/(KDIF2-KDIF1)))**2      
 790 CONTINUE
                                                        
      DO 791 I=KDIF2,KDIF3
      SPONCH(I)=1.+COFDIFM      
 791 CONTINUE

      DO 792 I=KDIF3,KDIF4
      SPONCH(I)=1.+COFDIFM*(SIN(0.5*PI*(KDIF4-I)/(KDIF4-KDIF3)))**2      
 792 CONTINUE
  
      NBEG=NN-LDP
      DO 793 I=NBEG,NN
      SPONCH(I)=1.+COFSPON*(SIN(0.5*PI*(I-NN+LDP)/LDP))**2                  
 793 CONTINUE 
     
!      OPEN(43,FILE=DIR//'SPONCH.DAT')
!      DO 796 I=1,NN
!      WRITE(43,795) I,SPONCH(I)
!  796 CONTINUE    
!      CLOSE(43)    
!  795 FORMAT(2X,I7, F10.4)    


! =====================================================================
! ======== THE BEGINING TIME BLOCK ====================================
! =====================================================================
      DO 9 K=1,KT
! =====================================================================
! =====================================================================
! =====================================================================
            
      DO 781 I=1,NN
      DO 781 J=1,MM
      XX(I,J)=DENS0*R2(I,J)+(DENS(I,J)-1000.)
      XX0(I,J)=DENS0*R2(I,J)+DENS(I,J)
  781 CONTINUE
      DO 782 I=1,NN
      DO 782 J=1,MM
      IF(XX(I,J).LE.ROMIXR) GOTO 783
      Z20(I,J)=CR6/2./DZ
      Z21(I,J)=CR6/DZ/DZ
      Z22(I,J)=CR6/DX/DX
      Z33(I,J)=CR6/2./DX/DZ
      Z40(I,J)=CR7/DZ/DZ
      GOTO 782
  783 MET=1
      Z20(I,J)=COFMIXRX*CR6/2./DZ
      Z21(I,J)=COFMIXRX*CR6/DZ/DZ
      Z22(I,J)=COFMIXRX*CR6/DX/DX
      Z33(I,J)=COFMIXRX*CR6/2./DX/DZ
      Z40(I,J)=COFMIXRZ*CR7/DZ/DZ
  782 CONTINUE

      DO 682 I=1,NN
      DO 682 J=1,MM
      IF(XX(I,J).LE.ROMIXW) GOTO 683
      Z13(I,J)=CR4/2./DZ
      Z14(I,J)=CR4/DZ/DZ
      Z15(I,J)=CR4/DX/DX
      Z16(I,J)=CR4/2./DX/DZ
      Z24(I,J)=CR5/DZ/DZ
      GOTO 682
  683 MET=1
      Z13(I,J)=COFMIXWX*CR4/2./DZ
      Z14(I,J)=COFMIXWX*CR4/DZ/DZ
      Z15(I,J)=COFMIXWX*CR4/DX/DX
      Z16(I,J)=COFMIXWX*CR4/2./DX/DZ
      Z24(I,J)=COFMIXWZ*CR5/DZ/DZ
  682 CONTINUE  
      DO 794 I=1,NN
      DO 794 J=1,MM
      Z13(I,J)=SPONCH(I)*Z13(I,J)
      Z14(I,J)=SPONCH(I)*Z14(I,J)
      Z15(I,J)=SPONCH(I)*Z15(I,J)
      Z16(I,J)=SPONCH(I)*Z16(I,J)
      Z20(I,J)=SPONCH(I)*Z20(I,J)
      Z21(I,J)=SPONCH(I)*Z21(I,J)
      Z22(I,J)=SPONCH(I)*Z22(I,J)
      Z33(I,J)=SPONCH(I)*Z33(I,J)
      Z24(I,J)=SPONCH(I)*Z24(I,J)
      Z40(I,J)=SPONCH(I)*Z40(I,J)
 794 CONTINUE

! =====================================================================
! ======== DEFINITION OF THE RICHARDSON NUMBER   ======================
! =====================================================================
! =====================================================================
      DO 759 I=1,NN
      DO 759 J=2,MM1
      DU2=PSI0**2*(R(I)/H0/DZ)**4*(PS2(I,J+1)-2.*PS2(I,J)+PS2(I,J-1))**2
      BUOY2=9.81*R(I)*(XX0(I,J+1)-XX0(I,J-1))/(2.*H0*1000.*DZ)
      IF(DU2.LT.0.0000001) RI(I,J)=900.
      IF(DU2.LT.0.0000001) GOTO 759
      RI(I,J)=BUOY2/DU2 
      IF(RI(I,J).GT.900.) RI(I,J)=900.
      IF(RI(I,J).LT.0.) RI(I,J)=0.
  759 CONTINUE   
      DO 859 I=1,NN
      RI(I,1)=RI(I,2)
      RI(I,MM)=RI(I,MM1)
  859 CONTINUE   
! =====================================================================
! ======== PAKANOVSKY - PHILANDER PARAMETERIZARION   ==================
! =====================================================================
      IF(MIXPAKAN.EQ.0) GOTO 802
! =====================================================================
      DO 801 I=1,NN
      DO 801 J=1,MM
      COF1=1./(1.+ALFA*RI(I,J))      
      COFAZ=1.+(AZ0/AZ)*COF1**NPAKAN
      COFRZ=1.+(AZ*COFAZ/RZ)*COF1
      Z24(I,J)=Z24(I,J)*COFAZ
      Z40(I,J)=Z40(I,J)*COFRZ
  801 CONTINUE  
  802 MET=0  
! =====================================================================
! ======== DEFINITION OF THE NONBOUSSINESQ TERM F(I,J) ================
! =====================================================================
      DO 359 I=2,NN1
      DO 359 J=2,MM1
      Z101=PS2(I+1,J+1)-PS2(I-1,J+1)-PS2(I+1,J-1)+PS2(I-1,J-1)
      Z102=PS2(I+1,J)-PS2(I-1,J)
      Z103=PS2(I,J+1)-PS2(I,J-1)
      Z104=PS2(I,J+1)-2.*PS2(I,J)+PS2(I,J-1)
      Z105=PS2(I+1,J)-2.*PS2(I,J)+PS2(I-1,J)
      Z106=PS(I+1,J)-PS(I-1,J)
      Z107=PS(I,J+1)-PS(I,J-1)
      Z108=(XX0(I+1,J)-XX0(I-1,J))/XX0(I,J)
      Z109=(XX0(I,J+1)-XX0(I,J-1))/XX0(I,J)
!      Z106=0.
!      Z107=0.
!      F(I,J)=
!     *0.25*Z5*Z109*(0.5*Z1*(Z107-Z103)+Z12*(Z103*Z101-4.*Z102*Z104))-
!     *0.25*Z7*Z108*(0.5*Z1*(Z102-Z106)+Z12*(Z102*Z101-4.*Z103*Z105))
      F(I,J)=&
      0.25*Z5*Z109*(0.5*Z1*(Z107-Z103)+Z12*(Z103*Z101-4.*Z102*Z104))-&
      0.25*Z7*Z108*(0.5*Z1*(Z102-Z106)+Z12*(Z102*Z101-4.*Z103*Z105))
      r2x(i,j)=z10*(r2(i+1,j)-r2(i-1,j)) ! basic term: for comparison with F(i,j) 
!      F(I,J)=0.
  359 CONTINUE   
!      Z22MAK=0.
!      Z22MIN=0.
!      DO 661 I=1,NN
!      DO 661 J=1,MM
!      IF(Z22(I,J).GT.Z22MAK) Z22MAK=Z22(I,J)
!      IF(Z22(I,J).LT.Z22MIN) Z22MIN=Z22(I,J)
!  661 CONTINUE
!      Z022='Z022FILD.GRD'
!      CALL PRINTGRD(Z022,Z22,Z22MIN,Z22MAK)

!      DO 781 I=1,NN
!      DO 781 J=1,MM
!      XX(I,J)=DENS0*R2(I,J)+(RO0(J)-1000.)
!  781 CONTINUE
!      zmax=0.
!      zmin=0.
!      diss='diss.grd'
!      DO 884 I=1,NN
!      DO 884 J=1,MM
!      IF(z40(I,J).GT.zmax) zmax=z40(I,J)
!      IF(z40(I,J).LT.zmin) zmin=z40(I,J)
!  884 CONTINUE
!      CALL PRINTGRD(diss,z40,zmin,zmax)
!     IF(PI.GT.1.) GOTO 777
!     DO 10 I=1,N1
!     PS2(I,1)=0.
!  10 CONTINUE
!     DO 42 J=1,M1
!     PS2(1,J)=0.
!     PS2(N1,J)=0.
!  42 CONTINUE
! ======================================================================
! ======================================================================
! ======================================================================
      DO 35 I=1,NN1
      DO 35 J=1,MM1
!      R0(I,J)=R2(I,J)
   35 W0(I,J)=W2(I,J)

! =====================================================================
! === Definition of the eddy at the bottom (friction or ideal fluid?) =
! =====================================================================

      DO 115 I=2,NN1
      IF(NBOTFR.EQ.0) GOTO 116
! =====================================================================
! === There is a bottom boundary layer (friction exists!) =============
! =====================================================================

!      XX(I,MM)=Z7*(PS2(I+1,MM)-2.*PS2(I,MM)+PS2(I-1,MM))+&
!      ZP(I,MM)*Z8*((PS2(I+1,MM-2)-PS2(I-1,MM-2))-&
!      4.*(PS2(I+1,MM-1)-PS2(I-1,MM-1))+3.*(PS2(I+1,MM)-PS2(I-1,MM)))+&
!      ZP(I,MM)*ZP(I,MM)*Z9*(-PS2(I,MM-3)+&
!      4.*PS2(I,MM-2)-5.*PS2(I,MM-1)+2.*PS2(I,MM))-&
!      ZPQ(I,MM)*Z6*(PS2(I,MM-2)-4.*PS2(I,MM-1)+3.*PS2(I,MM))+&
!      R(I)*R(I)*Z5*(-PS2(I,MM-3)+&
!      4.*PS2(I,MM-2)-5.*PS2(I,MM-1)+2.*PS2(I,MM))

!            Approximatsiya prostimi proizvodnimi
      XX(I,MM)=Z7*(PS2(I+1,MM)-2.*PS2(I,MM)+PS2(I-1,MM))-&
  	  ZP(I,MM)*Z8*(PS2(I+1,MM)+PS2(I-1,MM-2)-PS2(I+1,MM-2)-PS2(I-1,MM))+&
      ZP(I,MM)**2*Z9*(PS2(I,MM-2)-2.*PS2(I,MM-1)+PS2(I,MM))+&
      ZPQ(I,MM)*Z6*(PS2(I,MM-2)-PS2(I,MM))+&
      R(I)*R(I)*Z5*(PS2(I,MM-2)-2.*PS2(I,MM-1)+PS2(I,MM))

!      XX(I,MM)=R(I)*R(I)*Z5*(PS2(I,MM-2)-2.*PS2(I,MM-1)+PS2(I,MM))
       XX(I,MM)=R(I)*R(I)*Z5*(-PS2(I,MM-3)+&
       4.*PS2(I,MM-2)-5.*PS2(I,MM-1)+2.*PS2(I,MM))


      
!       XX(I,MM)=0.
                                    
      W2(I,MM)=XX(I,MM)
      GOTO 115
  116 MET=4   
! =====================================================================
! === No friction at the bottom!) =====================================
! =====================================================================
      XX(I,MM)=0.
      W2(I,MM)=XX(I,MM)
  115 CONTINUE
! =====================================================================
! ======== VORTECETY - VERTICAL =====================================
! =====================================================================
      DO 17 I=2,NN1
      A(1)=0.
      B(1)=0.
      DO 18 J=2,MM1
      IF(MBUSS.EQ.0) BUSSINES=1.
      IF(MBUSS.NE.0) BUSSINES=1000./XX0(I,J)
      Z25R=R(I)*Z12*(PS2(I+1,J)-PS2(I-1,J))
      Z14ZP2=Z14(I,J)*ZP(I,J)*ZP(I,J)
      Z24R2=Z24(I,J)*R(I)*R(I)
      Z13ZPQ=Z13(I,J)*ZPQ(I,J)
      CCC=Z25R+Z14ZP2+Z13ZPQ+Z24R2

      C=Z1+2.*Z14ZP2+2.*Z24R2-A(J-1)*CCC

      A(J)=-(Z25R-Z14ZP2+Z13ZPQ-Z24R2)/C

      B(J)=(Z12*R(I)*(PS2(I,J+1)-PS2(I,J-1))*(W2(I+1,J)-W2(I-1,J))+&
          Z10*(R2(I+1,J)-R2(I-1,J))*BUSSINES+MBUSS*F(I,J)+&
          Z11*ZP(I,J)*(R2(I,J+1)-R2(I,J-1))+&
          Z1*W2(I,J)+Z15(I,J)*(W2(I+1,J)-2.*W2(I,J)+W2(I-1,J))-&
          Z16(I,J)*ZP(I,J)*(W2(I+1,J-1)-W2(I+1,J+1)-&
          W2(I-1,J-1)+W2(I-1,J+1))-&
          CR8*CR9*R(I)*Z4*(V2(I,J+1)-V2(I,J-1))+&
          CR10*CR9*(Z3*(V2(I+1,J)-V2(I-1,J))+Z4*ZP(I,J)*(V2(I,J+1)-V2(I,J-1)))+&
          B(J-1)*CCC)/C
   18 CONTINUE
!      XX(I,MM)=0.
      XX(I,1)=0.
      DO 19 J=MM1,2,-1
   19 XX(I,J)=A(J)*XX(I,J+1)+B(J)
   17 CONTINUE     
      DO 119 J=1,MM
      XX(1,J)=0.
      XX(NN,J)=0.
  119 CONTINUE     
      
! =====================================================================
! ======== VORTICETY - HORIZONTAL ====================================
! =====================================================================
      DO 22 J=2,MM1
      A(NN)=0.
      B(NN)=0.
      DO 23 I=NN1,2,-1
      IF(MBUSS.EQ.0) BUSSINES=1.
      IF(MBUSS.NE.0) BUSSINES=1000./XX0(I,J)
      Z27=Z12*(PS2(I,J+1)-PS2(I,J-1))
      CCC=-R(I)*Z27-Z15(I,J)
      C=Z1+2.*Z15(I,J)+A(I+1)*CCC
      A(I)=(-R(I)*Z27+Z15(I,J))/C
      B(I)=(-CR8*CR9*R(I)*Z4*(V2(I,J+1)-V2(I,J-1))+&
          CR10*CR9*(Z3*(V2(I+1,J)-V2(I-1,J))+Z4*ZP(I,J)*(V2(I,J+1)-V2(I,J-1)))-&
              Z12*R(I)*(PS2(I+1,J)-PS2(I-1,J))*(XX(I,J+1)-XX(I,J-1))+&
          Z10*(R2(I+1,J)-R2(I-1,J))*BUSSINES+MBUSS*F(I,J)+&
          Z11*ZP(I,J)*(R2(I,J+1)-R2(I,J-1))-&
          Z16(I,J)*ZP(I,J)*(W0(I+1,J-1)-W0(I+1,J+1)-&
          W0(I-1,J-1)+W0(I-1,J+1))+&
          Z14(I,J)*(XX(I,J+1)-2.*XX(I,J)+XX(I,J-1))*ZP(I,J)*ZP(I,J)-&
          Z13(I,J)*(XX(I,J+1)-XX(I,J-1))*ZPQ(I,J)+&
          Z24(I,J)*(XX(I,J+1)-2.*XX(I,J)+XX(I,J-1))*R(I)*R(I)+&
          Z1*XX(I,J)-B(I+1)*CCC)/C
   23 CONTINUE
      W2(1,J)=0.
!      W2(1,J)=(4.*B(2)-B(2)*A(3)-B(3))/(3.-4.*A(2)+A(2)*A(3))
      W2(NN,J)=0.
      DO 24 I=2,NN1
   24 W2(I,J)=A(I)*W2(I-1,J)+B(I)
   22 CONTINUE
      DO 25 I=1,NN 
      W2(I,1)=0.
!      W2(I,MM)=0.                            
   25 CONTINUE
      DO 629 I=2,NN1
!      W2(I,2)=(3.*W2(I,1)+W2(I,3))/4.
  629 W2(I,MM1)=(3.*W2(I,MM)+W2(I,MM-2))/4.
      DO 26 J=2,MM1
      W2(1,J)=0.
!     W2(NN1,J)=0.
!     W2(2,J)=(3.*W2(1,J)+W2(3,J))/4.
!  26 W2(NN1,J)=(3.*W2(NN,J)+W2(N-2,J))/4.
   26 CONTINUE 


! =====================================================================
! ======== VELOCITY V - VERTICAL ======================================
! =====================================================================
      DO 717 I=2,NN1
      A(1)=1.
      B(1)=0.
      DO 718 J=2,MM1
      Z25R=R(I)*Z12*(PS2(I+1,J)-PS2(I-1,J))
      Z14ZP2=Z14(I,J)*ZP(I,J)*ZP(I,J)
      Z24R2=Z24(I,J)*R(I)*R(I)
      Z13ZPQ=Z13(I,J)*ZPQ(I,J)                
      CCC=Z25R+Z14ZP2+Z13ZPQ+Z24R2

      C=Z1+2.*Z14ZP2+2.*Z24R2-A(J-1)*CCC

      A(J)=-(Z25R-Z14ZP2+Z13ZPQ-Z24R2)/C

      B(J)=(Z1*V2(I,J)+&
          Z12*R(I)*(PS2(I,J+1)-PS2(I,J-1))*(V2(I+1,J)-V2(I-1,J))+&
          Z15(I,J)*(V2(I+1,J)-2.*V2(I,J)+V2(I-1,J))+&
          Z4*(PS2(I,J+1)-PS2(I,J-1))*R(I)*CR8/CR9-&
         (Z3*(PS2(I+1,J)-PS2(I-1,J))+Z4*ZP(I,J)*(PS2(I,J+1)-PS2(I,J-1)))*CR10/CR9-&
          Z16(I,J)*ZP(I,J)*(V2(I+1,J-1)-V2(I+1,J+1)-&
          V2(I-1,J-1)+V2(I-1,J+1))+&
          B(J-1)*CCC)/C
  718 CONTINUE
      IF(NBOTFR.EQ.0) GOTO 678
	  V1(I,MM)=0.
	  GOTO 679
  678 V1(I,MM)=(4.*B(MM-1)-A(MM-2)*B(MM-1)-B(MM-2))/&
              (3.-4.*A(MM-1)+A(MM-2)*A(MM-1))
!      V1(I,1)=(4.*B(2)-A(3)*B(2)-B(3))/(3.-4.*A(2)+A(3)*A(2))
  679 MET=1  
      DO 719 J=MM1,2,-1
  719 V1(I,J)=A(J)*V1(I,J+1)+B(J)
      V1(I,1)=(4.*V1(I,2)-V1(I,3))/3.
  717 CONTINUE
! =====================================================================
! ======== VELOCITY V - HORIZONTAL ====================================
! =====================================================================
      
      DO 722 J=2,MM1
      A(NN)=0.
!      B(NN)=0.
      B(NN)=VBOUND*COS(2*PI*(K*DT+TBEG))
      DO 723 I=NN1,2,-1
      Z27=Z12*(PS2(I,J+1)-PS2(I,J-1))
      
      CCC=-R(I)*Z27-Z15(I,J)           ! Pri inversnoi progonke ot NN1 do 2 (zdes')
!      CCC=R(I)*Z27-Z15(I,J)             Pri obichnoi progonke ot 2 do NN1

      C=Z1+2.*Z15(I,J)+A(I+1)*CCC
      
      A(I)=(-R(I)*Z27+Z15(I,J))/C       ! Pri inversnoi progonke ot NN1 do 2 (zdes')    
!      A(I)=(R(I)*Z27+Z15(I,J))/C         Pri obichnoi progonke ot 2 do NN1

      B(I)=(Z1*V1(I,J)-&
          Z12*R(I)*(PS2(I+1,J)-PS2(I-1,J))*(V1(I,J+1)-V1(I,J-1))+&
          Z24(I,J)*R(I)*R(I)*(V1(I,J+1)-2.*V1(I,J)+V1(I,J-1))+&
          Z4*(PS2(I,J+1)-PS2(I,J-1))*R(I)*CR8/CR9-&
         (Z3*(PS2(I+1,J)-PS2(I-1,J))+Z4*ZP(I,J)*(PS2(I,J+1)-PS2(I,J-1)))*CR10/CR9-&
          Z16(I,J)*ZP(I,J)*(V1(I+1,J-1)-V1(I+1,J+1)-&
          V1(I-1,J-1)+V1(I-1,J+1))+&
          Z14(I,J)*(V1(I,J+1)-2.*V1(I,J)+V1(I,J-1))*ZP(I,J)*ZP(I,J)-&
          Z13(I,J)*(V1(I,J+1)-V1(I,J-1))*ZPQ(I,J)-&
          B(I+1)*CCC)/C
  723 CONTINUE
!      V2(1,J)=(4.*B(2)-B(2)*A(3)-B(3))/(3.-4.*A(2)+A(2)*A(3))
!      V2(NN,J)=0.
       V2(1,J)=VBOUND*COS(2*PI*(K*DT+TBEG))
       V2(NN,J)=H0/(H0-HG)*VBOUND*COS(2*PI*(K*DT+TBEG))

      DO 724 I=2,NN1
  724 V2(I,J)=A(I)*V2(I-1,J)+B(I)
  722 CONTINUE
      DO 729 I=1,NN 
	  IF(NBOTFR.EQ.0) GOTO 778
	  V1(I,MM)=0.
	  GOTO 779
  778 V2(I,MM)=(4.*V2(I,MM-1)-V2(I,MM-2))/3.                            
  779 MET=0
      V2(I,1)=(4.*V2(I,2)-V2(I,3))/3.
  729 CONTINUE



! ======================================================================
! = DEFINITION OF THE STREAM-FUNCTION PS(I,J) ON PREVIOUS TEMPORAL LAYER
! ======================================================================
      DO 329 I=1,NN
      DO 329 J=1,MM
      PS(I,J)=PS2(I,J)
  329 CONTINUE
! ======================================================================
! === ITERRATION METHOD OF DUGLAS-RACKFORD  ============================
! ======================================================================
      S0=(1.+SQRT(EPS))/(1.-SQRT(EPS))
      S=SEPS
      IT=0
   30 IT=IT+1
      S=S/S0**(IT-1)
      Z2=2./S
!      DO 330 KITER=1,2
!      DO 430 L=NITER,1,-1
!      IT=IT+1
      DO 29 I=1,NN
      DO 29 J=1,MM
   29 PS0(I,J)=PS2(I,J)
! =====================================================================
! =====================================================================
      PSISURF=SIN(2*PI*(K*DT+TBEG))
! =====================================================================
! ======== STREAM FUNCTION - VERTICAL (WITHOUT BOTTOM FRICTION) =======
! =====================================================================
      DO 32 I=2,NN1
      A(1)=0.
!      B(1)=0.
      B(1)=PSISURF
      DO 33 J=2,MM1
      Z99=Z9*ZP(I,J)*ZP(I,J)
      Z55=Z5*R(I)*R(I)
      Z66=Z6*ZPQ(I,J)
      CCC=Z99+Z66+Z55
      C=Z2+2.*(Z99+Z55)-A(J-1)*CCC
      A(J)=(Z99-Z66+Z55)/C
      B(J)=(Z2*PS2(I,J)+Z7*(PS2(I+1,J)-2.*PS2(I,J)+PS2(I-1,J))-&
        Z8*ZP(I,J)*(PS2(I+1,J-1)-PS2(I+1,J+1)-PS2(I-1,J-1)+PS2(I-1,J+1))&
        -W2(I,J)+B(J-1)*CCC)/C
   33 CONTINUE
      XX(I,MM)=0.
      XX(I,1)=PSISURF
      DO 34 J=MM1,2,-1
   34 XX(I,J)=A(J)*XX(I,J+1)+B(J)
   32 CONTINUE
! =====================================================================
! =====================================================================
 402 MET=5
! =====================================================================
! =====================================================================



! =====================================================================
! ======== STREAM FUNCTION - HORIZONTAL ===============================
! =====================================================================
      DO 36 J=2,MM1
      A(NN)=0.
      B(NN)=PSISURF*(1.-(J-1)*DZ)
      DO 37 I=NN1,2,-1
      DPSI2=XX(I,J+1)-2.*XX(I,J)+XX(I,J-1)
      C=Z2+(2.-A(I+1))*Z7
      A(I)=Z7/C
      B(I)=(Z2*XX(I,J)-&
        Z8*ZP(I,J)*(PS0(I+1,J-1)-PS0(I+1,J+1)-PS0(I-1,J-1)+PS0(I-1,J+1))&
       +Z9*ZP(I,J)*ZP(I,J)*DPSI2-&
        Z6*ZPQ(I,J)*(XX(I,J+1)-XX(I,J-1))+Z5*DPSI2*R(I)*R(I)-W2(I,J)+&
        Z7*B(I+1))/C
   37 CONTINUE
!      PS2(1,J)=(4.*B(2)-B(2)*A(3)-B(3))/(3.-4.*A(2)+A(2)*A(3))
      PS2(1,J)=PSISURF*(1.-(J-1)*DZ) 
      DO 38 I=2,NN1
   38 PS2(I,J)=A(I)*PS2(I-1,J)+B(I)
   36 CONTINUE
      DO 725 I=1,NN 
      PS2(I,1)=PSISURF
      PS2(I,MM)=0.                            
  725 CONTINUE
      DO 94 J=1,MM
      PS2(1,J)=PSISURF*(1.-(J-1)*DZ) 
      PS2(NN,J)=PSISURF*(1.-(J-1)*DZ)
   94 CONTINUE

!  430 CONTINUE
!  330 CONTINUE
! ======================================================================
! ========= PRILIPANIE NA DNE ==========================================
! ======================================================================
      IF(NBOTFR.EQ.0) GOTO 401
      DO 410 I=1,NN
      PS2(I,MM-1)=PS2(I,MM-2)/4.
  410 CONTINUE
  
  401 MET=4   


! =====================================================================
! ========         END ITERRATION      ================================
! =====================================================================
! =====================================================================
! ======== CRITERION OF END ITERRATION ================================
! =====================================================================
      EPS1=0.
      DO 39 I=2,NN1
      DO 39 J=2,MM1
      IF(ABS(PS0(I,J)).LE.0.00001) GO TO 39
      DPSI=ABS((PS2(I,J)-PS0(I,J))/PS0(I,J))
      IF(DPSI-EPS1) 39,39,41
   41 EPS1=DPSI
      IM=I
      JM=J
   39 CONTINUE
      IF(IT.GT.100) GO TO 40
      IF(EPS1.GT.EPS) GO TO 30
   40 MET=2


! ======================================================================
      WRITE(*,305) K,IT,IM,JM,EPS1
  305 FORMAT(10X,3HKT=,I4,2X,3HIT=,I4,2X,3HIM=,I4,2X,3HJM=,I4,2X,5HEPS1=,E11.4)
! =====================================================================
! ======== DENSITY- VERTICAL ==========================================
! =====================================================================
!      WRITE(*,*) '1'


! =====================================================================
! ======== DEFINITION OF FULL DENSITY FIELD  ==========================
! =====================================================================
      DO 95 I=1,NN
      DO 95 J=1,MM
      R2(I,J)=(DENS0*R2(I,J)+(DENS(I,J)-1000.))/DENS0
   95 CONTINUE
! =====================================================================
! ======== DENSITY- VERTICAL ==========================================
! =====================================================================
      DO 99 I=1,NN1
      DO 99 J=1,MM1
   99 R0(I,J)=R2(I,J)
! =====================================================================
! ======== ALL TERMS WITH CR0 WERE REMOVED FROM THE SCHEME (fool density)
! =====================================================================
      DO 51 I=2,NN1
      A(1)=0.
      B(1)=(DENS(I,1)-1000.)/DENS0
      DO 52 J=2,MM1
      Z31=Z12*(PS2(I+1,J)-PS2(I-1,J))
      DPSI=PS2(I,J+1)-PS2(I,J-1)
      DPSI2=PS2(I+1,J)-PS2(I-1,J)
      Z31RZ20=Z31*R(I)+Z20(I,J)*ZPQ(I,J)
      Z21ZPZ40=Z21(I,J)*ZP(I,J)*ZP(I,J)+Z40(I,J)*R(I)*R(I)

      C=Z1+2.*Z21ZPZ40-A(J-1)*(Z31RZ20+Z21ZPZ40)

      A(J)=-(Z31RZ20-Z21ZPZ40)/C

      B(J)=(Z1*R2(I,J)+Z12*R(I)*DPSI*(R2(I+1,J)-R2(I-1,J))+&
           Z22(I,J)*(R2(I+1,J)-2.*R2(I,J)+R2(I-1,J))-&
           Z33(I,J)*ZP(I,J)*(R2(I+1,J-1)-R2(I+1,J+1)-&
           R2(I-1,J-1)+R2(I-1,J+1))+&
           B(J-1)*(Z31RZ20+Z21ZPZ40))/C

   52 CONTINUE
      XX(I,MM)=(DENS(I,MM)-1000.)/DENS0
      XX(I,1)=(DENS(I,1)-1000.)/DENS0
      DO 53 J=MM1,2,-1
   53 XX(I,J)=A(J)*XX(I,J+1)+B(J)
   51 CONTINUE

! =====================================================================
! ======== DENSITY- HORIZONTAL ========================================
! =====================================================================
      DO 54 J=2,MM1
      A(NN)=0.
      B(NN)=(DENS(NN,J)-1000.)/DENS0
      DO 55 I=NN1,2,-1
      Z32=Z12*(PS2(I,J+1)-PS2(I,J-1))
      DPSI=PS2(I+1,J)-PS2(I-1,J)
      DPSI2=PS2(I,J+1)-PS2(I,J-1)
      Z32RZ22=-R(I)*Z32-Z22(I,J)

      C=Z1+2.*Z22(I,J)+A(I+1)*Z32RZ22

      A(I)=(-R(I)*Z32+Z22(I,J))/C

      B(I)=(Z1*XX(I,J)-Z12*R(I)*DPSI*(XX(I,J+1)-XX(I,J-1))-&
           Z33(I,J)*ZP(I,J)*(R0(I+1,J-1)-R0(I+1,J+1)-&
           R0(I-1,J-1)+R0(I-1,J+1))+&
           Z21(I,J)*(XX(I,J+1)-2.*XX(I,J)+XX(I,J-1))*ZP(I,J)**2-&
           Z20(I,J)*(XX(I,J+1)-XX(I,J-1))*ZPQ(I,J)+&
           Z40(I,J)*(XX(I,J+1)-2.*XX(I,J)+XX(I,J-1))*R(I)**2-&
           B(I+1)*Z32RZ22)/C
   55 CONTINUE

!      R2(1,J)=(4.*B(2)-B(2)*A(3)-B(3))/(3.-4.*A(2)+A(2)*A(3))
      R2(1,J)=(DENS(1,J)-1000.)/DENS0
      R2(NN,J)=(DENS(NN,J)-1000.)/DENS0

      DO 57 I=2,NN1
      R2(I,J)=A(I)*R2(I-1,J)+B(I)
   57 CONTINUE

   54 CONTINUE

      DO 625 I=1,NN 
      R2(I,1)=(DENS(I,1)-1000.)/DENS0
      R2(I,MM)=(DENS(I,MM)-1000.)/DENS0                            
  625 CONTINUE
      DO 194 J=1,MM
      R2(1,J)=(DENS(1,J)-1000.)/DENS0
      R2(NN,J)=(DENS(NN,J)-1000.)/DENS0
   194 CONTINUE

! =====================================================================
! ======== RETURN TO THE DENSITY DISTURBANCES  ========================
! =====================================================================
      DO 594 I=1,NN
      DO 594 J=1,MM
      R2(I,J)=(DENS0*R2(I,J)-(DENS(I,J)-1000.))/DENS0
  594 CONTINUE




! =====================================================================
! ======== PRINTING BLOCK =============================================
! =====================================================================
      BK=K
      BKPR=KPRVEL
      KP=INT(BK/BKPR)
      IF(K-KP*KPRVEL) 150,80,150
   80 TT=K*DT
      TBEG1=TBEG1+KPRVEL*DT

! ======================================================================
! ======= PRINTING OF THE RICHARDSON NUMBER  ===========================
! ======================================================================
      RIMAK=0.
      RIMIN=0.
      DO 677 I=1,NN
      DO 677 J=1,MM
      IF(RI(I,J).GT.RIMAK) RIMAK=RI(I,J)
      IF(RI(I,J).LT.RIMIN) RIMIN=RI(I,J)
  677 CONTINUE
      RICH='richards.grd'
      CALL PRINTGRD(RICH,RI,RIMIN,RIMAK)

! ======================================================================
! ======= PRINTING OF THE NONBOUSSINESQ TERM F(I,J) ====================
! ======================================================================
      FMAK=0.
      FMIN=0.
      DO 627 I=1,NN
      DO 627 J=1,MM
      IF(F(I,J).GT.FMAK) FMAK=F(I,J)
      IF(F(I,J).LT.FMIN) FMIN=F(I,J)
  627 CONTINUE
!     BUSS='bussines.grd'
!      CALL PRINTGRD(BUSS,F,FMIN,FMAK)

      rxmax=0.
      rxmin=0.
      DO 623 I=1,NN
      DO 623 J=1,MM
      IF(r2x(I,J).GT.rxmax) rxmax=r2x(I,J)
      IF(r2x(I,J).LT.rxmin) rxmin=r2x(I,J)
  623 CONTINUE
!     rxx='rx-00000.grd'
!      CALL PRINTGRD(rxx,r2x,rxmax,rxmin)


! ======================================================================
! DEFINITION OF ARRAY DENS(I,J) IN THE NEW VARIABLES OVER THE BOTTOM ==
! ======================================================================
!      DO 175 I=1,NN      
!      DENS(I,1)=1000.
!      DO 75 J=2,MM
!      GOR1=(J-1)*H0/(MM-1)/R(I)
!      IF(GOR1.LT.HUPPER) DENS(I,J)=1000. 
!      IF(GOR1.LT.HUPPER) GOTO 75 
!      XXX=0.
!      DO 196 KK=1,5
!      XXX=XXX+((-BJ(K)-HUPPER)/(AJ(K)*(BJ(K)+HUPPER)**2+CJ(K))-&
!               (-BJ(K)-GOR1)/(AJ(K)*(BJ(K)+GOR1)**2+CJ(K))-&
!               SQRT(1./AJ(K)/CJ(K))*&
!              (ATAN(SQRT(AJ(K)/CJ(K))*(-BJ(K)-GOR1))-&
!               ATAN(SQRT(AJ(K)/CJ(K))*(-BJ(K)-HUPPER))))/CJ(K)               
!  196 CONTINUE
!      DENS(I,J)=1000.+0.5*1000.*XXX/9.81
!   75 CONTINUE
!  175 CONTINUE 
!      DO 177 I=1,NN
!      DO 177 J=1,MM
!      IF(DENS(I,J).GT.DMAK) DMAK=DENS(I,J)
!      IF(DENS(I,J).LT.DMIN) DMIN=DENS(I,J)
!  177 CONTINUE
!      DENSITY='dens(xz).grd'
!      CALL PRINTGRD(DENSITY,DENS,DMIN,DMAK)

! ======================================================================
! DEFINITION OF U(I,J), W(I,J) AND DENSITY FIELDS  OVER THE BOTTOM =====
! ======================================================================      
      DO 78 I=1,NN
      DO 78 J=1,MM
      PS0(I,J)=0.
      XX(I,J)=0.
      U(I,J)=0.
      W(I,J)=0.
   78 CONTINUE
      
! =======================================================================
! ======== DEFINITION OF FOOL DENSITY FIELD  (only for writing GRD file)=
! =======================================================================
      DO 195 I=1,NN
      DO 195 J=1,MM
!      XX(I,J)=DENS0*R2(I,J)+(DENS(I,J)-1000.)
      XX(I,J)=DENS0*R2(I,J)+(DENS(I,J)-1000.)
  195 CONTINUE

      DO 79 I=1,NN
      DH=DZ*H0/R(I)
      DO 79 J=1,MM
      GOR=DELH0*(J-1)
      J2=INT(GOR/DH)+1
      IF(J2+1.GT.MM) GOTO 79
      DDZ=GOR-(J2-1)*DH
      PS0(I,J)=PS2(I,J2)+(PS2(I,J2+1)-PS2(I,J2))*DDZ/DH
      XX(I,J)=XX(I,J2)+(XX(I,J2+1)-XX(I,J2))*DDZ/DH
      V(I,J)=V2(I,J2)+(V2(I,J2+1)-V2(I,J2))*DDZ/DH
   79 CONTINUE

! ----U-SPEED------------------------------------
      DO 82 I=1,NN
      DO 82 J=2,MM
      U(I,J)=100.*PSI0*(PS0(I,J-1)-PS0(I,J))/DELH0
   82 CONTINUE
      DO 882 I=1,NN
  882 U(I,1)=U(I,2)
! ----V-SPEED------------------------------------
      DO 382 I=1,NN
      DO 382 J=1,MM
      V(I,J)=100.*VSCALE*V(I,J)
  382 CONTINUE
! ----W-SPEED-------------------------------------
      DO 86 J=1,MM
      DO 86 I=1,NN1
      W(I,J)=-100.*PSI0*(PS0(I+1,J)-PS0(I,J))/DX/AL
   86 CONTINUE
      DO 886 J=1,MM 
  886 W(NN,J)=W(NN1,J)
! =====================================================================
! ===========DEFINITION OF KINETIC ENERGY (J/M)========================
! =====================================================================
      UMAK=0.
      UMIN=0.
      VMIN=0.
      VMAK=0.
      WMAK=0.
      WMIN=0.
      ROMAK=0.
      ROMIN=0.
      DO 84 I=1,NN
      DO 84 J=1,MM
      IF(XX(I,J).GT.ROMAK) ROMAK=XX(I,J)
      IF(XX(I,J).LT.ROMIN) ROMIN=XX(I,J)
   84 CONTINUE
      DO 85 I=1,NN
      DO 85 J=1,MM
      IF(U(I,J).GT.UMAK) UMAK=U(I,J)
      IF(U(I,J).LT.UMIN) UMIN=U(I,J)
   85 CONTINUE
      DO 684 I=1,NN
      DO 684 J=1,MM
      IF(V(I,J).GT.VMAK) VMAK=V(I,J)
      IF(V(I,J).LT.VMIN) VMIN=V(I,J)
  684 CONTINUE
      DO 87 I=2,NN1
      DO 87 J=2,MM1
      IF(W(I,J).GT.WMAK) WMAK=W(I,J)
      IF(W(I,J).LT.WMIN) WMIN=W(I,J)
   87 CONTINUE
      DO 184 I=1,NN
      DO 184 J=1,MM
      U2CUR=U(I,J)**2
      W2CUR=W(I,J)**2
      IF(U2CUR.GT.U2max(I,J)) U2max(I,J)=U2CUR
      IF(W2CUR.GT.W2max(I,J)) W2max(I,J)=W2CUR
  184 CONTINUE
! =====================================================================
! =====================================================================
      U2MAK=0.
      U2MIN=0.
      W2MAK=0.
      W2MIN=0.
      DO 185 I=1,NN
      DO 185 J=1,MM
      IF(U2max(I,J).GT.U2MAK) U2MAK=U2max(I,J)
      IF(W2MAX(I,J).GT.W2MAK) W2MAK=W2max(I,J) 
  185 CONTINUE
  
! =====================================================================
! =====================================================================
! =====================================================================
  150 BK=K
      BKPRGRD=KPRGRD
      KPGRD=INT(BK/BKPRGRD)
      IF(K-KPGRD*KPRGRD) 9,789,9
  789 MET=1
      TBEG2=TBEG2+KPRGRD*DT
      LETA='a'
      LETU='u'
      LETW='w'
      LETRPQ='h'
      CALL NAMEDAT(TBEG2,AMPCENTR,LETA)
      CALL NAMEDAT(TBEG2,UCENTR,LETU)
      CALL NAMEDAT(TBEG2,WCENTR,LETW)
      CALL NAMEDAT(TBEG2,RPQ,LETRPQ)
      CALL NAMEGRD(TBEG2,UU,VV,WW,RR)
      CALL PRINTGRD(UU,U,UMIN,UMAK)
      CALL PRINTGRD(VV,V,VMIN,VMAK)
      CALL PRINTGRD(WW,W,WMIN,WMAK)
      CALL PRINTGRD(RR,XX,ROMIN,ROMAK)
      U2AMPL='MAXIM_U2.grd'
      W2AMPL='MAXIM_W2.grd'
      CALL PRINTGRD(U2AMPL,U2max,U2MIN,U2MAK)
      CALL PRINTGRD(W2AMPL,W2max,W2MIN,W2MAK)
      CALL PRINTDAT(TBEG2)
!     TTT='TIME='
    9 CONTINUE
! 222 MET=1
!      CALL PRINTDAT(TBEG2)      
      PRINT 122
  122 FORMAT(//10X,'H A P P Y    E N D ,   B A B Y ! ! !')
      STOP
      END



      SUBROUTINE NAMEGRD(TBEG,UU,VV,WW,RR) 
      CHARACTER*1 TIME1,TIME4
      CHARACTER*2 TIME3,TIME5
      CHARACTER*3 TIME2,TIME6
      CHARACTER*12 UU,VV,WW*12,RR*12
      temp=anint(tbeg*1000.)
      kt1=int(temp/1000.)
      if(kt1.lt.10) write(time1,124) kt1
  124 FORMAT(I1)
      if(kt1.ge.10.and.kt1.lt.100) write(time3,424) kt1
  424 format(i2)
      if(kt1.ge.100) write(time6,524) kt1
  524 FORMAT(I3)
      kt2=int(temp-aint(kt1*1000.))
      IF(KT2-0) 120,121,120
  121 TIME2='000'
      GOTO 127
  120 if(kt2.lt.10.and.kt2.gt.0) write(time4,124) kt2
      if(kt2.lt.10.and.kt2.gt.0) time2='00'//time4
      if(kt2.ge.10.and.kt2.lt.100) write(time5,424) kt2
      if(kt2.ge.10.and.kt2.lt.100) time2='0'//time5
      if(kt2.ge.100) write(time2,125) kt2
  125 FORMAT(I3)
  127 MET=1
      IF(KT1.LT.10) GOTO 148
      IF(KT1.GE.10.AND.KT1.LT.100) GOTO 149
      IF(KT1.GE.100) GOTO 777
  148 uu='u0'//time1//'_'//time2//'t.grd'
      vv='v0'//time1//'_'//time2//'t.grd'
      ww='w0'//time1//'_'//time2//'t.grd'
      rr='r0'//time1//'_'//time2//'t.grd'
      GOTO 150
  149 uu='u'//time3//'_'//time2//'t.grd'
      vv='v'//time3//'_'//time2//'t.grd'
      ww='w'//time3//'_'//time2//'t.grd'
      rr='r'//time3//'_'//time2//'t.grd'
      GOTO 150
  777 uu='u'//time6//'_'//time2//'.grd'
      vv='v'//time6//'_'//time2//'.grd'
      ww='w'//time6//'_'//time2//'.grd'
      rr='r'//time6//'_'//time2//'.grd'
  150 MET=1    
      RETURN
      END

    
      SUBROUTINE NAMEDAT(TBEG,AMPCENTR,FIRSTLET) 
      CHARACTER*1 TIME1,TIME4,FIRSTLET
      CHARACTER*2 TIME3,TIME5
      CHARACTER*3 TIME2,TIME6
      CHARACTER*12 AMPCENTR
      temp=anint(tbeg*1000.)
      kt1=int(temp/1000.)
      if(kt1.lt.10) write(time1,124) kt1
  124 FORMAT(I1)
      if(kt1.ge.10.and.kt1.lt.100) write(time3,424) kt1
  424 format(i2)
      if(kt1.ge.100) write(time6,524) kt1
  524 FORMAT(I3)
      kt2=int(temp-aint(kt1*1000.))
      IF(KT2-0) 120,121,120
  121 TIME2='000'
      GOTO 127
  120 if(kt2.lt.10.and.kt2.gt.0) write(time4,124) kt2
      if(kt2.lt.10.and.kt2.gt.0) time2='00'//time4
      if(kt2.ge.10.and.kt2.lt.100) write(time5,424) kt2
      if(kt2.ge.10.and.kt2.lt.100) time2='0'//time5
      if(kt2.ge.100) write(time2,125) kt2
  125 FORMAT(I3)
  127 MET=1
      IF(KT1.LT.10) GOTO 148
      IF(KT1.GE.10.AND.KT1.LT.100) GOTO 149
      IF(KT1.GE.100) GOTO 777
  148 AMPCENTR=FIRSTLET//'0'//time1//'_'//time2//'T.DAT'
      GOTO 150
  149 AMPCENTR=FIRSTLET//time3//'_'//time2//'T.DAT'
      GOTO 150
  777 AMPCENTR=FIRSTLET//time6//'_'//time2//'.DAT'
  150 MET=1    
      RETURN
      END


      SUBROUTINE PRINTDAT(TBEG)
      PARAMETER(MM=451,NN=10001)
      COMMON /D/R2(NN,MM),W2(NN,MM),PS2(NN,MM),V2(NN,MM)
      COMMON /C/DIR
!     CHARACTER*28 DIR
      CHARACTER*22 DIR
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
      RETURN
      END
    
   
      SUBROUTINE PRINTGRD(NAME,A,VMIN,VMAK)
      PARAMETER(NN=10001,MM=451)
      DIMENSION A(NN,MM)
      COMMON /B/KGRD1,KGRD2,KGRDSTEP,KPRGRDZ
!      COMMON /B/KGRDSTEP
      COMMON /C/DIR
!      CHARACTER*28 DIR
      CHARACTER*22 DIR
      CHARACTER*12 NAME
      NK=INT((KGRD2-KGRD1)/KGRDSTEP)+1
      MK=INT((MM-1)/KPRGRDZ)+1
    1 FORMAT('DSAA'/I5,I5/'1  ',I5/'1  ',I5/2F11.5)
    2 FORMAT(7F11.5)
      OPEN(11,FILE=DIR//'OUTPUT.GRD\'//NAME)
      WRITE(11,1) NK,MK,NK,MK,VMIN,VMAK
      WRITE(11,2) ((A(I,J),I=KGRD2,KGRD1,-KGRDSTEP),J=MM,1,-KPRGRDZ)
      CLOSE(11)
      WRITE(*,110) NAME
  110 FORMAT(10X,A12,' FIELD WAS WRITTEN IN OUTPUT.GRD')
      CLOSE(11)
      RETURN
      END
  
