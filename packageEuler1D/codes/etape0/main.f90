Module Precisions
integer, PARAMETER::DP=KIND(1.d0)
END MODULE PRECISIONS
!c======================================================================
!c======================================================================
 MODULE dimension_probleme
integer,parameter::Nface=4  !!
END MODULE dimension_probleme
!c=============================================================
!c======================================================================
!c======================================================================
 MODULE PARAMETRE_EOS
USE precisions
USE dimension_probleme

   INTEGER,parameter      :: NPARA_MAX=20
        REAL(KIND=DP),dimension(NPARA_MAX),save :: PARA_EOS

END MODULE PARAMETRE_EOS
!c======================================================================
!c======================================================================
 MODULE COURANT
   use precisions
   use dimension_probleme
   type cellule_gaz
   real(kind=dp)::ro
   real(kind=dp)::u
   real(kind=dp)::P
   real(kind=dp)::C
   real(kind=dp)::T
       end type cellule_gaz

 END MODULE COURANT
!c======================================================================
!c======================================================================
    MODULE GEOMETRIE
    use precisions
    use dimension_probleme

 type maillage
 real(kind=dp)   ::x,volume
 real(kind=dp),dimension(NFACE)::aire_face,cosn
  end type maillage

type definition_domaine
integer,dimension(4)::type_limite !!
end type  definition_domaine


 END MODULE GEOMETRIE

!===============================================================================
!===============================================================================
                PROGRAM PEGASE
!===============================================================================
!===============================================================================
use precisions
use dimension_probleme
         use PARAMETRE_EOS
         use COURANT
 use geometrie

implicit none
INTEGER NEQ,NX,NXY

! MESH
type(maillage),dimension(:),allocatable::grid
type(definition_domaine)    ::def_bloc
REAL(KIND=DP) Long_Physique
! METHODE
    real(kind=dp), allocatable,dimension(:,:)    ::VAR
real(kind=dp), allocatable,dimension(:)     :: FLUX,SOMFLUX
real(kind=dp), allocatable,dimension(:,:,:) :: VARI
real(kind=dp), allocatable,dimension(:,:,:) :: VARP

!  GAZ
type(cellule_gaz),dimension(:),allocatable::cell_gaz

! DIVERS
real(kind=dp)::CFL,DT,DTCFL
real(kind=dp)::DXS2
! Calculde l'atténuation du choc
REAL*8 Xchoc_init,Pchoc_init

!---------------------------------------------------------------------------
!     DECLARATION AUTRE QUE DES TABLEAUX
!---------------------------------------------------------------------------
! DIVERS
INTEGER KPRINT
INTEGER IFILE,NCOURBE,I,K,IEQ,KT,KMAX
INTEGER IFACE,KINSTA
INTEGER NPARAMETRE
REAL(kind=dp)  PI,TEMPS,DT0,masse
! position onde de choc
real(kind=dp) ::XODC
INTEGER ::IODC
CHARACTER*16 FILE1,FILE2

     PI=DACOS(-1.D0)

!C--------------------------------------------------------------------
!C 		 LECTURE DU FICHIER 'DATAINIT'
!C--------------------------------------------------------------------

    OPEN (UNIT=110 , FILE='DATAINIT.DAT' ,    STATUS='UNKNOWN')
    READ(110,*)
    READ(110,*)KMAX,KPRINT
    READ(110,*)KINSTA
    READ(110,*)
    READ(110,*)DT0,CFL
    READ(110,*)
    read(110,*)NX
    read(110,*)Long_Physique
    read(110,*)
    read(110,*)XODC
    read(110,*)
    read(110,*)def_bloc%type_limite(4),def_bloc%type_limite(2)
    read(110,*)
    read(110,*)FILE1
    read(110,*)FILE2
    CLOSE(110)

    FILE1=trim(FILE1)
    FILE2=trim(FILE2)

!=========================================================
! Variables modifiable par un utilisateur avancé
!=========================================================

IF(KINSTA.EQ.0)KINSTA=KMAX+1
     IFILE=0
     NCOURBE=0

!c======================================================================
!c	LECTURE DE LA DIMENSION  DES TABLEAUX
!c======================================================================
    NXY =MAX(NX,1)
    NEQ=4
!c======================================================================
!c				ALLOCATION DES TABLEAUX
!c======================================================================
!c general
  allocate ( cell_gaz(0:NX+1) )

!c maillage
allocate ( grid(0:NX+1)    )
!c schéma
allocate(                   &
             VAR(0:NX+1,NEQ),    &
             FLUX(NEQ),&
             SOMFLUX(NEQ)&
             )
!
    allocate(                    &
        VARP (0:NX+1,4,NEQ),     &
         VARI    (0:NX+1,4,NEQ)  &
               )


!c======================================================================
!c AFFECTAION DU PAS DE TEMPS LOCAL
  IF(CFL.eq.0.) DT=DT0

!C--------------------------------------------------------------------
!C 	ECRITURE DES DONNEES DE CONTROLES
!C--------------------------------------------------------------------

WRITE(*,*)
WRITE(*,*)' ------------------------------------------'
WRITE(*,*)'    Ecriture DES DONNEES DE CONTROLES'
WRITE(*,*)'  NBRE D''ITERATION MAX',KMAX
WRITE(*,*)'  ECRITURE DES FICHIERS PLOT TOUS LES',KPRINT
WRITE(*,*)
WRITE(*,*)'   METHODE DE CALCUL A L''ORDRE 1'
WRITE(*,*)'   METHODE DE CALCUL A L''ORDRE 2'
WRITE(*,*)
WRITE(*,*)
IF(CFL.EQ.0.D0)WRITE(*,*)'PAS DE TEMPS FIXE'
IF(CFL.EQ.0.D0)WRITE(*,*)'DT =',DT0
IF(CFL.NE.0.D0)WRITE(*,*)'PAS DE TEMPS FIXE PAR RAPPORT AU CFL'
IF(CFL.NE.0.D0)WRITE(*,*)'CFL =',CFL

!----------------------------------------------------------------------
!               OUVERTURE DES FICHIERS
!----------------------------------------------------------------------

 OPEN (UNIT=12,FILE=FILE1,STATUS='UNKNOWN')
!----------------------------------------------------------------------
!			LECTURE DE LA GEOMETRIE
!-----------------------------------------------------------------------

write(*,*)
write(*,*)'	---------------------------------------------------'
write(*,*)'		on cree les maillages'
write(*,*)'	---------------------------------------------------'

       CALL GENMESH(grid,Long_Physique, NX)


!C position ODC
iODC=1
dxs2=grid(nx)%x
  do i=1,nx
   if(dabs(grid(i)%x-XODC).le. dxs2) then
   dxs2=dabs(grid(i)%x-XODC)
   iODC=i
    end if
  end do

!-----------------------------------------------------------------------
!      INITIALISATION DES GRANDEURS THERMOPHYSIQUES DES PHASES
!-----------------------------------------------------------------------
! DU GAZ
      PARA_EOS  = 0.d0
    write(*,*)'====================================================='
write(*,*)' Grandeurs Thermophysiques des Phases (lecture)'
write(*,*)' on ouvre le fichier  ''air_gaz.inp'' '
      OPEN (UNIT=90,FILE='air_gaz.inp', STATUS='OLD')

REAd(90,*)
READ(90,*)NPARAMETRE
  Do K=1,NPARAMETRE
   READ(90,*)PARA_EOS(K)
  END DO
close(90)
      write(*,*)'===================================================='
      TEMPS=0.d0
!-----------------------------------------------------------------------
!************  INITIALISATION DES GRANDEURS CONSERVATIVES***************
!-----------------------------------------------------------------------
!SOLUTION INITIALE GAZ

  CALL INITGAZ(VAR,Pchoc_init,IODC,NX,NEQ)

Xchoc_init =grid(iODC)%x
write(*,*)'======================================================'
write(*,*)' La position de l''onde de choc '
write(*,*)' XODC =',XODC,'(m)  soit l''indice ',IODC
write(*,*)
write(*,*)'======================================================'
WRITE(*,*)'TEMPS INITIAL', TEMPS

!---------------------------------------------------------------------
!	INITIALSIATION DES DIFFERENTS VECTEURS
!---------------------------------------------------------------------


 DO I=0,NX+1
 DO IEQ=1,NEQ
 DO K=1,4
  VARI(I,K,IEQ)=VAR(I,IEQ)
  varp(i,K,IEQ)=VAR(I,IEQ)
 END DO
 END DO
 END DO

!-----------------------------------------------------------------------
!	CALCUL DES CONDITIONS AUX LIMITES ANANt la premiere inetration
!-----------------------------------------------------------------------

         CALL LIMGAZ(def_bloc,VAR,NX,NEQ)

!C----------------------------------------------------------------------
!C	ON CALCULE LES GRANDEURS PRIMITIVES
!C---------o------------------------------------------------------------

CALL PRIM(cell_gaz,VAR,NX,NEQ)
CALL  ECRITPLOT(cell_gaz,grid,FILE2,IFILE,NX)
NCOURBE = NCOURBE +1
!C*********************************************************************
!C---------------------------------------------------------------------
!C ******************** DEBUT DE LA BOUCLE SUR LE TEMPS ***************
!C---------------------------------------------------------------------
!C*********************************************************************
          DO 999 KT=1,KMAX
!C----------------------------------------------------------------------
!C		CALCUL DES CONDITIONS AUX LIMITES
!C----------------------------------------------------------------------
          CALL LIMGAZ(def_bloc,VAR,NX,NEQ)

!C----------------------------------------------------------------------
!C	 calcul du pas de temps
!C----------------------------------------------------------------------

   IF(CFL.NE.0.D0) THEN
 DTCFL=9.d9
    DO I=1,NX
 DTCFL=DMIN1(DTCFL,grid(I)%volume/(dabs(cell_gaz(i)%U) +cell_gaz(i)%c))
    END DO
 DT =CFL* DTCFL
    END IF
TEMPS = TEMPS + DT

CALL AFFICHE_TEMPS(KMAX,DT,KT)

  DO  IEQ=1,NEQ
  DO  IFACE=2,4,2
  DO  I=0,NX+1
         varp(i,IFACE,IEQ)=VAR(I,IEQ)
  END DO
  END DO
  END DO

!C----------------------------------------------------------------------
!C CALCUL DES FLUX PASSANT AU TRAVERS DE CHAQUE INTERFACE
!C                  ON UTILISE LES VALEURS U(N+1/2,J+1/2)
!C----------------------------------------------------------------------

CALL INTERCELL(VARP,VARI,GRID,NX,NEQ)

!C-----------------------------------------------------------------
!C		         ETAPE DE CONSERVATION
!C-----------------------------------------------------------------

         DO  I=1,NX
         CALL GREEN_FLUX(grid,VARI,SOMFLUX,FLUX,I,NEQ,NX)

      DO IEQ=1,NEQ
       VAR(I,IEQ)=VAR(I,IEQ)+DT*(SOMFLUX(IEQ))/grid(i)%volume
      ENDDO

        END DO

!c--------------------------------------------------------------
!c       	CALCUL DES GRANDEURS PRIMTIVES  A LA FIN DE L ETAPE
!c--------------------------------------------------------------

CALL PRIM(cell_gaz,VAR,NX,NEQ)

!c-----------------------------------------------------------------
!C		ECRITURE DU FICHIER insta (12)
!c-----------------------------------------------------------------

        IF((MOD(KT,KINSTA).EQ.0).AND.(KINSTA.NE.0)) THEN

!c ecriture sur le fichier 'insta' et calcul du débit
     masse=0.0d0
   do i=1,nx
       masse =masse + VAR(I,4)*grid(i)%volume
   end do
       WRITE(12,*)TEMPS/1.d-3, masse

      end if ! FIN ECRITURE DU FICHIER insta
!c-----------------------------------------------------------------
!C		ECRITURE DU FICHIER FILE2  de type Grandeur_f=f(x,t_fixé)
!c-----------------------------------------------------------------
    IF(KPRINT.NE.0.AND.MOD(KT,KPRINT).EQ.0.and.kt.ge.0)THEN
NCOURBE=NCOURBE+1
CALL  ECRITPLOT(cell_gaz,grid,FILE2,IFILE,NX)

WRITE(*,*)' ON A ECRIT LE FICHIER RESULTAT N.',NCOURBE
     END IF

!c-----------------------------------------------------------------
!C		               FIN DU PAS DE TEMPS.
!c-----------------------------------------------------------------
999     CONTINUE


 write(*,*)'temps final',TEMPS

 deallocate(cell_gaz, grid, VAR,  FLUX,   &
             SOMFLUX,VARP ,VARI )


                       END PROGRAM

!=======================================================================
SUBROUTINE INITGAZ(VAR,Pchoc_init,ichoc_init,NX,NEQ)
 use precisions
 use dimension_probleme
     use PARAMETRE_EOS
IMPLICIT NONE

INTEGER NX,NEQ,ichoc_init
REAL*8 VAR(0:NX+1,NEQ),Pchoc_init

! VARIABLE LOCALE
INTEGER I,ic_choix
INTEGER I1,I2
REAL*8 P1,RO1,U1,T1,ENER1
REAL*8 P2,RO2,U2,T2,ENER2
REAl*8 P2SURP1,UCHOC,DEBIT
REAL*8 GA,GAP1,GAM1,CV

GA   =PARA_EOS(1)
GAp1 =GA+1.d0
GAM1 =GA-1.d0
CV   =PARA_EOS(2)

write(*,*)'==================================================='
write(*,*)'  Condition INITIALE DU GAZ                       '
write(*,*)'==================================================='
!=============================================================
!        lecture du ficheir de données initiales du gaz
!=============================================================
OPEN (UNIT=111 , FILE='INITGAZ.DAT' ,    STATUS='OLD')
READ(111,*)
READ(111,*)ic_choix

          If (ic_choix == 1) THEN
!===============================================
!  ONDE DE CHOC MOBILE
!===============================================

I1=0
I2=ichoc_init
READ(111,*)RO1,P1
READ(111,*)U1
READ(111,*)
READ(111,*)P2SURP1
!=======================================================================

ENER1=p1/ro1/(ga-1.d0)
T1   =ener1/cv

!  defintion des grandeurs du choc par des relations analytiques

P2=p2surp1*p1
ro2=ro1*(p2surp1*gap1+gam1)/(p2surp1*gam1+gap1)
         if(ro2.ne.ro1) then
debit=dsqrt((p2-p1)/(1.d0/ro1-1.d0/ro2))
uchoc=(debit+ro1*u1)/ro1
ener2=P2/gam1/ro2
u2=(ro2*uchoc-debit)/ro2
       else
        debit=0.d0
        u2=0.d0
        ener2=P2/gam1/ro2
       end if


T2=ener2/CV
uchoc=(ro1*u1+debit)/ro1

write(*,*)'==================================================='
write(*,*)'  			CHOC   STATIONNAIRE         '

write(*,*)' ETAT 1 (Etat devant le choc)'
write(*,*)' rho ', ro1
write(*,*)' P ', p1
write(*,*)' u ', u1
write(*,*)' T ',T1
write(*,*)
write(*,*)' ETAT 2 (Etat derriere le choc)'
write(*,*)' rho ', ro2
write(*,*)' P ', p2
write(*,*)' u ', u2
write(*,*)' T ',T2
write(*,*)' Vitesse du choc',uchoc
Pchoc_init=P2
!=======================================================================
! initilasation de l'état choqué

     DO I=I1,I2

 VAR(I,1) =ro2*u2
 VAR(I,3) =ro2*(ENER2+0.5D0*U2*U2)
 VAR(I,4)=RO2
    END DO
! initialasation de la zone basse pression

     DO I=I2+1,NX+1
 VAR(I,1) =ro1*u1
 VAR(I,3) =ro1*(ENER1+0.5D0*U1*U1)
 VAR(I,4) =RO1
     END DO
               else
!=======================================================================
!                   ERROR
!=======================================================================
write(*,*)' Attention mauvais coix de Condition initiale ',ic_choix, ' n est pas valable'
        stop
                End If


   RETURN
   END
!=======================================================================
SUBROUTINE  AFFICHE_TEMPS(KMAX,DT,KT)
IMPLICIT NONE

INTEGER KMAX,Kt
REAL*8 DT

        IF (KMAX.LT.100) THEN
 PRINT*,'KT',KT,'DT=',DT/1.D-6,'MICRO SEC'
        ELSE
      IF (KMAX.LT.1000)THEN
      IF (MOD(KT,50).EQ.0) THEN
       PRINT*,'KT',KT,DT
      END IF
     else
     IF (MOD(KT,500).EQ.0) THEN
     PRINT*,'KT',KT,DT
      END IF
    end if
    END IF

RETURN
END

!=======================================================================

SUBROUTINE ECRITPLOT(cell_gaz,grid,FILE2,IFILE,NX)
use precisions
use COURANT
use geometrie

       IMPLICIT NONE
INTEGER NX,IFILE


type(cellule_gaz),dimension(0:NX+1)::cell_gaz
type(maillage),dimension(0:NX+1)  ::grid

!  VARIABLES LOCALES
INTEGER I,NUMFILE
CHARACTER*16  FILE2


! LECTURE ET ECRITURE
NUMFILE=60+IFILE
WRITE(*,*)' NOM DU FICHIER :',FILE2
OPEN (UNIT=NUMFILE ,FILE=FILE2 ,STATUS='UNKNOWN')

     DO I=1,NX
 WRITE(NUMFILE,300 )grid(i)%x,           &    !1
                    cell_gaz(i)%u ,      &! 2
                    cell_gaz(i)%P ,      &! 3
                    cell_gaz(i)%T,       &! 4
                    cell_gaz(i)%RO,      &! 5
                    cell_gaz(i)%C
     END DO
!	CLOSE(NUMFILE)
 WRITE(NUMFILE,* )
300 FORMAT(6E16.7)

RETURN
END

!=======================================================================
 SUBROUTINE FLUHLLC(                                   &
                    RO_U_NORMAL_L,RO_U_TANG_L,ETOT_L,RO_L,PL,AL,&
                    RO_U_NORMAL_R,RO_U_TANG_R,ETOT_R,RO_R,PR,AR,&
                    RN,RT,ENR,RO)
        IMPLICIT    NONE


REAL*8  RO_U_NORMAL_L, RO_U_TANG_L
REAL*8  RO_U_NORMAL_R, RO_U_TANG_R
REAL*8  RO_L,          RO_R
REAL*8  ETOT_L,        ETOT_R

REAL*8  RO,RN,RT,ENR

! VARIABLES LOCALES

REAL*8  SM,SR,SL
REAL*8  UL,VL,PL,AL
REAL*8  UR,VR,PR,AR
REAL*8  FACT
REAL*8  FLU_L(4),FLU_R(4),UU_L(4),UU_R(4)

!      INITIALISATION des vitesses ondes de Droite et Gauche
!      Etat gauche
UL = RO_U_NORMAL_L / RO_L
VL = RO_U_TANG_L   / RO_L


!      Etat droite
          UR = RO_U_NORMAL_R / RO_R
  VR = RO_U_TANG_R   / RO_R

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!      CALCUL DES VITESSES DES ONDES A DROITE ET A GAUCHE
SL = DMIN1(UL-AL  ,  UR-AR)
SR = DMAX1(UL+AL  ,  UR+AR)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!                       CALCUL DE LA VITESSE SM

SM=(PR-PL + RO_L*UL*(SL-UL)-RO_R*UR*(SR-UR))/  &
         (RO_L*(SL-UL)-RO_R*(SR-UR))
!______________________________________________________________
!			AFFECTATION DES VECTEURS
!______________________________________________________________
!  grandeurs conservatives à gauche
    UU_L(1) = RO_L
    UU_L(2) = RO_U_NORMAL_L
    UU_L(3) = RO_U_TANG_L
    UU_L(4) = ETOT_L



!  Flux a agauche
    FLU_L(1) = RO_U_NORMAL_L
    FLU_L(2) = RO_U_NORMAL_L  * UL  +  PL
    FLU_L(3) = RO_U_TANG_L    * UL
    FLU_L(4) =(ETOT_L  + PL)  * UL



!  grandeurs conservatives à droite
    UU_R(1) = RO_R
    UU_R(2) = RO_U_NORMAL_R
    UU_R(3) = RO_U_TANG_R
    UU_R(4) = ETOT_R



!  Flux a droite
    FLU_R(1) = RO_U_NORMAL_R
    FLU_R(2) = RO_U_NORMAL_R  * UR  +  PR
     FLU_R(3) = RO_U_TANG_R    * UR
     FLU_R(4) =(ETOT_R  + PR)  * UR



!_____________________________________________________________
! CALCUL DE LA SOLUTION en TERME DE FLUX, au bord de la maille (x/t = 0)
!_____________________________________________________________
           IF(SL.GE.0.D0) THEN
        RO=   FLU_L(1)
        RN=   FLU_L(2)
        RT=   FLU_L(3)
        ENR=  FLU_L(4)
           END IF
!--------------------
       IF(SR.LE.0.D0) THEN
        RO= FLU_R(1)
        RN= FLU_R(2)
        RT= FLU_R(3)
        ENR=FLU_R(4)
       END IF
!--------------------
      IF(SL.LT.0.D0.AND.SM.GT.0.D0) THEN

 FACT=RO_L*(SL-UL)/(SL-SM)
 ENR = ETOT_L / RO_L + (SM-UL) *(SM + PL/RO_L/(SL-UL) )

            RO= FLU_L(1) + SL*( FACT     - UU_L(1) )
            RN= FLU_L(2) + SL*( FACT*SM  - UU_L(2) )
            RT= FLU_L(3) + SL*( FACT*VL  - UU_L(3) )
            ENR=FLU_L(4) + SL*( FACT*ENR - UU_L(4) )


          END IF
!--------------------
         IF(SR.GT.0.D0.AND.SM.LE.0.D0) THEN
    FACT=RO_R*(SR-UR)/(SR-SM)
    ENR = ETOT_R / RO_R + (SM-UR) *(SM + PR/RO_R/(SR-UR) )
        RO= FLU_R(1) + SR*( FACT     - UU_R(1) )
        RN= FLU_R(2) + SR*( FACT*SM  - UU_R(2) )
        RT= FLU_R(3) + SR*( FACT*VR  - UU_R(3) )
        ENR=FLU_R(4) + SR*( FACT*ENR - UU_R(4) )

         END IF

RETURN
      END

!=======================================================================
       SUBROUTINE GENMESH(grid,Long_Physique,NX)

use precisions
!	use COURANT
use dimension_probleme
use geometrie

               IMPLICIT NONE
INTEGER  NX


 type(maillage),dimension(0:NX+1)   ::grid
 real(kind=dp) Long_Physique

! VARIABLE LOCALE

INTEGER I,L
INTEGER LIMITE
REAL(Kind=DP) ::DELTAX,X1,X2

!------------------------------------------------------------------
!	Création des conditions aux limites de chaque sous domaine
!------------------------------------------------------------------
deltax=Long_Physique/dfloat(NX)

      DO i=1,Nx

 X1= dfloat(i-1)*deltax
 X2=X1+deltax

 grid(i)%x=    (X1+X2)/2.d0
 grid(I)%volume= deltax

     do L=2,4,2
       if (L.EQ.2) THEN
grid(I)%cosn(L) =1.d0

grid(I)%aire_face(L)=1.d0
       END IF

       if (L.EQ.4) THEN
grid(I)%cosn(L) =-1.d0

grid(I)%aire_face(L)= 1.d0
        END IF

     end do
     End Do



RETURN
END


!=======================================================================

SUBROUTINE GREEN_FLUX(grid,VARI,SOMFLUX,FLUX,I,NEQ,NX)
       use precisions
 use COURANT
     use geometrie
IMPLICIT NONE
INTEGER I,NEQ,NX

REAL*8 VARI(0:NX+1,4,NEQ),SOMFLUX(NEQ),FLUX(NEQ)
    type(maillage),dimension(0:NX+1)::grid

! VARIABLE LOCALE
INTEGER IEQ,IFACE
REAL*8 L_face

    SOMFLUX=0.D0
    FLUX=0.D0

!-------------------------
                  DO 10 IFACE=2,4,2
       L_face  =grid(i)%aire_face(iface)

       FLUX(1)=vari(i,IFACE,1)*L_face * grid(i)%cosn(iface)
       FLUX(3)=vari(i,IFACE,3)*L_face
       FLUX(4)=vari(i,IFACE,4)*L_face

!    LA SOMME DES FLUX EST EFFECTIVEMENT CALCULEE
           DO IEQ=1,NEQ
               SOMFLUX(IEQ)=SOMFLUX(IEQ)-FLUX(IEQ)
           ENDDO
10                 CONTINUE ! Fin boucle sur les faces

RETURN
END

!=======================================================================
       SUBROUTINE INTERCELL (VARP,VARI,&
          grid,NX,NEQ)
use precisions
use COURANT
use geometrie
use PARAMETRE_EOS

IMPLICIT NONE

INTEGER NX,NEQ

        REAL*8 VARP(0:NX+1,4,NEQ)
        REAL*8 VARI(0:NX+1,4,NEQ)


    type(maillage),dimension(0:NX+1)     ::grid

!-----------------------------------------------------------------------
!   VARIABLES LOCALES
INTEGER I,IFACE,IFA,IEQ
REAL*8 ROGL,VGNL,VGTL,EGL
REAL*8 ROGR,VGNR,VGTR,EGR
REAL*8 RR,RN,RT,ENR,GA,GAM1
REAL*8 AR,PR,AL,PL



GA=PARA_EOS(1)
GAM1=GA-1.d0
!-----------------------------------------------------------------------
! SECOND ETAPE : CALCUL DES FLUX PASSANT AU TRAVERS DE CHAQUE INTERFACE
!                 ON UTILISE LES VALEURS U(N+1/2,J+1/2)
!-----------------------------------------------------------------------

!-----------------  LIMITE OUEST     -------------------------------
I=0
        IFACE = 2

IFA=IFACE+2
! prepare l etat gauche
call prepare_flux(VARP,-grid(i+1)%cosn(4),GA,GAM1,     &
        ROGL,VGNL,VGTL,EGL,AL,PL,           &
        IFACE,I,NX,NEQ)

! prepare l etat droit
CALL prepare_flux(VARP,-grid(i+1)%cosn(4),GA,GAM1,     &
        ROGR,VGNR,VGTR,EGR,AR,PR,           &
        IFACE+2,I+1,NX,NEQ)

!--------------------------------------------------------------------

CALL FLUHLLC(VGNL,VGTL,EGL,ROGL,PL,AL,        &
             VGNR,VGTR,EGR,ROGR,PR,AR,    &
                        RN,RT,ENR,RR)

           vari(i,IFACE,1)=RN
           vari(i,IFACE,2)=RT
           vari(i,IFACE,3)=ENR
           vari(i,IFACE,4)=RR

!----------------------------------------------------------------------
!  		POINT COURANT
!----------------------------------------------------------------------

        DO 200 I=1,NX

IFACE = 2


IFA=IFACE+2
! prepare l etat gauche
  call prepare_flux(VARP,     &
        grid(i)%cosn(IFACE),GA,GAM1,     &
        ROGL,VGNL,VGTL,EGL,AL,PL,     &
        IFACE,I,NX,NEQ)
! prepare l etat droit
   CALL prepare_flux(VARP,    &
        grid(i)%cosn(IFACE),GA,GAM1,    &
        ROGR,VGNR,VGTR,EGR,AR,PR,     &
        IFACE+2,I+1,NX,NEQ)

!   GAZ
       CALL FLUHLLC(VGNL,VGTL,EGL,ROGL,PL,AL,     &
                    VGNR,VGTR,EGR,ROGR,PR,AR,     &
                    RN,RT,ENR,RR)


!            RANGEMENT DE L'ETAT A L'INTERFACE (ENTRE CELLULES)
!
           vari(i,IFACE,1)=RN
           vari(i,IFACE,2)=RT
           vari(i,IFACE,3)=ENR
           vari(i,IFACE,4)=RR

IFACE = 4

           DO IEQ=1,NEQ
            vari(i,IFACE,IEQ)=- VARI(I-1,2,IEQ)
           ENDDO
           vari(i,IFACE,1)=-vari(i,IFACE,1)
           vari(i,IFACE,2)=-vari(i,IFACE,2)

200    CONTINUE
RETURN
END

!=======================================================================
          SUBROUTINE LIMGAZ(def_bloc,VAR,NX,NEQ)
       use precisions
use COURANT
use geometrie
                 IMPLICIT NONE
INTEGER NX,NEQ
REAL*8  VAR(0:NX+1,NEQ)
 type(definition_domaine) ::def_bloc
! VARAIBLE LOCALE
INTEGER IEQ

!  limite gauche
      If(def_bloc%type_limite(4)==1) then
            DO IEQ=1,NEQ
              VAR(0,IEQ)=VAR(1,IEQ)
            END DO
       end if
     If(def_bloc%type_limite(4)==2) then
        DO IEQ=1,NEQ
            VAR(0,IEQ)=VAR(1,IEQ)
        END DO
              VAR(0,1)=-VAR(1,1)
    end if

! limite droite

     If(def_bloc%type_limite(2)==1) then
            DO IEQ=1,NEQ
              VAR(NX+1,IEQ)=VAR(NX,IEQ)
            END DO
      end if

     If(def_bloc%type_limite(2)==2) then
      DO IEQ=1,NEQ
              VAR(NX+1,IEQ)=VAR(NX,IEQ)
      END DO
              VAR(NX+1,1)=-VAR(NX,1)
     end if

return
end
!=======================================================================

SUBROUTINE prepare_flux(VARP,DNX,GA,GAM1,    &
                    RO,VN,VT,EG,A,P, &
                   IFAC,I,NX,NEQ)

       use dimension_probleme


IMPLICIT NONE

INTEGER I,NX,NEQ,IFAC
REAL*8 VARP(0:NX+1,4,NEQ)
REAL*8 DNX
! GAZ
REAL*8 RO,VN,VT,EG,P,A,EI
REAL*8 GA,GAM1

!  VARIABLE LOCALE


!-----------------------------------------------------------------------
! 		CALCUL DE L'ETAT PROJETE DANS LE REPERE DE LA FACE IFAC du point I (gauche ou droite)
!-----------------------------------------------------------------------
      VN= varp(i,IFAC,1)*DNX   ! vitesse normale
      VT= varp(i,IFAC,2)*DNX   ! vitesse tangeantielle
      EG= varp(i,IFAC,3)
      RO=  varp(i,IFAC,4)
EI =(EG -0.5D0*(VN*VN+ VT*VT)/RO)/RO
p=gam1*ro*ei
A=dsqrt(GA*P/RO)

RETURN
END
!=======================================================================

!=========================================================================
SUBROUTINE PRIM(cell_gaz,   &
                    VAR,       &
                   NX,NEQ)

       use precisions
       use COURANT
       use dimension_probleme
       use PARAMETRE_EOS
     use geometrie
     IMPLICIT NONE

INTEGER NX,NEQ
!  GAZ

type(cellule_gaz),dimension(0:NX+1),intent(out) ::cell_gaz
!   SCHEMA
       REAL*8   VAR(0:NX+1,NEQ)
!   VARIABLES LOCALES
INTEGER I
REAL*8 E,PI
REAl*8 GA,GAP1,GAM1,CV
real(kind=dp) ::rog,UG,PG,TG,VIT_SON

       PI=DACOS(-1.D0)


!-----------------------------------------------------------------------

GA =PARA_EOS(1)
GAp1=GA+1.d0
GAM1=GA-1.d0
CV=PARA_EOS(2)

        DO 100 I=0,NX+1

ROG=VAR(I,4)
UG=VAR(I,1)/ROG
E=VAR(I,3)-0.5D0*ROG*UG**2
e=e/rog
pG=gam1*rog*e
TG=e/CV
vit_son=dsqrt(ga*pg/rog)

cell_gaz(i)%ro =ROG
cell_gaz(i)%u  =UG
cell_gaz(i)%p  =PG
cell_gaz(i)%c  =VIT_SON
cell_gaz(i)%t  =TG

100 CONTINUE
      RETURN
         END

