module ThrularNumerovMethod_
	use IOStream_
	use Grid_
	use RNFunction_
	implicit none
	private
	
	public :: &
		ThrularNumerovMethod_test
	
	type, public :: ThrularNumerovMethod
		real(8) :: rMass
		integer :: nStates
		type(RNFunction) :: potential
		real(8), allocatable :: eigenvalue(:)
		type(RNFunction), allocatable :: eigenfunction(:)
		
		contains
			procedure init
			procedure destroy
			procedure str
			procedure show
			procedure run
	end type ThrularNumerovMethod
	
	contains
	
	!>
	!! @brief Contructor
	!!
	subroutine init( this, potential, nStates, rMass )
		implicit none
		class(ThrularNumerovMethod) :: this
		class(RNFunction), intent(in) :: potential
		integer, optional, intent(in) :: nStates
		real(8), optional, intent(in) :: rMass
		
		this.potential = potential
		
		if( present(nStates) ) then
			this.nStates = nStates
		else
			this.nStates = 10
		end if
		
		if( present(rMass) ) then
			this.rMass = rMass
		else
			this.rMass = 1.0_8
		end if
		
		allocate( this.eigenvalue(potential.xGrid.nPoints) )
		allocate( this.eigenfunction(this.nStates) )
	end subroutine init
	
	!>
	!! @brief Destructor
	!!
	subroutine destroy( this )
		implicit none
		class(ThrularNumerovMethod) :: this
		
		this.rMass = 1.0_8
		this.nStates = 10
		
		deallocate( this.eigenvalue )
		deallocate( this.eigenfunction )
	end subroutine destroy
	
	!>
	!! @brief String representation of the object
	!!
	function str( this ) result( output )
		implicit none
		class(ThrularNumerovMethod) :: this 
		character(len=200) :: output
		
		integer :: fmt
		character(len=200) :: strBuffer
		
		output = ""
		
		output = trim(output)//"<ThrularNumerovMethod:"
		
		output = trim(output)//this.potential.str()
		
		output = trim(output)//",rMass="
		fmt = int(log10(this.rMass+1.0))+1
		write(strBuffer, "(f<fmt+7>.6)") this.rMass
		output = trim(output)//trim(strBuffer)
		
		output = trim(output)//",nStates="
		fmt = int(log10(float(this.nStates)+1.0))+1
		write(strBuffer, "(i<fmt>)") this.nStates
		output = trim(output)//trim(strBuffer)
		
		output = trim(output)//">"
	end function str
	
	!>
	!! @brief Write the string representation of the object
	!        in a selected unit
	!!
	subroutine show( this, unit )
		class(ThrularNumerovMethod) :: this
		integer, optional, intent(in) :: unit
		
		integer :: effunit
		
		if( present(unit) ) then
			effunit = unit
		else
			effunit = 6
		end if
		
		write(effunit,"(a)") trim(this.str())
	end subroutine show
	
	!>
	!! @brief start the numerical method
	!!
	subroutine run( this )
		implicit none
		class(ThrularNumerovMethod) :: this
		
		integer :: i
		real(8), allocatable :: eigenfunction(:,:)
		integer :: nBound
		
		allocate( eigenfunction(this.nStates,this.potential.xGrid.nPoints) )
		
		call thrularnumerov( this.rMass, this.potential.xGrid.min, this.potential.xGrid.max, &
							 this.potential.xGrid.nPoints, this.potential.fArray, this.nStates, 1, &
							 this.eigenvalue, eigenfunction, nBound )
		do i=1,this.nStates
			call this.eigenfunction(i).fromGridArray( this.potential.xGrid, eigenfunction(i,:) )
! 			this.eigenfunction(i).fArray(:) = eigenfunction(i,:)
		end do
		
		deallocate( eigenfunction )

		this.nStates = nBound
	end subroutine run
	
	!>
	! Internal function, neccesary to link with numerov method writed in f77
	!!
	subroutine thrularnumerov(xmu, rmin, rmax, ngridr, V, nbound, ifun, Ebound, fbound, nboundEnd)
		implicit none
		integer, intent(in) :: ngridr, nbound, ifun
		double precision, intent(in)  :: xmu, rmin, rmax, V(ngridr)
		double precision, intent(out) :: Ebound(nbound), fbound(nbound,ngridr)
		integer, intent(out) :: nboundEnd
		integer :: i, nb, itry, maxit, kv
		double precision :: dr, Z, ZZ, eps, E0, E2, VV(ngridr), G(ngridr,2), state(ngridr)
		
		dr=(rmax-rmin)/dfloat(ngridr-1)
		Z=0.5d0/xmu
		ZZ=2.0d0*xmu
		maxit=20
		eps=1.D-20
		
		! Approximate bound energies (Numerov Method):
		do i=1,ngridr
			G(i,1) = -2.d0 - 2.d0*xmu*V(i)*dr*dr
			G(i,2) =  1.d0
		end do
		
		call givhoa(G,ngridr,Ebound,nbound)
		
		nb=0
		do i=1,ngridr
			Ebound(i)=-Z*Ebound(i)/dr/dr
			if (Ebound(i) < 0.d0) then
				nb=nb+1
	!             write(*,*) Ebound(i)/Econv
			end if
		end do
		nboundEnd = nb
		!write(*,*) 'Number of bound states: ', nbound
		
		! Exact bound energies and states (Thrular method):
		VV(:)=ZZ*V(:)
		maxit=40
		eps=1.0e-20
		do i=1,nb
			E0=Ebound(i)*ZZ
			call SCHR(rmin,rmax,ngridr,dr,VV, MAXIT,EPS,KV,ITRY,  E0,E2,state)
			Ebound(i)=E2/ZZ
			fbound(i,:)=state(:)
	!          write(*,*) E0/ZZ/Econv,Ebound(i)/Econv,kv,itry
	!          write(*,*) Ebound(i)/Econv
		end do
	end subroutine thrularnumerov

	SUBROUTINE GIVHOA(A,NPUN,E,NEV)
		implicit none
	!     A IS THE NPUN X NPUN INPUT MATRIX.  THE NEV ALGEBRAICALLY LARGEST
	!     EIGENVALUES WILL BE COMPUTED AND STORED IN E.
		integer :: i, ii,  m,  k, n, npun, nev, ip1, nm1, nm2, ip2, ag
		LOGICAL :: FIRST,IN
		double precision :: KAP,NORM,LAMBDA,L,U,MULT, alpha, T, S
		double precision :: A(NPUN,2),B(NPUN),C(NPUN)
		double precision :: W(NPUN), E(NEV)
		
		N=NPUN
		NM1=N-1
		NM2=N-2
	!     BEGIN THE REDUCTION TO TRIDIAGONAL FORM.  THE ORIGINAL MATRIX A I
	!     DESTROYED BY THE REDUCTION AND THE DIAGONAL AND OFF-DIAGONAL
	!     ELEMENTS OF THE TRIDIAGONAL MATRIX ARE STORED IN C AND B.  THE
	!     DIAGONAL POSITIONS OF A ARE USED TO STORE THE ALPHAS AND
	!     THE SUBDIAGONAL POSITIONS HOLD THE VECTORS W.
		IF(N.LE.2) GO TO 9
		DO 8 I=1,NM2
		IP1=I+1
		C(I)=A(I,1)
		B(I)=-A(I,2)
	!     IF A(I,2) IS ZERO THEN ALPHA MUST BE ZERO.
		ALPHA=0.D0
		IF(A(I,2).EQ.0.D0) GO TO 8
		ALPHA=0.5D0/A(I,2)**2
		IP2=I+2
		A(IP1,2)=-A(IP1,2)
		A(I,2)=2.D0*A(I,2)
		8 A(I,1)=ALPHA
		9 C(N-1)=A(N-1,1)
		C(N)=A(N,1)
		B(N-1)=A(N-1,2)
		B(N)=0.D0
	!     THIS COMPLETES THE REDUCTION TO TRIDIAGONAL FORM.
	!     BEGIN THE EIGENVALUE COMPUTATION
	!     COMPUTE NORM OF TRIDIAGONAL MATRIX,SQUARES OF THE B(I).
		NORM=DABS(C(1))+DABS(B(1))
		DO 10 I=2,N
		T=DABS(C(I))+DABS(B(I))+DABS(B(I-1))
	10 NORM=DMAX1(NORM,T)
		DO 11 I=1,N
	11 W(I)=B(I)**2
	!     SET K AND UPPER AND LOWER ESTIMATES.
		K=1
		U=NORM
		DO 12 I=1,NEV
	12 E(I)=-NORM
	!     BEGIN MAIN LOOP.
	13 L=E(K)
	14 LAMBDA=0.5*(L+U)
	!     THE CONVERGENCE TEST AS IMPLEMENTED HERE ALLOWS THE COMPUTATION T
	!     PROCEED UNTIL THE INTERVAL (L,U) CAN BE MADE NO SMALLER.
		IF ((LAMBDA.EQ.L).OR.(LAMBDA.EQ.U)) GO TO  30
	!     BEGIN COMPUTATION OF NUMBER OF SIGN AGREEMENTS,AG.
		AG=0
		I=1
	16 S=C(I)-LAMBDA
	18 IF(S.GE.0.D0) AG=AG+1
		IF(S.EQ.0.D0) GO TO 20
		I=I+1
		IF(I.GT.N) GO TO 22
		S=C(I)-LAMBDA-W(I-1)/S
		IF(II.LT.10000) GO TO 18
		AG=AG+1
		I=I-1
	20 I=I+2
		IF(I.LE.N) GO TO 16
	!     THE COMPUTATION OF AG IS COMPLETE.  ADJUST INTERVAL.
	22 IF(AG.GE.K) GO TO 24
		U=LAMBDA
		GO TO 14
	24 L=LAMBDA
		M=MIN0(AG,NEV)
		DO 26 I=K,M
	26 E(I)=LAMBDA
		GO TO 14
	!     THE KTH EIGENVALUE IS COMPUTED.  STORE IN E(K) AND PROCEED.
	30 E(K)=LAMBDA
		K=K+1
		IF(K.LE.NEV) GO TO 13
	!     THIS COMPLETES THE EIGENVALUE COMPUTATION.
		RETURN
	END SUBROUTINE GIVHOA
	
	SUBROUTINE SCHR(rmin,rmax,ngridr,dr,V, MAXIT,EPS,KV,ITRY, E0,E2,P)
		! Método de Thrular para el cálculo de estados ligados
		! Previamente se ha tenido que usar un método de Numerov
		! para calcular una energía ligada E0
		!
		! El método de Thrular mejora esta energía, E2
		! y facilita el estado ligado P(ngridr).
		!
		! Datos que debe leer: rmin, rmax, ngridr, dr, V(ngridr)
		implicit none
		integer :: i, i1, it, j, m, msave, m1, n, nl, maxit, kv, itry, ngridr
		double precision :: rmin,rmax,dr,V(ngridr)
		double precision :: E0, E2, E, EOLD, DE, DOLD, F, DF, eps, TEST, GN, GI, APR, PM, YIN, YOUT, YM, SN, SCHROD, XIT
		double precision :: H, H2, HV
		double precision :: Y(3)
		double precision, intent(out) :: P(ngridr)

		ITRY=0
		n=ngridr
		H=dr
		H2=H**2
		HV=H2/12.D0
		E=E0
		TEST=-1.D0
		DE=0.D0
		do i=1,N
			!S(I)=0.D0
			P(I)=0.D0
		end do
	!     BOUCLE DES ITERATIONS
	12 DO 171 IT=1,MAXIT
		XIT=IT
	!     INTEGRATION VERS L-INTERIEUR.PREMIERS PAS
		P(N)=1.D-30
		GN=V(N)-E
		GI=V(N-1)-E
	!     WRITE(6,1122)GN,GI
	1122  FORMAT(2X,'GN = ',D16.8,2X,'GI =',D16.8/)
	!     E EST-IL TROP GRAND
		IF(GI.GE.0.D0) GO TO 36
		IF(IT.EQ.MAXIT)GO TO 900
		E=V(N-2)
	!     PRINT 901,E
	901 FORMAT(1H ,'E EST TROP GRAND ON LE REMPLACE PAR V(N-2) = ',E13.6)
		GO TO 36
	900 PRINT 899
	899 FORMAT('LA TECHNIQUE UTILISEE EST EN DEFAUT')
		ITRY=1
	914 FORMAT(1H ,5(5X,D13.6))
		RETURN
	36    APR=(RMAX-H)*DSQRT(GI)-RMAX*DSQRT(GN)
		IF(APR.GT.50.D0) APR=50.D0
		P(N-1)=P(N)*DEXP(-APR)
	38    Y(1)=(1.D0-HV*GN)*P(N)
	40    Y(2)=(1.D0-HV*GI)*P(N-1)

	!     INTEGRATION
	44 M=N-2
	46 Y(3)=Y(2)+((Y(2)-Y(1))+H2*GI*P(M+1))
	48 GI=V(M)-E
	50    P(M)=Y(3)/(1.D0-HV*GI)
	52    IF(DABS(P(M)).LT.1.E+34) GO TO 70

	!     DEPASSEMENT DE LA LIMITE
		M1=M+1
		PM=P(M1)
	179   FORMAT(2X,'PM = ',D16.8/)
	55 DO 56 J=M1,N
	56 P(J)=P(J)/PM
	58 Y(1)=Y(1)/PM

	!     NOUVEAU DEPART
	60 Y(2)=Y(2)/PM
	62 Y(3)=Y(3)/PM
		GI=V(M+1)-E
		GO TO 46
	!     L-INTEGRATION VERS L-INTERIEUR EST-ELLE TERMINEE
	70 IF((DABS(P(M)).LE.DABS(P(M+1))).OR.(M.LE.2))GO TO 90
	81 Y(1)=Y(2)
	82 Y(2)=Y(3)
	84 M=M-1
		GO TO 46
	!     L-INTEGRATION VERS L-INTERIEUR EST TERMINEE
	90 PM=P(M)
		MSAVE=M
	92 YIN=Y(2)/PM
	94 DO 96 J=M,N
	96 P(J)=P(J)/PM
	!     INTEGRATION VERS L-EXTERIEUR.PREMIERS PAS
	100   P(1)=1.D-20
	102 Y(1)=0.D0
	104 GI=V(1)-E
	106   Y(2)=(1.D0-HV*GI)*P(1)

	!     INTEGRATION
	108 DO 132 I=2,M
	110 Y(3)=Y(2)+((Y(2)-Y(1))+H2*GI*P(I-1))
	112 GI=V(I)-E
	114   P(I)=Y(3)/(1.D0-HV*GI)
	116   IF(DABS(P(I)).LT.1.E+34) GO TO 130
	!     LA LIMITE A ETE DEPASSEE
	118 I1=I-1
		PM=P(I1)
		DO 120 J=1,I1
	120 P(J)=P(J)/PM
	122 Y(1)=Y(1)/PM
	124 Y(2)=Y(2)/PM
	126 Y(3)=Y(3)/PM
		GI=V(I1)-E
		GO TO 110
	130 Y(1)=Y(2)
	132 Y(2)=Y(3)
	!     L-INTEGRATION VERS L-EXTERIEUR EST TERMINEE

		PM=P(M)
		IF(PM) 135,149,135
	135 YOUT=Y(1)/PM
	136 YM=Y(3)/PM
	138 DO 140 J=1,M
	140 P(J)=P(J)/PM
	!     LES DEUX BRANCHES SONT MAINTENANT RACCORDEES
	!     CORRECTION
	142   DF=0.D0
	144 DO 146 J=1,N
	146 DF=DF-P(J)**2
	148   F=(-YOUT-YIN+2.D0*YM)/H2+(V(M)-E)
		DOLD=DE
		IF(DABS(F).LT.1.E+37) GO TO 150
	149   F=9.99999D+29
		DF=-F
		DE=DABS(0.0001D0*E)
		GO TO 152
	150 DE=-F/DF
	152 CONTINUE
	156 FORMAT(I2,5X,E16.8,5X,E16.8,5X,E16.8,5X,E16.8)
	164 EOLD=E
		E=E+DE
		TEST=DMAX1((DABS(DOLD)-DABS(DE)),TEST)
		IF(TEST.LT.0.D0) GO TO 171
		IF(DABS(E-EOLD).LT.EPS)GO TO 172
	171 CONTINUE
		SCHROD=1.0D0
		GO TO 173
	!     LES ITERATIONS SONT TERMINEES
	172   SCHROD=0.D0
	!     LES ITERATIONS ONT CONVERGE
	!     COMPTAGE DES NOEUDS
	173 KV=0
		NL=N-2
	174 DO 192 J=3,NL
	176 IF(P(J))178,177,177
	177 IF(P(J-1))180,192,192
	178 IF(P(J-1))192,270,184
	!     NOEUD POSITIF
	180 IF(P(J+1))192,182,182
	182 IF(P(J-2))190,192,192
	!     NOEUD NEGATIF
	184 IF(P(J+1))186,192,192
	186 IF(P(J-2))192,190,190
	!     LE NOEUD EST-IL DU A UN SOUS-DEPASSEMENT
	270 IF(P(J+1))280,192,192
	280 IF(P(J-2))192,192,190
	190 KV=KV+1
	192 CONTINUE
		E2=E
	!     NORMALISATION
	200 SN=DSQRT(-H*DF)
	202 DO 204 J=1,N
	204 P(J)=P(J)/SN
	250 FORMAT(10X,'SCHR=',I1,/)
	!     ECRITURE DE LA SOLUTION
	214 FORMAT('V=',I3,5X,' E=',E16.8/)
	228 FORMAT(10X,E16.8,10X,E16.8)
		RETURN
	END SUBROUTINE SCHR
	
	!>
	! This is neccesary only for NFunction_test()
	!!
	function funcTest( x ) result( output )
		real(8), intent(in) :: x
		real(8) :: output
		
		output = 5.0_8*( exp(2.0_8*(2.0_8-x))-2.0_8*exp(2.0_8-x) )
	end function funcTest
	
	!>
	! Test method of this class
	!!
	subroutine ThrularNumerovMethod_test()
		implicit none
		
		type(Grid) :: rGrid
		type(RNFunction) :: potential
		type(ThrularNumerovMethod) :: solver
		integer :: i
		
		call rGrid.init( 1.0_8, 30.0_8, 1000 )
		call rGrid.show()
		
		call potential.fromFunction( rGrid, funcTest )
		call potential.show()
		call potential.save( "morse.out" )
		
		call solver.init( potential, rMass=5.0_8 )
		call solver.run()
		
		do i=1,solver.nStates
			write(*,"(i5,f20.10)") i, solver.eigenvalue(i)
		end do
		
		call solver.eigenfunction(7).save( "salida" )
	end subroutine ThrularNumerovMethod_test

end module ThrularNumerovMethod_