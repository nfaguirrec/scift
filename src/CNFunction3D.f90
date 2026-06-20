!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                                   !!
!!  This file is part of SciFT project                                               !!
!!  Copyright (c) 2013-2013 Nestor F. Aguirre (nfaguirrec@gmail.com)                 !!
!!                                                                                   !!
!!  Redistribution and use in source and binary forms, with or without               !!
!!  modification, are permitted provided that the following conditions are met:      !!
!!                                                                                   !!
!!  1. Redistributions of source code must retain the above copyright notice, this   !!
!!     list of conditions and the following disclaimer.                              !!
!!  2. Redistributions in binary form must reproduce the above copyright notice,     !!
!!     this list of conditions and the following disclaimer in the documentation     !!
!!     and/or other materials provided with the distribution.                        !!
!!  3. Neither the name of the copyright holders nor the names of its contributors   !!
!!     may be used to endorse or promote products derived from this software         !!
!!     without specific prior written permission.                                    !!
!!                                                                                   !!
!!  The copyright holders provide no reassurances that the source code provided      !!
!!  does not infringe any patent, copyright, or any other intellectual property      !!
!!  rights of third parties.  The copyright holders disclaim any liability to any    !!
!!  recipient for claims brought against recipient by any third party for            !!
!!  infringement of that parties intellectual property rights.                       !!
!!                                                                                   !!
!!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND  !!
!!  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED    !!
!!  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE           !!
!!  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR  !!
!!  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES   !!
!!  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;     !!
!!  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND      !!
!!  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT       !!
!!  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS    !!
!!  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                     !!
!!                                                                                   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!>
!! @brief
!!
module CNFunction3D_
	use GOptions_
	use Math_
	use String_
	use IOStream_
	use RNFunction_
	use Grid3D_
	use RNFunction3D_
	implicit none
	private
	public :: funcTest
	
	public :: CNFunction3D_testOpenMP
	
!>
!! This class use the List template declared into List.h90 file,
!! please take a look to this file for details
!!
#define NFunction3D CNFunction3D
#define __TYPE_VALUE__ complex(8)
#define __ADD_METHODS__ \
	procedure :: fromRNFunction3D
#define __ID_TYPE__ 1
#include "NFunction3D.h90"
#undef NFunction3D
#undef __TYPE_VALUE__
#undef __ADD_METHODS__
#undef __ID_TYPE__

	!>
	!! @brief 
	!!
	subroutine fromRNFunction3D( this, nfunc )
		class(CNFunction3D) :: this
		class(RNFunction3D) :: nfunc
		
		write(*,*) "### ERROR ### Function CNFunction3D::fromRNFunction3D is not implemented yet"
		stop
	end subroutine fromRNFunction3D
	
	!>
	!! @brief Convert to string
	!!
	function str( this, formatted, prefix ) result( output )
		class(CNFunction3D) :: this 
		character(:), allocatable :: output
		logical, optional :: formatted
		character(*), optional :: prefix
		
		logical :: effFormatted
		character(:), allocatable :: effPrefix
		
		integer :: fmt
		character(200) :: fstr
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effPrefix = ""
		if( present(prefix) ) effPrefix = prefix
		
		output = ""
		
		if( .not. effFormatted ) then
#define ITEMS(l,v) output = trim(output)//effPrefix//trim(l)//trim(adjustl(v))
#define ITEMI(l,v) output = trim(output)//l; write(fstr, "(i20)") v; output = trim(output)//trim(adjustl(fstr))
#define ITEMR(l,v) output = trim(output)//l; write(fstr, "(f20.6)") v; output = trim(output)//trim(adjustl(fstr))
		
			output = trim(output)//"<CNFunction3D:"
			output = trim(output)//trim(this.xyzGrid.str())
! 			ITEMI( "min=", this.min )
			ITEMI( ",size=", this.size() )
#undef RFMT
#undef ITEMS
#undef ITEMI
#undef ITEMR
			output = trim(output)//">"
		else
#define LINE(l) output = trim(output)//effPrefix//l//new_line('')
#define ITEMS(l,v) output = trim(output)//effPrefix//l; write(fstr, "(x,a)") trim(v); output = trim(output)//trim(fstr)//new_line('')
#define ITEMI(l,v) output = trim(output)//effPrefix//l; write(fstr, "(i10)") v; output = trim(output)//trim(fstr)//new_line('')
#define ITEMR(l,v) output = trim(output)//effPrefix//l; write(fstr, "(f10.5)") v; output = trim(output)//trim(fstr)//new_line('')

			LINE("CNFunction3D")
			LINE("---------")
! 			ITEMI( "min=", this.min )
! 			ITEMR( ",size=", this.size )
			LINE("")
#undef LINE
#undef ITEMS
#undef ITEMI
#undef ITEMR
		end if
	end function str
	
	!>
	!! Save the data in two column format in a
	!! selected unit
	!!
	subroutine toFStream( this, ofile )
		class(CNFunction3D) :: this
		type(OFStream), optional, intent(in) :: ofile
		
		integer :: unitEff
		
		if( present(ofile) ) then
			unitEff = ofile.unit
		else
			unitEff = IO_STDOUT
		end if
		
		write(*,*) "### ERROR ### CNFunction3D.toFStream is no implemented yet"
		stop
	end subroutine toFStream

	!>
	!! @brief Show 
	!!
	subroutine show( this, unit, formatted )
		class(CNFunction3D) :: this
		integer, optional, intent(in) :: unit
		logical, optional :: formatted
		
		integer :: effunit
		logical :: effFormatted
		
		effFormatted = .false.
		if( present(formatted) ) effFormatted = formatted
		
		effunit = 6
		if( present(unit) ) effunit = unit
		
		write(effunit,"(a)") trim(str(this,effFormatted))
	end subroutine show

	!>
	!! This is neccesary only for NFunction_test()
	!!       f = exp(-0.44*x)*sin(x)**2
	!!   df/dx = exp(-0.44*x)*(2.0*sin(x)*cos(x)-0.44*sin(x)**2)
	!! d2f/dx2 = exp(-0.44*x)*(2.0*cos(x)**2 - 1.76*cos(x)*sin(x) - 2.0*sin(x)**2 + 0.1936*sin(x)**2)
	!!
	pure function funcTest( x, y, z ) result( output )
		real(8), intent(in) :: x, y, z
		complex(8) :: output
		
! 		output = sqrt( x**2 + y**2 + z**2 )
! 		output = (x**2+y**2-1.0_8)**3-x**2*y**3+z**2
		output = (x**2+2.25_8*y**2+z**2-1.0_8)**3-x**2*z**3-0.1125_8*y**2*z**3 + Math_I*( sqrt( x**2 + y**2 + z**2 ) )  ! http://mathworld.wolfram.com/HeartSurface.html
	end function funcTest
	
	subroutine kernel( k, ny, nx, fArray )
		integer, intent(in) :: k, ny, nx
		complex(8), intent(inout) :: fArray(:,:,:)
		
		integer :: j, i
		
		do j=1,ny; do i=1,nx
				fArray(i,j,k) = sqrt( 10.5_8 )**5
		end do; end do
	end subroutine
	
	!>
	!! @test Testing OPENMP scaling for CNFunction3D class
	!! @brief Testing OPENMP scaling for CNFunction3D class
	!!
	subroutine CNFunction3D_testOpenMP()
#ifdef _OPENMP
		use omp_lib
		use Timer_
		use CommandLineParser_
		
		type(Grid3D) :: xyzGrid
		type(CNFunction3D) :: func
		type(CommandLineParser) :: parser
		type(Timer) :: sTime
		
		integer :: strategy, nThreads
		integer :: i, j, k
		integer :: nx, ny, nz
		integer :: nCopy
		real(8) :: ssum
		
		call sTime.init()
		
		nx = parser.getInteger( "-n", def=500 )
		ny = nx
		nz = nx
		
		call xyzGrid.init( min=[-3.0_8,-3.0_8,-3.0_8], max=[3.0_8,3.0_8,3.0_8], size=[nx,ny,nz] )
		call func.init( xyzGrid, funcTest )
		
		write(*,"(A1,A9,2A10)") "#", "nThreads", "strategy", "time"
		write(*,"(A1,A9,2A10)") "#", "--------", "--------", "----"
		
		do strategy=1,6
			do nThreads=1,omp_get_num_procs()
				
				call omp_set_num_threads( nThreads )
				
				ssum = 0.0_8
				do nCopy=1,2
					call sTime.start()
					
					select case(strategy)
						case(1)
							!$omp parallel
							!$omp do
								do k=1,nz; do j=1,ny; do i=1,nx
									func.fArray(i,j,k) = sqrt( 10.5_8 )**5
								end do; end do; end do
							!$omp end do
							!$omp end parallel
							
						case(2)
							!$omp parallel private(k,j,i)
							!$omp do
								do k=1,nz; do j=1,ny; do i=1,nx
									func.fArray(i,j,k) = sqrt( 10.5_8 )**5
								end do; end do; end do
							!$omp end do
							!$omp end parallel
							
						case(3)
							!$omp parallel private(k,j,i)
							!$omp do
								do k=1,nz; do j=1,ny; do i=1,nx
									func.fArray(i,j,k) = sqrt( 10.5_8 )**5
								end do; end do; end do
							!$omp end do nowait
							!$omp end parallel
							
						case(4)
							!$omp parallel private(k)
							!$omp do
								do k=1,nz; do j=1,ny; do i=1,nx
									func.fArray(i,j,k) = sqrt( 10.5_8 )**5
								end do; end do; end do
							!$omp end do
							!$omp end parallel
							
						case(5)
							!$omp parallel private(k)
							!$omp do schedule(dynamic,1)
								do k=1,nz; do j=1,ny; do i=1,nx
									func.fArray(i,j,k) = sqrt( 10.5_8 )**5
								end do; end do; end do
							!$omp end do
							!$omp end parallel
							
						case(6)
							!$omp parallel private(k)
							!$omp do schedule(dynamic,1)
								do k=1,nz; do j=1,ny; do i=1,nx
									func.fArray(i,j,k) = sqrt( 10.5_8 )**5
								end do; end do; end do
							!$omp end do nowait
							!$omp end parallel
							
						case(7)
							!$omp parallel private(k)
							!$omp do schedule(dynamic,2)
								do k=1,nz; do j=1,ny; do i=1,nx
									func.fArray(i,j,k) = sqrt( 10.5_8 )**5
								end do; end do; end do
							!$omp end do nowait
							!$omp end parallel
							
						case(8)
							!$omp parallel private(k)
							!$omp do schedule(static,1)
								do k=1,nz; do j=1,ny; do i=1,nx
									func.fArray(i,j,k) = sqrt( 10.5_8 )**5
								end do; end do; end do
							!$omp end do
							!$omp end parallel
							
						case(9)
							!$omp parallel private(k)
							!$omp do schedule(static,2)
								do k=1,nz; do j=1,ny; do i=1,nx
									func.fArray(i,j,k) = sqrt( 10.5_8 )**5
								end do; end do; end do
							!$omp end do nowait
							!$omp end parallel
							
						case(10)
							!$omp parallel default(shared) private(k,j,i)
							!$omp do schedule(static)
								do k=1,nz; do j=1,ny; do i=1,nx
										func.fArray(i,j,k) = sqrt( 10.5_8 )**5
								end do; end do; end do
							!$omp end do nowait
							!$omp end parallel
							
						case(11)
							!$omp parallel default(shared) private(k,j,i)
							!$omp do schedule(static)
								do k=1,nz
									call kernel( k, ny, nx, func.fArray )
								end do
							!$omp end do nowait
							!$omp end parallel
							
					end select
					
					ssum = ssum + sTime.elapsedSeconds()
				end do
			
				write(*,"(2I10,F10.5)") omp_get_max_threads(), strategy, ssum/2.0_8
				
			end do
			
			write(*,*) ""
			write(*,*) ""
		end do
#else
		write(*,*) "### ERROR ### CNFunction3D_testOpenMP(): OPENMP is not available"
		stop
#endif
	end subroutine CNFunction3D_testOpenMP
	
end module CNFunction3D_
