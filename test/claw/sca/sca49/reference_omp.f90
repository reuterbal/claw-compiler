MODULE mo_column

CONTAINS
 SUBROUTINE compute_all ( nz , a , b , q , t , nproma , pstart , pend )
  INTEGER , INTENT(IN) :: pend
  INTEGER , INTENT(IN) :: pstart
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: a ( : , : )
  REAL , INTENT(INOUT) :: b ( : , : )
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER :: k
  INTEGER :: p

!$omp target
!$omp teams
!$omp distribute
  DO p = pstart , pend , 1
   DO k = 1 , nz , 1
    a ( p , k ) = a ( p , k ) + b ( p , k )
   END DO
   CALL compute_column ( nz , q ( p , : ) , t ( p , : ) )
  END DO
!$omp end distribute
!$omp end teams
!$omp end target
 END SUBROUTINE compute_all

 SUBROUTINE compute_column ( nz , q , t )

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : )
  REAL , INTENT(INOUT) :: q ( : )
  INTEGER :: k
  REAL :: c

!$omp declare target
!$omp single
  c = 5.345
  DO k = 2 , nz , 1
   t ( k ) = c * k
   q ( k ) = t ( k - 1 ) + t ( k ) * c
  END DO
  q ( nz ) = q ( nz ) * c
 END SUBROUTINE compute_column

END MODULE mo_column

