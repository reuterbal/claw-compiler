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

  DO k = 1 , nz , 1
   DO p = pstart , pend , 1
    a ( p , k ) = a ( p , k ) + b ( p , k )
   END DO
  END DO
  CALL compute_column ( nz , q , t , nproma = nproma , pstart = pstart , pend&
   = pend )
 END SUBROUTINE compute_all

 SUBROUTINE compute_column ( nz , q , t , nproma , pstart , pend )
  INTEGER , INTENT(IN) :: pend
  INTEGER , INTENT(IN) :: pstart
  INTEGER , INTENT(IN) :: nproma

  INTEGER , INTENT(IN) :: nz
  REAL , INTENT(INOUT) :: t ( : , : )
  REAL , INTENT(INOUT) :: q ( : , : )
  INTEGER :: k
  REAL :: c
  INTEGER :: p

  c = 5.345
  DO k = 2 , nz , 1
   DO p = pstart , pend , 1
    t ( p , k ) = c * k
    q ( p , k ) = t ( p , k - 1 ) + t ( p , k ) * c
   END DO
  END DO
  DO p = pstart , pend , 1
   q ( p , nz ) = q ( p , nz ) * c
  END DO
 END SUBROUTINE compute_column

END MODULE mo_column

