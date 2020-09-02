PROGRAM test_abstraction22
 USE mo_column , ONLY: compute_all
 REAL :: a ( 1 : 20 , 1 : 60 )
 REAL :: b ( 1 : 20 , 1 : 60 )
 REAL :: q ( 1 : 20 , 1 : 60 )
 REAL :: t ( 1 : 20 , 1 : 60 )
 INTEGER :: nproma
 INTEGER :: nz
 INTEGER :: p
 INTEGER :: pstart
 INTEGER :: pend

 nproma = 20
 nz = 60
 pstart = 1
 pend = nproma
 DO p = 1 , nproma , 1
  q ( p , 1 ) = 0.0
  t ( p , 1 ) = 0.0
  a ( p , : ) = 2.0
  b ( p , : ) = 3.0
 END DO
!$omp target data map(alloc:a(:,:),b(:,:),q(:,:),t(:,:))
!$omp target update to(a(:,:),b(:,:),q(:,:),t(:,:))
 CALL compute_all ( nz , a ( : , : ) , b ( : , : ) , q ( : , : ) , t ( : , : )&
  , nproma = nproma , pstart = pstart , pend = pend )
!$omp target update from(a(:,:),b(:,:),q(:,:),t(:,:))
!$omp end target data
 PRINT * , sum ( q )
 PRINT * , sum ( t )
END PROGRAM test_abstraction22

