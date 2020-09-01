!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Test the CLAW abstraction model with one additional dimension.
!

PROGRAM test_abstraction22
  USE mo_column, ONLY: compute_all
  REAL, DIMENSION(20,60) :: a, b  ! Fields as declared in the whole model
  REAL, DIMENSION(20,60) :: q, t  ! Fields as declared in the whole model
  INTEGER :: nproma, nz           ! Size of array fields
  INTEGER :: p                    ! Loop index
  INTEGER :: pstart, pend         ! Loop bounds

  nproma = 20
  nz = 60
  pstart = 1
  pend = nproma

  DO p = 1, nproma
    q(p,1) = 0.0
    t(p,1) = 0.0
    a(p,:) = 2.0
    b(p,:) = 3.0
  END DO

  !$claw sca forward create update
  DO p = 1, nproma
    CALL compute_all(nz, a(p,:), b(p,:), q(p,:), t(:,p))
  END DO

  PRINT*,SUM(q)
  PRINT*,SUM(t)
END PROGRAM test_abstraction22
