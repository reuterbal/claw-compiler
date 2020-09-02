!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

MODULE mo_column
  IMPLICIT NONE
CONTAINS

  SUBROUTINE compute_all(nz, a, b, q, t)
    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    !$claw model-data
    REAL, INTENT(INOUT)   :: a(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: b(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only
    !$claw end model-data
    INTEGER :: k

    !$claw sca

    DO k=1,nz
      a(k) = a(k) + b(k)
    END DO

    !$claw sca forward
    CALL compute_column(nz, q, t)

  END SUBROUTINE compute_all

  ! Compute only one column
  SUBROUTINE compute_column(nz, q, t)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    !$claw model-data
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only
    !$claw end model-data
    INTEGER :: k                  ! Loop index
    REAL :: c                     ! Coefficient

    !$claw sca routine

    c = 5.345
    DO k = 2, nz
      t(k) = c * k
      q(k) = t(k - 1)  + t(k) * c
    END DO
    q(nz) = q(nz) * c

  END SUBROUTINE compute_column
END MODULE mo_column
