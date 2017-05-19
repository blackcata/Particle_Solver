!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : Particle_solver.f90                                              !
!                                                                              !
!   PURPOSE : Solvers for particle solvers                                     !
!                                                                              !
!                                                             2017.05.16 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

        SUBROUTINE SOLVER
          USE particle
          USE numerical

          IMPLICIT NONE

          INTEGER :: it

          DO it = 1,N_par
            CALL VEL_INTERPOLATION(it)
            CALL RUNGE_KUTTA_VEL(it)
          END DO

        END SUBROUTINE SOLVER
