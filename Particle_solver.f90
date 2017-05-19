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
          REAL(KIND=8) :: TMP, time_sta, time_end

          TMP = 0.0

          WRITE(*,*) '----------------------------------------------------'
          WRITE(*,*) '      PARTICLE TIME INTERATION PROCESS STARTED      '
          CALL CPU_TIME(time_sta)

          DO it = 1,N_par
            CALL VEL_INTERPOLATION(it)
            !------------------------------------------------------------------!
            !                   Calculate particles velocities                 !
            !------------------------------------------------------------------!
            CALL RUNGE_KUTTA(PAR_VEL,                                           &
                        particles(it)%X_vel,interpol_vel(1),particles(it)%X_vel)
            CALL RUNGE_KUTTA(PAR_VEL,                                        &
                        particles(it)%Y_vel,interpol_vel(2),particles(it)%Y_vel)
            CALL RUNGE_KUTTA(PAR_VEL,                                        &
                        particles(it)%Z_vel,interpol_vel(3),particles(it)%Z_vel)

            !------------------------------------------------------------------!
            !                  Calculate particles positions                   !
            !------------------------------------------------------------------!
            CALL RUNGE_KUTTA(PAR_POS,                                           &
                                    particles(it)%X_vel,TMP,particles(it)%X_pos)
            CALL RUNGE_KUTTA(PAR_POS,                                           &
                                    particles(it)%Y_vel,TMP,particles(it)%Y_pos)
            CALL RUNGE_KUTTA(PAR_POS,                                           &
                                    particles(it)%Z_vel,TMP,particles(it)%Z_pos)
          END DO

          CALL CPU_TIME(time_end)
          WRITE(*,*) '       PARTICLE TIME INTERATION PROCESS ENDED       '
          WRITE(*,*) '  Total Reading time : ',time_end - time_sta,' s'
          WRITE(*,*) '----------------------------------------------------'
          WRITE(*,*) ''

        END SUBROUTINE SOLVER
