!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : Particle_solver.f90                                              !
!                                                                              !
!   PURPOSE : Solvers for particle solvers                                     !
!                                                                              !
!                                                             2017.05.16 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

        SUBROUTINE SOLVER(it)
          USE particle
          USE numerical

          IMPLICIT NONE

          INTEGER,INTENT(IN) :: it
          INTEGER :: itp
          REAL(KIND=8) :: TMP, time_sta, time_end

          TMP = 0.0
          IF (it == 1) THEN
            WRITE(*,*) '----------------------------------------------------'
            WRITE(*,*) '       PARTICLE TIME ITERATION PROCESS STARTED      '
            WRITE(*,*) ''
          END IF

          CALL CPU_TIME(time_sta)

          DO itp = 1,N_par
            CALL VEL_INTERPOLATION(itp)
            !------------------------------------------------------------------!
            !                   Calculate particles velocities                 !
            !------------------------------------------------------------------!
            CALL RUNGE_KUTTA(PAR_VEL_X,                                           &
                        particles(itp)%X_vel,interpol_vel(1),particles(itp)%X_vel)
            CALL RUNGE_KUTTA(PAR_VEL_Y,                                         &
                        particles(itp)%Y_vel,interpol_vel(2),particles(itp)%Y_vel)
            CALL RUNGE_KUTTA(PAR_VEL_Z,                                           &
                        particles(itp)%Z_vel,interpol_vel(3),particles(itp)%Z_vel)

            !------------------------------------------------------------------!
            !                  Calculate particles positions                   !
            !------------------------------------------------------------------!
            CALL RUNGE_KUTTA(PAR_POS,                                           &
                                    particles(itp)%X_vel,TMP,particles(itp)%X_pos)
            CALL RUNGE_KUTTA(PAR_POS,                                           &
                                    particles(itp)%Y_vel,TMP,particles(itp)%Y_pos)
            CALL RUNGE_KUTTA(PAR_POS,                                           &
                                    particles(itp)%Z_vel,TMP,particles(itp)%Z_pos)
          END DO

          CALL BOUNDARY_CONDITION

          CALL CPU_TIME(time_end)

          WRITE(*,"(A,1X,I6,2X,A,F10.6,A)")                                     &
                  'Particle iteration for',it,'time : ',time_end - time_sta,' s'
          Total_time = Total_time + (time_end - time_sta)

          IF (it == Nlast) THEN
            WRITE(*,*) ''
            WRITE(*,*) '        PARTICLE TIME ITERATION PROCESS ENDED       '
            WRITE(*,*) '  Total Iteration time : ',Total_time,' s'
            WRITE(*,*) '----------------------------------------------------'
            WRITE(*,*) ''
          END IF

        END SUBROUTINE SOLVER
