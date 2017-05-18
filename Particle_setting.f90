!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : Particle_setting.f90                                             !
!                                                                              !
!   PURPOSE : Setup each particles characteristic including, particle number   !
!             positions and velocities of each directions.                     !
!                                                                              !
!                                                             2017.05.17 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

        SUBROUTINE PARTICLE_SETTING
            USE mesh
            USE particle

            IMPLICIT NONE

            INTEGER :: it
            REAL(KIND=8) :: X_start, X_end, Y_start, Y_end, Z_start, Z_end,     &
                            time_sta, time_end, tmp(1:3), Y_tmp

            WRITE(*,*) '----------------------------------------------------'
            WRITE(*,*) '          PARTICLE SETTING PROCESS STARTED          '
            CALL CPU_TIME(time_sta)


            !------------------------------------------------------------------!
            !                  Make & Initialize Result folder                 !
            !------------------------------------------------------------------!
            X_start = x1(1)  ; Y_start = x2(1)  ; Z_start = x3(1)
            X_end   = x1(Nx) ; Y_end   = x2(Ny) ; Z_end   = x3(Nz)

            CALL RANDOM_SEED

            DO it = 1,N_par
              particles(it)%par_num = it

              CALL RANDOM_NUMBER(tmp)
              particles(it)%X_pos = X_start + (X_end-X_start)*tmp(1)
              particles(it)%Y_pos = Y_start + (Y_end-Y_start)*tmp(2)
              particles(it)%Z_pos = Z_start + (Z_end-Z_start)*tmp(3)

              Y_tmp               = particles(it)%Y_pos
              particles(it)%X_vel = Y_tmp * ( x2(Ny)- Y_tmp )
            END DO

            CALL CPU_TIME(time_end)

            WRITE(*,*) '       PARTICLE SETTING PROCESS IS COMPLETED        '
            WRITE(*,*) '  Total Setting time : ',time_end - time_sta,' s'
            WRITE(*,*) '----------------------------------------------------'
            WRITE(*,*) ''
        END SUBROUTINE PARTICLE_SETTING