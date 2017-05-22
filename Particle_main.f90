!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : Particle_main.f90                                                !
!                                                                              !
!   PURPOSE : To simulate particles motions in the fluid                       !
!                                                                              !
!                                                             2017.05.16 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

        PROGRAM Particle_main
          USE numerical

          IMPLICIT NONE
          INTEGER :: it

          CALL SETUP
          CALL READ_DNS
          CALL PARTICLE_SETTING

          DO it = 1,Nlast
            CALL SOLVER
          END DO
          
        END PROGRAM Particle_main
