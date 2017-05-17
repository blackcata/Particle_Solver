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

          IMPLICIT NONE

          CALL SETUP
          CALL READ_DNS
          CALL PARTICLE_SETTING
          
        END PROGRAM Particle_main
