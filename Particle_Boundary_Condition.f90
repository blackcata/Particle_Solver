!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : Particle_Boundary_Condition.f90                                  !
!                                                                              !
!   PURPOSE : Boundary condition for each particles at                         !
!             periodic channel case.                                           !
!                                                                              !
!                                                             2017.05.22 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

        SUBROUTINE BOUNDARY_CONDITION
          USE mesh
          USE particle
          USE physical
          USE numerical

          IMPLICIT NONE

          INTEGER :: it

          DO it = 1,N_par
            particles(it)%age = particles(it)%age + dt
            !------------------------------------------------------------------!
            !             X-direction periodic boundary condition              !
            !------------------------------------------------------------------!
            IF ( particles(it)%X_pos > Lx ) THEN
              particles(it)%X_pos = particles(it)%X_pos - Lx
            ELSEIF ( particles(it)%X_pos < 0 ) THEN
              particles(it)%X_pos = particles(it)%X_pos + Lx
            END IF

            !------------------------------------------------------------------!
            !             Z-direction periodic boundary condition              !
            !------------------------------------------------------------------!
            IF ( particles(it)%Z_pos > Lz ) THEN
              particles(it)%Z_pos = particles(it)%Z_pos - Lz
            ELSEIF ( particles(it)%Z_pos < 0 ) THEN
              particles(it)%Z_pos = particles(it)%Z_pos + Lz
            END IF

            !------------------------------------------------------------------!
            !                  Y-direction boundary condition                  !
            !------------------------------------------------------------------!
            IF ( particles(it)%Y_pos < Y_eps ) THEN
                CALL PARTICLE_SETTING(it)
                particles(it)%age = 0.
            END IF

            IF ( particles(it)%Y_pos > x2(Ny) ) THEN
                CALL PARTICLE_SETTING(it)
                particles(it)%age = 0.
            END IF

          END DO

        END SUBROUTINE BOUNDARY_CONDITION
