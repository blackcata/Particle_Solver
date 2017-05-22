!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : Particle_write.f90                                               !
!                                                                              !
!   PURPOSE : Write each variables in the RESULT folder.                       !
!                                                                              !
!                                                             2017.05.22 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
          SUBROUTINE OUTPUT(it)
              USE mesh
              USE fileout
              USE particle
              USE numerical

              IMPLICIT NONE
              INTEGER,INTENT(IN) :: it
              INTEGER :: itp

              dirname = 'RESULT'
              !----------------------------------------------------------------!
              !               Outputs for particle informations                !
              !----------------------------------------------------------------!
              filename = '/Particle_vel.plt'
              pathname = TRIM(dirname)//TRIM(filename)

              OPEN(100,FILE=pathname,FORM='FORMATTED',POSITION='APPEND')
              WRITE(100,*) 'VARIABLES = X,Y,Z'
              WRITE(100,*) 'ZONE'
              WRITE(100,*) 'SOLUTIONTIME =',it*dt

              DO itp = 1,N_par
                WRITE(100,"(3F15.9)") particles(itp)%X_pos, particles(itp)%Y_pos, &
                                      particles(itp)%Z_pos!,                      &
                                      ! particles(itp)%X_vel, particles(itp)%Y_vel, &
                                      ! particles(itp)%Z_vel
              END DO
              WRITE(100,*)
              CLOSE(100)

          END SUBROUTINE OUTPUT
