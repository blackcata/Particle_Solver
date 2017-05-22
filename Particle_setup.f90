!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : Particle_setup.f90                                               !
!                                                                              !
!   PURPOSE : Setup for particle solvers                                       !
!                                                                              !
!                                                             2017.05.16 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

        SUBROUTINE SETUP
          USE mesh
          USE field
          USE numerical
          USE physical
          USE particle
          USE fileout

          IMPLICIT NONE
          INTEGER :: it

          !--------------------------------------------------------------------!
          !                   Make & Initialize Result folder                  !
          !--------------------------------------------------------------------!
          dirname  = 'RESULT'
          CALL SYSTEM('mkdir '//TRIM(dirname))
          CALL SYSTEM('rm -rf ./'//TRIM(dirname)//'/*.plt')

          !--------------------------------------------------------------------!
          !                   Constants for particle tracking                  !
          !--------------------------------------------------------------------!
          Nx = 127
          Ny = 190
          Nz = 158
          Nlast = 1000

          Lx = 4.*PI
          Lz = 2.*PI

          dx = Lx / Nx
          dz = Lz / Nz
          dt = 0.005

          N_par  = 300
          RK_ord = 3
          BC     = 1

          Y_eps = 1e-6
          Total_time = 0.0

          !------------------------------------------------------------------!
          !                         Allocate variables                       !
          !------------------------------------------------------------------!
          ALLOCATE ( x1(0:Nx), x2(0:Ny), x3(0:Nz) )
          ALLOCATE ( U(3,0:Nx,0:Ny,0:Nz) )
          ALLOCATE ( particles(1:N_par) )

          !------------------------------------------------------------------!
          !                         Initial Conditions                       !
          !------------------------------------------------------------------!
          x1(0:Nx) = 0.0 ; x2(0:Ny) = 0.0 ; x3(0:Nz) = 0.0
          U(1:3,0:Nx,0:Ny,0:Nz) = 0.0

          particles(1:N_par)%par_num = 0
          particles(1:N_par)%X_pos   = 0.
          particles(1:N_par)%Y_pos   = 0.
          particles(1:N_par)%Z_pos   = 0.
          particles(1:N_par)%X_vel   = 0.
          particles(1:N_par)%Y_vel   = 0.
          particles(1:N_par)%Z_vel   = 0.
          particles(1:N_par)%age     = 0.
          particles(1:N_par)%radius  = 1.


          DO it = 1,N_par
            CALL PARTICLE_SETTING(it)
          END DO

        END SUBROUTINE SETUP
