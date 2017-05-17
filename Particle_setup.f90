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

          IMPLICIT NONE

          Nx = 128
          Ny = 191
          Nz = 159
          Nlast = 100

          Lx = 4.*PI
          Lz = 2.*PI

          dt = 0.005

          N_par  = 100
          RK_ord = 3
          BC     = 1

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

        END SUBROUTINE SETUP
