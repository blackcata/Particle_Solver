!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : Particle_module.f90                                              !
!                                                                              !
!   PURPOSE : Module for particle solvers                                      !
!                                                                              !
!                                                             2017.05.16 K.Noh !
!                                                                              !
!   VARIABLES :                                                                !
!               N_par  : The number of particles                               !
!               RK_ord : Order of Runge-Kutta Method                           !
!               BC     : Boundary condition of particles                       !
!                                                                              !
!------------------------------------------------------------------------------!


        MODULE physical
          IMPLICIT NONE
          REAL(KIND=8) :: Lx, Ly, Lz, vol
          REAL(KIND=8) :: time_accu
          REAL(KIND=8) :: vper
          REAL(KIND=8), PARAMETER :: PI=ACOS(-1.0)
          REAL(KIND=8), PARAMETER :: g = 9.81       ! Add by K.Noh 2017
          REAL(KIND=8), PARAMETER :: e = 1.         ! Add by K.Noh 2017
          REAL(KIND=8), PARAMETER :: tau_p = 1.0    ! Add by K.Noh 2017
        END MODULE physical

        MODULE mesh
          IMPLICIT NONE
          INTEGER :: Nx, Ny, Nz, NxM, NyM, NzM
          REAL(KIND=8) :: dx, dz, dx_sq, dz_sq
          REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: y, dy, dym, dyc, dyp, dy_sq
          REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: h, hm, hc, hp
          REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: x1,x2,x3
          REAL(KIND=8), PARAMETER :: gamma=1.9
          INTEGER, ALLOCATABLE, DIMENSION(:) :: ipa, ima, kpa, kma,             &
                                                jpa, jma, jmu, jmv
        END MODULE mesh

        MODULE numerical
          IMPLICIT none
          INTEGER      :: Ninit, Nlast, Ntime
          REAL(KIND=8) :: NxzM
          REAL(KIND=8) :: dt
          REAL(KIND=8) :: IDtopt, CFL_std, CFL_max
          REAL(KIND=8) :: Re
          REAL(KIND=8) :: Fr                        ! Add by K.Noh 2017
          REAL(KIND=8) :: divU_max
        END MODULE numerical

        MODULE field
          IMPLICIT NONE
          INTEGER :: Nread
          REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:,:,:) :: U, UH
          REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:,:) :: P, dP
          REAL(KIND=8) :: PRESGx, PRESGz
          REAL(KIND=8) :: FRx, FRz
        END MODULE field

        MODULE particle
          INTEGER :: N_par, RK_ord, BC

          TYPE particle_type
            INTEGER :: par_num                     ! Particles number
            REAL(KIND=8) :: X_pos, Y_pos, Z_pos, & ! Particles position
                            X_vel, Y_vel, Z_vel    ! Particles velocities
          END TYPE particle_type

          TYPE(particle_type), ALLOCATABLE, DIMENSION(:) :: particles
        END MODULE particle

        MODULE fileout
          IMPLICIT none
          INTEGER :: Nfile, IDins
          INTEGER :: Nx1,Nx2,Nx3,Nx4
          INTEGER :: Ny1,Ny2,Ny3,Ny4
          INTEGER :: Nz1,Nz2,Nz3,Nz4
          INTEGER :: pathID, mkdirID
          CHARACTER(LEN=80) :: path, pathname, filename, dirname
        END MODULE fileout
