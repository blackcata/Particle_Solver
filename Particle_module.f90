!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : Particle_module.f90                                              !
!                                                                              !
!   PURPOSE : Module for particle solvers                                      !
!                                                                              !
!                                                             2017.05.16 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!


        MODULE physical
          IMPLICIT NONE
          REAL(KIND=8) :: Lx, Ly, Lz, vol
          REAL(KIND=8) :: time_accu
          REAL(KIND=8) :: vper
          REAL(KIND=8), PARAMETER :: PI=ACOS(-1.0)
          REAL(KIND=8), PARAMETER :: g = 9.81
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

        MODULE particles
          TYPE particle_type
            INTEGER :: par_num                     ! Particles number
            REAL(KIND=8) :: X_pos, Y_pos, Z_pos, & ! Particles position
                            X_vel, Y_vel, Z_vel    ! Particles velocities
          END TYPE particle_type

        END MODULE particles
