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
!               Y_eps  : Y distance criteria of boundary distance
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
          REAL(KIND=8) :: interpol_vel(1:3), Y_eps

          TYPE particle_type
            INTEGER :: par_num                     ! Particles number
            REAL(KIND=8) :: X_pos, Y_pos, Z_pos, & ! Particles position
                            X_vel, Y_vel, Z_vel, & ! Particles velocities
                            age, radius

          END TYPE particle_type

          TYPE(particle_type), ALLOCATABLE, DIMENSION(:) :: particles

          CONTAINS
          !--------------------------------------------------------------------!
          !                 Velocity Interpolation  Subroutine                 !
          !--------------------------------------------------------------------!
            SUBROUTINE VEL_INTERPOLATION(it)
              USE mesh
              USE field

              IMPLICIT NONE
              INTEGER,INTENT(IN) :: it
              INTEGER :: index_x, index_y, index_z, t_tmp, i,j,k, ind
              REAL(KIND=8) :: x_dis_f,x_dis_b, y_dis_d,y_dis_u, z_dis_l,z_dis_r,&
                              y_tmp_d, y_tmp_u, u_loc(1:8), u_tmp(1:6)

              !----------------------------------------------------------------!
              !                Find nearest index of velocities                !
              !----------------------------------------------------------------!
              index_x = INT(particles(it)%X_pos/dx)
              index_z = INT(particles(it)%Z_pos/dz)
              index_y = INT( Ny/2*(1 -                                          &
                         atanh((1-particles(it)%Y_pos)*tanh(gamma))/gamma) + 1 )

              !----------------------------------------------------------------!
              !           Find the distance between two cell-walls             !
              !----------------------------------------------------------------!
              x_dis_b = ( particles(it)%X_pos - index_x*dx )/dx
              x_dis_f = ( (index_x+1)*dx - particles(it)%X_pos )/dx

              z_dis_r = ( particles(it)%Z_pos - index_z*dz )/dz
              z_dis_l = ( (index_z+1)*dz - particles(it)%Z_pos )/dz

              y_tmp_d   = 1 - tanh(gamma*(1-2*REAL(index_y-1)/Ny))/tanh(gamma)
              y_tmp_u   = 1 - tanh(gamma*(1-2*REAL(index_y+1-1)/Ny))/tanh(gamma)

              y_dis_d = ( particles(it)%Y_pos - y_tmp_d )/( y_tmp_u - y_tmp_d )
              y_dis_u = ( y_tmp_u - particles(it)%Y_pos )/( y_tmp_u - y_tmp_d )

              interpol_vel(1:3) = 0.
              u_tmp(1:6)        = 0.
              u_loc(1:8)        = 0.

              !----------------------------------------------------------------!
              !               Calculate interpolated velocities                !
              !----------------------------------------------------------------!
              DO t_tmp = 1,3
                ind = 0
                DO k = 0,1
                  Do j = 0,1
                    DO i = 0,1
                      ind = ind + 1
                      u_loc(ind) = U(t_tmp,index_x+i,index_y+j,index_z+k)
                    END DO
                  END DO
                END DO

                u_tmp(1) = x_dis_f * u_loc(1) + x_dis_b * u_loc(2)
                u_tmp(2) = x_dis_f * u_loc(3) + x_dis_b * u_loc(4)
                u_tmp(3) = x_dis_f * u_loc(5) + x_dis_b * u_loc(6)
                u_tmp(4) = x_dis_f * u_loc(7) + x_dis_b * u_loc(8)

                u_tmp(5) = z_dis_r * u_tmp(1) + z_dis_l * u_tmp(2)
                u_tmp(6) = z_dis_r * u_tmp(3) + z_dis_l * u_tmp(4)

                interpol_vel(t_tmp) = y_dis_u*u_tmp(5) + y_dis_d*u_tmp(6)
              END DO

            END SUBROUTINE VEL_INTERPOLATION

          !--------------------------------------------------------------------!
          !                    Runge Kutta Method Subroutine                   !
          !--------------------------------------------------------------------!
            SUBROUTINE RUNGE_KUTTA(FUNC,input_1,input_2,output)
              USE numerical,                                                    &
                ONLY : dt

              IMPLICIT NONE
              REAL(KIND=8),INTENT(IN) :: input_1, input_2
              REAL(KIND=8),INTENT(INOUT) :: output
              REAL(KIND=8) :: FUNC, k1, k2, k3, k4

              !----------------------------------------------------------------!
              !                 Calculate x-direction velocity                 !
              !----------------------------------------------------------------!
              k1 = FUNC(input_1,input_2)
              k2 = FUNC(input_1 + dt*k1/2,input_2)
              k3 = FUNC(input_1 + dt*k2/2,input_2)
              k4 = FUNC(input_1 + dt*k3,input_2)

              output = output + (k1 + 2*k2 + 2*k3 + k4)*dt/6

            END SUBROUTINE RUNGE_KUTTA
          !--------------------------------------------------------------------!
          !           Particle velocity & position equation functions          !
          !--------------------------------------------------------------------!
            REAL(KIND=8) FUNCTION PAR_VEL(vel,interpol_vel)
              USE physical
              IMPLICIT NONE
              REAL(KIND=8), INTENT(IN) :: vel, interpol_vel

              PAR_VEL = - (vel - interpol_vel)/tau_p
            END FUNCTION PAR_VEL

            REAL(KIND=8) FUNCTION PAR_POS(vel,interpol_vel)
              USE physical
              IMPLICIT NONE
              REAL(KIND=8), INTENT(IN) :: vel, interpol_vel

              PAR_POS= vel
            END FUNCTION PAR_POS

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
