module geometry
  !! Provides routines for basic geometrical and trigonometric functions in 2D and 3D.
  
  implicit none

  public :: norm, distance
  
contains

  pure real function norm(p) result(d)
    !! Returns the \(L_2\)-norm of a vector \(\mathbf{p}\),
    !! $$ \lVert \mathbf{p} \rVert = \sqrt{\sum_{i=1}^n p_i} $$

    implicit none

    real, allocatable, intent(in) :: p(:)
    integer :: l, u, i
    
    l = lbound(p,1)
    u = ubound(p,1)

    d = 0.0
    do i = l, u
       d = d + p(i)**2
    end do

    d = sqrt(d)

  end function norm
  
  pure real function distance(p, q) result(d)
    !! Returns the Euclidean distance between two points \(\mathbf{x}\) and \(\mathbf{y}\), 
    !! $$ \lVert \mathbf{q} - \mathbf{p} \rVert = \sqrt{\sum_{i=1}^n \left(q_i - p_i\right)^2} $$ 
    
    implicit none

    real, allocatable, intent(in) :: p(:), q(:)
    integer :: l, u, i

    !! @todo Check rank of p vs q
    l = lbound(p,1)
    u = ubound(p,1)

    d = 0.0
    do i = l, u
       d = d + (q(i) - p(i))**2
    end do

    d = sqrt(d)
    
  end function distance
end module geometry
