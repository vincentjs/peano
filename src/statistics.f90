module statistics
  !! Provides routines for basic statistical methods. 
  
  implicit none
  
  public :: factorial, mean, variance, standardDeviation, covariance

  public

  interface median
     module procedure median_real, median_int
  end interface median
  
contains

  pure integer function factorial(x) result(xfac)
    !! Returns the factorial of the scalar value \(\mathbf{x}\),
    !! $$ x! = n(n-1)(n-2)\cdots(3)(2)(1) $$

    integer, intent(in) :: x

    integer :: i

    xfac = 1
    if (x >= 0) then
       do i = x, 1, -1
          xfac = xfac * i
       end do
    end if

  end function factorial

  pure real function mean(x) result(mu)
    !! Returns the arithmatic mean of a vector \(\mathbf{x}\).
    !! $$ \mu = \frac{1}{n} \sum_{i=1}^n x_i $$

    implicit none

    real, intent(in) :: x(:)

    real :: Sx
    integer :: n, i, l, u

    mu = 0.0
    Sx = 0.0

    n = size(x)
    l = lbound(x,1)
    u = ubound(x,1)

    do i = l, u
       Sx = Sx + x(i)
    end do

    mu = Sx / n

  end function mean
  
  pure integer function median_int(x) result(xm)

    integer, intent(in) :: x(:)

    xm = ceiling(size(x)/2.0)
       
  end function median_int
  
  pure integer function median_real(x) result (xm)

    real, intent(in) :: x(:)
    
    xm = ceiling(size(x)/2.0)
  
  end function median_real

  pure real function variance(x) result(ss)
    !! Returns the sample variance of the vector \(\mathbf{x}\),
    !! $$ s^2 = \frac{1}{n-1} \sum_{i=1}^n \left(x_i - \mu_x \right)^2 $$

    implicit none

    real, intent(in) :: x(:)

    integer :: n, i, l, u
    real :: xb

    n = size(x)

    xb = mean(x)
    ss = 0.0

    l = lbound(x,1)
    u = ubound(x,1)
    do i = l, u
       ss = ss + (x(i) - xb)**2
    end do

    ss = 1 / (n - 1) * ss

  end function variance

  pure real function standardDeviation(x) result(s)
    !! Returns the sample standard deviation of a vector \(\mathbf{x}\),
    !! $$ s = \sqrt{\frac{1}{n-1} \sum_{i=1}^n \left(x_i - \mu_x \right)^2} $$

    implicit none

    real, intent(in) :: x(:)

    s = sqrt(variance(x))

  end function standardDeviation

  pure real function covariance(x, y) result(sigma)
    !! Returns the covariance of two vectors \(\mathbf{x}\) and \(\mathbf{y}\),
    !! $$ \sigma(x,y) =\frac{1}{n-1} \sum_{i=1}^n \left(x_i - \mu_x \right) \left(y_i-\mu_y\right) $$

    implicit none

    real, intent(in) :: x(:), y(:)

    real :: Sx, Sy, xb, yb
    integer :: n, i, l, u

    !!@todo If x and y aren't same size, return error
    !!@todo Handle x and y not same bound start/end
    sigma = 0.0

    
    n = size(x)

    xb = mean(x)
    yb = mean(y)

    l = lbound(x,1)
    u = ubound(x,1)
    do i = l, u
       Sx = x(i) - xb
       Sy = y(i) - yb
       sigma = sigma + (Sx*Sy / n)
    end do

  end function covariance

end module statistics

