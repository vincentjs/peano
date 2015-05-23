var tipuesearch = {"pages":[{"text":"peano A hodgepodge of mathematical routines written in modern Fortran. It provides functions for statistics, linear algebra, interpolation, search and sorts, root finding, fourier transforms, and differential equations. Some routines have been parallelized. This project's codename refers to Guiseppe Peano , a prolific late 19th century mathematician who contributed much to set theory and mathematical logic, as well as the fields of calculus and differential equations. In perhaps his most famous contribution, Peano introduces the definition of the natural numbers in terms of sets, resulting in a set of axioms that have become a cornerstone in modern set theory and mathematical induction. This project is licensed under the MIT License, and can be obtained at the project's github page, https://github.com/vincentjs/peano. Developer Info Vincent San Miguel","tags":"home","loc":"index.html","title":" peano "},{"text":"Procedures Procedure Location Procedure Type Description covariance statistics Function Returns the covariance of two vectors \\mathbf{x} and \\mathbf{y},\n  \\sigma(x,y) =\\frac{1}{n-1} \\sum_{i=1}&#94;n \\left(x_i - \\mu_x \\right) \\left(y_i-\\mu_y\\right)  distance geometry Function Returns the Euclidean distance between two points \\mathbf{x} and \\mathbf{y}, \n  \\lVert \\mathbf{q} - \\mathbf{p} \\rVert = \\sqrt{\\sum_{i=1}&#94;n \\left(q_i - p_i\\right)&#94;2}  factorial statistics Function Returns the factorial of the scalar value \\mathbf{x},\n  x! = n(n-1)(n-2)\\cdots(3)(2)(1)  mean statistics Function Returns the arithmatic mean of a vector \\mathbf{x}.\n  \\mu = \\frac{1}{n} \\sum_{i=1}&#94;n x_i  median statistics Function Returns the median of a vector. If the vector has an odd number\n of elements, the \"mean of middle\" two is returned. norm geometry Function Returns the L_2-norm of a vector \\mathbf{p},\n  \\lVert \\mathbf{p} \\rVert = \\sqrt{\\sum_{i=1}&#94;n p_i}  standardDeviation statistics Function Returns the sample standard deviation of a vector \\mathbf{x},\n  s = \\sqrt{\\frac{1}{n-1} \\sum_{i=1}&#94;n \\left(x_i - \\mu_x \\right)&#94;2}  variance statistics Function Returns the sample variance of the vector \\mathbf{x},\n  s&#94;2 = \\frac{1}{n-1} \\sum_{i=1}&#94;n \\left(x_i - \\mu_x \\right)&#94;2 ","tags":"list procedures","loc":"lists/procedures.html","title":"\nAll Procedures – peano\n"},{"text":"Source Files File Description assert.f90 error.f90 geometry.f90 kdtree.f90 statistics.f90","tags":"list files","loc":"lists/files.html","title":"\nAll Files – peano\n"},{"text":"Modules Module Source File Description assert assert.f90 Development tool used to ensure that predicates are as expected before proceeding. If an assertion fails, the error message will be logged and the program terminated. error error.f90 Development tool used to add and print error messages, as well as handle logic flow in the event an error is thrown. geometry geometry.f90 Provides routines for basic geometrical and trigonometric functions in 2D and 3D. kdtree kdtree.f90 Provides routines for a k-dimensional tree, a space-partitioning data structure used for organizing points in k-dimensional space. Note that nearest neightbor searches within k-d trees are not suitable for high dimensional space and will likely perform no better than an exhaustive search. As a general rule of thumb, N \\gg 2&#94;k. statistics statistics.f90 Provides routines for basic statistical methods.","tags":"list modules","loc":"lists/modules.html","title":"\nAll Modules – peano\n"},{"text":"statistics.f90 Source File Source File statistics.f90 Modules statistics All Source Files assert.f90 error.f90 geometry.f90 kdtree.f90 statistics.f90 module statistics !! Provides routines for basic statistical methods. implicit none public :: factorial , mean , variance , standardDeviation , covariance contains pure integer function factorial ( x ) result ( xfac ) !! Returns the factorial of the scalar value \\mathbf{x}, !!  x! = n(n-1)(n-2)\\cdots(3)(2)(1)  integer , intent ( in ) :: x integer :: i xfac = 1 if ( x >= 0 ) then do i = x , 1 , - 1 xfac = xfac * i end do end if end function factorial pure real function mean ( x ) result ( mu ) !! Returns the arithmatic mean of a vector \\mathbf{x}. !!  \\mu = \\frac{1}{n} \\sum_{i=1}&#94;n x_i  implicit none real , allocatable , intent ( in ) :: x (:) real :: Sx integer :: n , i , l , u mu = 0.0 Sx = 0.0 if ( allocated ( x )) then n = size ( x ) l = lbound ( x , 1 ) u = ubound ( x , 1 ) do i = l , u Sx = Sx + x ( i ) end do mu = Sx / n end if end function mean pure real function median ( x ) result ( xm ) !! Returns the median of a vector. If the vector has an odd number !! of elements, the \"mean of middle\" two is returned. implicit none real , allocatable , intent ( in ) :: x (:) integer :: n , nd2 real :: xmm1 , xmp1 n = size ( x ) nd2 = n / 2 if ( mod ( n , 2 ) . eq . 0 ) then xm = x ( nd2 ) else xmm1 = x ( nd2 - 1 ) xmp1 = x ( nd2 + 1 ) xm = ( xmm1 + xmp1 ) / 2.0 end if end function median pure real function variance ( x ) result ( ss ) !! Returns the sample variance of the vector \\mathbf{x}, !!  s&#94;2 = \\frac{1}{n-1} \\sum_{i=1}&#94;n \\left(x_i - \\mu_x \\right)&#94;2  implicit none real , allocatable , intent ( in ) :: x (:) integer :: n , i , l , u real :: xb n = size ( x ) xb = mean ( x ) ss = 0.0 if ( allocated ( x )) then l = lbound ( x , 1 ) u = ubound ( x , 1 ) do i = l , u ss = ss + ( x ( i ) - xb ) ** 2 end do ss = 1 / ( n - 1 ) * ss end if end function variance pure real function standardDeviation ( x ) result ( s ) !! Returns the sample standard deviation of a vector \\mathbf{x}, !!  s = \\sqrt{\\frac{1}{n-1} \\sum_{i=1}&#94;n \\left(x_i - \\mu_x \\right)&#94;2}  implicit none real , allocatable , intent ( in ) :: x (:) s = sqrt ( variance ( x )) end function standardDeviation pure real function covariance ( x , y ) result ( sigma ) !! Returns the covariance of two vectors \\mathbf{x} and \\mathbf{y}, !!  \\sigma(x,y) =\\frac{1}{n-1} \\sum_{i=1}&#94;n \\left(x_i - \\mu_x \\right) \\left(y_i-\\mu_y\\right)  implicit none real , allocatable , intent ( in ) :: x (:), y (:) real :: Sx , Sy , xb , yb integer :: n , i , l , u !!@todo If x and y aren't same size, return error !!@todo Handle x and y not same bound start/end sigma = 0.0 if ( allocated ( x ). and . allocated ( y )) then n = size ( x ) xb = mean ( x ) yb = mean ( y ) l = lbound ( x , 1 ) u = ubound ( x , 1 ) do i = l , u Sx = x ( i ) - xb Sy = y ( i ) - yb sigma = sigma + ( Sx * Sy / n ) end do end if end function covariance end module statistics © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"sourcefile/statistics.f90.html","title":"statistics.f90 – peano"},{"text":"assert.f90 Source File Source File assert.f90 Modules assert All Source Files assert.f90 error.f90 geometry.f90 kdtree.f90 statistics.f90 module assert !! Development tool used to ensure that predicates are as expected before proceeding. If an assertion fails, the error message will be logged and the program terminated. use error implicit none private contains subroutine assert_same_rank ( x , y ) real , allocatable , intent ( in ) :: x (:), y (:) logical :: isSameRank character ( len = 256 ) :: err_s isSameRank = is_same_rank ( x , y ) if (. not . isSameRank ) then err_s = \"Assertion error - ranks are not equivalent.\" call add_error_message ( err_s ) call print_error_list_to_shell () call terminate_with_failure () end if end subroutine assert_same_rank pure logical function is_same_rank ( x , y ) result ( res ) real , allocatable , intent ( in ) :: x (:), y (:) if ( size ( x ) == size ( y )) then res = . true . else res = . false . end if end function is_same_rank end module assert © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"sourcefile/assert.f90.html","title":"assert.f90 – peano"},{"text":"geometry.f90 Source File Source File geometry.f90 Modules geometry All Source Files assert.f90 error.f90 geometry.f90 kdtree.f90 statistics.f90 module geometry !! Provides routines for basic geometrical and trigonometric functions in 2D and 3D. implicit none public :: norm , distance contains pure real function norm ( p ) result ( d ) !! Returns the L_2-norm of a vector \\mathbf{p}, !!  \\lVert \\mathbf{p} \\rVert = \\sqrt{\\sum_{i=1}&#94;n p_i}  implicit none real , allocatable , intent ( in ) :: p (:) integer :: l , u , i l = lbound ( p , 1 ) u = ubound ( p , 1 ) d = 0.0 do i = l , u d = d + p ( i ) ** 2 end do d = sqrt ( d ) end function norm pure real function distance ( p , q ) result ( d ) !! Returns the Euclidean distance between two points \\mathbf{x} and \\mathbf{y}, !!  \\lVert \\mathbf{q} - \\mathbf{p} \\rVert = \\sqrt{\\sum_{i=1}&#94;n \\left(q_i - p_i\\right)&#94;2}  implicit none real , allocatable , intent ( in ) :: p (:), q (:) integer :: l , u , i !! @todo Check rank of p vs q l = lbound ( p , 1 ) u = ubound ( p , 1 ) d = 0.0 do i = l , u d = d + ( q ( i ) - p ( i )) ** 2 end do d = sqrt ( d ) end function distance end module geometry © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"sourcefile/geometry.f90.html","title":"geometry.f90 – peano"},{"text":"error.f90 Source File Source File error.f90 Modules error All Source Files assert.f90 error.f90 geometry.f90 kdtree.f90 statistics.f90 module error !! Development tool used to add and print error messages, as well as handle logic flow in the event an error is thrown. implicit none private character ( len = 1024 ) :: error_list ( 1 : 100 ) integer :: err_i = 0 contains subroutine add_error_message ( err_s ) character ( len =* ), intent ( in ) :: err_s err_i = err_i + 1 error_list ( err_i ) = \"Error: \" // trim ( err_s ) end subroutine add_error_message subroutine print_error_list_to_shell () integer :: i , l , u l = lbound ( error_list , 1 ) u = ubound ( error_list , 1 ) do i = l , u write ( 6 , * ) error_list ( i ) end do end subroutine print_error_list_to_shell subroutine terminate_with_failure () error stop 1 end subroutine terminate_with_failure end module error © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"sourcefile/error.f90.html","title":"error.f90 – peano"},{"text":"kdtree.f90 Source File Source File kdtree.f90 Modules kdtree All Source Files assert.f90 error.f90 geometry.f90 kdtree.f90 statistics.f90 module kdtree !! Provides routines for a k-dimensional tree, a space-partitioning data structure used for organizing points in k-dimensional space. Note that nearest neightbor searches within k-d trees are not suitable for high dimensional space and will likely perform no better than an exhaustive search. As a general rule of thumb, N \\gg 2&#94;k. contains end module kdtree © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"sourcefile/kdtree.f90.html","title":"kdtree.f90 – peano"},{"text":"factorial Function Source File statistics.f90 statistics factorial Variables i All Procedures covariance distance factorial mean median norm standardDeviation variance public pure function factorial(x) Arguments Type Intent Optional Attributes Name integer, intent(in) :: x Return Value integer Description Returns the factorial of the scalar value \\mathbf{x},\n  x! = n(n-1)(n-2)\\cdots(3)(2)(1)  Variables Type Visibility Attributes Name Initial integer, public :: i © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"proc/factorial.html","title":"factorial – peano"},{"text":"mean Function Source File statistics.f90 statistics mean Variables Sx n i l u All Procedures covariance distance factorial mean median norm standardDeviation variance public pure function mean(x) Arguments Type Intent Optional Attributes Name real, intent(in), allocatable :: x (:) Return Value real Description Returns the arithmatic mean of a vector \\mathbf{x}.\n  \\mu = \\frac{1}{n} \\sum_{i=1}&#94;n x_i  Variables Type Visibility Attributes Name Initial real, public :: Sx integer, public :: n integer, public :: i integer, public :: l integer, public :: u © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"proc/mean.html","title":"mean – peano"},{"text":"median Function Source File statistics.f90 statistics median Variables n nd2 xmm1 xmp1 All Procedures covariance distance factorial mean median norm standardDeviation variance public pure function median(x) Arguments Type Intent Optional Attributes Name real, intent(in), allocatable :: x (:) Return Value real Description Returns the median of a vector. If the vector has an odd number\n of elements, the \"mean of middle\" two is returned. Variables Type Visibility Attributes Name Initial integer, public :: n integer, public :: nd2 real, public :: xmm1 real, public :: xmp1 © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"proc/median.html","title":"median – peano"},{"text":"variance Function Source File statistics.f90 statistics variance Variables n i l u xb All Procedures covariance distance factorial mean median norm standardDeviation variance public pure function variance(x) Arguments Type Intent Optional Attributes Name real, intent(in), allocatable :: x (:) Return Value real Description Returns the sample variance of the vector \\mathbf{x},\n  s&#94;2 = \\frac{1}{n-1} \\sum_{i=1}&#94;n \\left(x_i - \\mu_x \\right)&#94;2  Variables Type Visibility Attributes Name Initial integer, public :: n integer, public :: i integer, public :: l integer, public :: u real, public :: xb © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"proc/variance.html","title":"variance – peano"},{"text":"standardDeviation Function Source File statistics.f90 statistics standardDeviation All Procedures covariance distance factorial mean median norm standardDeviation variance public pure function standardDeviation(x) Arguments Type Intent Optional Attributes Name real, intent(in), allocatable :: x (:) Return Value real Description Returns the sample standard deviation of a vector \\mathbf{x},\n  s = \\sqrt{\\frac{1}{n-1} \\sum_{i=1}&#94;n \\left(x_i - \\mu_x \\right)&#94;2}  © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"proc/standarddeviation.html","title":"standardDeviation – peano"},{"text":"covariance Function Source File statistics.f90 statistics covariance Variables Sx Sy xb yb n i l u All Procedures covariance distance factorial mean median norm standardDeviation variance public pure function covariance(x, y) Arguments Type Intent Optional Attributes Name real, intent(in), allocatable :: x (:) real, intent(in), allocatable :: y (:) Return Value real Description Returns the covariance of two vectors \\mathbf{x} and \\mathbf{y},\n  \\sigma(x,y) =\\frac{1}{n-1} \\sum_{i=1}&#94;n \\left(x_i - \\mu_x \\right) \\left(y_i-\\mu_y\\right)  Variables Type Visibility Attributes Name Initial real, public :: Sx real, public :: Sy real, public :: xb real, public :: yb integer, public :: n integer, public :: i integer, public :: l integer, public :: u ToDo If x and y aren't same size, return error ToDo Handle x and y not same bound start/end © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"proc/covariance.html","title":"covariance – peano"},{"text":"norm Function Source File geometry.f90 geometry norm Variables l u i All Procedures covariance distance factorial mean median norm standardDeviation variance public pure function norm(p) Arguments Type Intent Optional Attributes Name real, intent(in), allocatable :: p (:) Return Value real Description Returns the L_2-norm of a vector \\mathbf{p},\n  \\lVert \\mathbf{p} \\rVert = \\sqrt{\\sum_{i=1}&#94;n p_i}  Variables Type Visibility Attributes Name Initial integer, public :: l integer, public :: u integer, public :: i © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"proc/norm.html","title":"norm – peano"},{"text":"distance Function Source File geometry.f90 geometry distance Variables l u i All Procedures covariance distance factorial mean median norm standardDeviation variance public pure function distance(p, q) Arguments Type Intent Optional Attributes Name real, intent(in), allocatable :: p (:) real, intent(in), allocatable :: q (:) Return Value real Description Returns the Euclidean distance between two points \\mathbf{x} and \\mathbf{y}, \n  \\lVert \\mathbf{q} - \\mathbf{p} \\rVert = \\sqrt{\\sum_{i=1}&#94;n \\left(q_i - p_i\\right)&#94;2}  Variables Type Visibility Attributes Name Initial integer, public :: l integer, public :: u integer, public :: i ToDo Check rank of p vs q © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"proc/distance.html","title":"distance – peano"},{"text":"statistics Module Source File statistics.f90 statistics Functions factorial mean median variance standardDeviation covariance All Modules assert error geometry kdtree statistics Provides routines for basic statistical methods. Functions public pure function factorial (x) Arguments Type Intent Optional Attributes Name integer, intent(in) :: x Return Value integer Description Returns the factorial of the scalar value \\mathbf{x},\n  x! = n(n-1)(n-2)\\cdots(3)(2)(1)  public pure function mean (x) Arguments Type Intent Optional Attributes Name real, intent(in), allocatable :: x (:) Return Value real Description Returns the arithmatic mean of a vector \\mathbf{x}.\n  \\mu = \\frac{1}{n} \\sum_{i=1}&#94;n x_i  public pure function median (x) Arguments Type Intent Optional Attributes Name real, intent(in), allocatable :: x (:) Return Value real Description Returns the median of a vector. If the vector has an odd number\n of elements, the \"mean of middle\" two is returned. public pure function variance (x) Arguments Type Intent Optional Attributes Name real, intent(in), allocatable :: x (:) Return Value real Description Returns the sample variance of the vector \\mathbf{x},\n  s&#94;2 = \\frac{1}{n-1} \\sum_{i=1}&#94;n \\left(x_i - \\mu_x \\right)&#94;2  public pure function standardDeviation (x) Arguments Type Intent Optional Attributes Name real, intent(in), allocatable :: x (:) Return Value real Description Returns the sample standard deviation of a vector \\mathbf{x},\n  s = \\sqrt{\\frac{1}{n-1} \\sum_{i=1}&#94;n \\left(x_i - \\mu_x \\right)&#94;2}  public pure function covariance (x, y) Arguments Type Intent Optional Attributes Name real, intent(in), allocatable :: x (:) real, intent(in), allocatable :: y (:) Return Value real Description Returns the covariance of two vectors \\mathbf{x} and \\mathbf{y},\n  \\sigma(x,y) =\\frac{1}{n-1} \\sum_{i=1}&#94;n \\left(x_i - \\mu_x \\right) \\left(y_i-\\mu_y\\right)  © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"module/statistics.html","title":"statistics – peano"},{"text":"assert Module Source File assert.f90 assert All Modules assert error geometry kdtree statistics Uses: error Development tool used to ensure that predicates are as expected before proceeding. If an assertion fails, the error message will be logged and the program terminated. © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"module/assert.html","title":"assert – peano"},{"text":"geometry Module Source File geometry.f90 geometry Functions norm distance All Modules assert error geometry kdtree statistics Provides routines for basic geometrical and trigonometric functions in 2D and 3D. Functions public pure function norm (p) Arguments Type Intent Optional Attributes Name real, intent(in), allocatable :: p (:) Return Value real Description Returns the L_2-norm of a vector \\mathbf{p},\n  \\lVert \\mathbf{p} \\rVert = \\sqrt{\\sum_{i=1}&#94;n p_i}  public pure function distance (p, q) Arguments Type Intent Optional Attributes Name real, intent(in), allocatable :: p (:) real, intent(in), allocatable :: q (:) Return Value real Description Returns the Euclidean distance between two points \\mathbf{x} and \\mathbf{y}, \n  \\lVert \\mathbf{q} - \\mathbf{p} \\rVert = \\sqrt{\\sum_{i=1}&#94;n \\left(q_i - p_i\\right)&#94;2}  © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"module/geometry.html","title":"geometry – peano"},{"text":"error Module Source File error.f90 error All Modules assert error geometry kdtree statistics Development tool used to add and print error messages, as well as handle logic flow in the event an error is thrown. © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"module/error.html","title":"error – peano"},{"text":"kdtree Module Source File kdtree.f90 kdtree All Modules assert error geometry kdtree statistics Provides routines for a k-dimensional tree, a space-partitioning data structure used for organizing points in k-dimensional space. Note that nearest neightbor searches within k-d trees are not suitable for high dimensional space and will likely perform no better than an exhaustive search. As a general rule of thumb, N \\gg 2&#94;k. © 2015 peano was written by Vincent San Miguel. Documentation generated by FORD .","tags":"","loc":"module/kdtree.html","title":"kdtree – peano"}]}