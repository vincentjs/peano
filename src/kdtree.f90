module kd_tree
  !! Provides routines for a k-dimensional tree, a space-partitioning data structure used for organizing points in k-dimensional space. Note that nearest neightbor searches within k-d trees are not suitable for high dimensional space and will likely perform no better than an exhaustive search. As a general rule of thumb, \(N \gg 2^k\). Input data set need not be sorted. For an introduction to k-d trees, see "An introductory tutorial on kd-trees" (Andrew Moore, 1991).

  implicit none
  
  type kdbox
     !! Represents a hypercube
     real :: min, max
  end type kdbox

  type kdnode
     !! Represents a node in the kdtree
     private
     integer :: splitDim
     !! The split dimension for this node
     real :: splitVal
     !! The value of the split dimension for this node
     integer :: l, u
     !! The lower and upper indices of the array slice of the original data set used for this node
     type(kdnode), pointer :: left, right
     !! The left and right child nodes
     type(kdbox), pointer :: orthotope => null()
     !! The bounding hypercube of this node
  end type kdnode

  type kdtree
     !! Represents the kdtree itself
     real, pointer :: set(:,:) => null()
     !! The input (sorted, row-major) data set associated with this kd tree
     type(kdnode), pointer :: root => null()
     !! The root node of the kd tree
     integer :: dim = 0
     !! The rank of the data set's domain
     integer :: num = 0
     !! The rank of the data set's range
  end type kdtree

  public :: build_kd_tree, destroy_kd_tree
  
contains

  function build_kd_tree(S) result(kdt)
    !! UX to build a balanced k-d tree from a set of sorted data points, /(S/).
    !! @warning Note that /(S/) must be input as a *row-major* array, /(\texttt{S(1:dim, 1:num)}/),
    !! in conjunction with Fortran's usual notation. 
    
    use assert, only: assert_x_is_ge_y

    real, target :: S(:,:)
    type(kdtree), pointer :: kdt
    integer :: rootDepth, l, u

    if (size(S,2) .gt. 0) then
       ! Store data set and properties in kdtree structure
       allocate(kdt)
       kdt%set => S
       kdt%dim = size(S,1)
       kdt%num = size(S,2)

       ! The number of data points must be greater than the number of its dimensions.
       call assert_x_is_ge_y(kdt%num, kdt%dim)

       ! Recursively build the kd tree
       l = lbound(S,2)
       u = ubound(S,2)
       rootDepth = 1
       kdt%root => build(kdt, l, u, rootDepth)
    end if
    
  end function build_kd_tree

  recursive function build(tree, l, u, depth) result (node)

    use statistics, only: median
    
    integer, intent(in) :: depth, l, u
    type(kdtree), pointer :: tree
    type(kdnode), pointer :: node

    integer :: k, axis, m

    if (u >= l) then
       ! Allocate the node resulting from this build
       allocate(node)

       ! Create a balanced kdtree
       ! Select axis based on modular depth, allowing axis to cycle through all valid values
       k = size(tree%set,2)
       axis = mod(depth, k)

       ! Set the split dimension as the median
       m = median(tree%set(axis,l:u))
       node%splitDim = m
       node%splitVal = tree%set(axis, m)

       ! Recursively build tree
       node%left => build(tree, l, m-1, depth + 1)
       node%right => build(tree, m, u, depth + 1)
    end if
    
  end function build

  subroutine destroy_kd_tree(kdt)

    type(kdtree), pointer :: kdt

    call destroy_node(kdt%root)

    deallocate(kdt)

  contains

    recursive subroutine destroy_node(node)
      !! Recursively destroy each node in the kdtree
      
      type(kdnode), pointer :: node

      if (associated(node%left)) then
         call destroy_node(node%left)
         nullify(node%left)
      end if

      if (associated(node%right)) then
         call destroy_node(node%right)
         nullify(node%right)
      end if

      deallocate(node)
    end subroutine destroy_node
    
  end subroutine destroy_kd_tree
  
end module kd_tree
