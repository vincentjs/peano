module kd_tree
  !! Provides routines for a k-dimensional tree, a space-partitioning data structure used for organizing points in k-dimensional space. Note that nearest neightbor searches within k-d trees are not suitable for high dimensional space and will likely perform no better than an exhaustive search. As a general rule of thumb, \(N \gg 2^k\). For an introduction to k-d trees, see "An introductory tutorial on kd-trees" (Andrew Moore, 1991).

  type kdbox
     !! Represents a hyperrectangle / box / n-orthotope)
     real :: min, max
  end type kdbox

  type kdnode
     !! Represents a node in the kdtree
     private
     integer :: split
     !! The split dimension of this node
     type(kdnode), pointer :: left, right
     !! The left and right child nodes
     type(kdbox), pointer :: orthotope => null()
     !! The bounding hyperrectangle of this node
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
  
contains

  function build_Kd_Tree(S) result(kdt)
    !! UX to build a balanced k-d tree from a set of sorted data points, /(S/).
    !! @warning Note that /(S/) must be input as a *row-major* array, /(\texttt{S(1:dim, 1:num)}/),
    !! NOT in Fortran's usual column-major notation. 
    
    use statistics, only: median
    use assert, only: assert_x_is_ge_y

    real, target :: S(:,:)
    type(kdtree), pointer :: kdt

    ! Store data set and properties in kdtree structure
    allocate(kdt)
    kdt%set => S
    kdt%dim = size(S,1)
    kdt%num = size(S,2)

    ! The number of data points must be greater than the number of its dimensions.
    call assert_x_is_ge_y(kdt%num, kdt%dim)

    ! The data set is bounded by a _dim_-orthotope. Find these bounds.
    
    ! Recursively build the kd tree
    kdt%root => build(kdt)
    
    ! The root of the kd tree is determined by the initial split dimension, which we set
    ! as the median of the (sorted) data set.
    
    ! Recursively build the tree
        
  end function build_Kd_Tree

  recursive function build(tree) result (node)

    type(kdtree), pointer :: tree
    type(kdnode), pointer :: node
    
  end function build
  
end module kd_tree
