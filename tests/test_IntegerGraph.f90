program test_IntegerGraph
    use IntegerGraph_
    use TestUtils_
    use Math_
    use String_
    use IOStream_
    use RealVector_
    use IntegerVector_
    use IntegerHyperVector_
    use Node_
    use Edge_
    use NodeVector_
    use EdgeVector_
    use Matrix_
    use StringIntegerMap_
    use StringIntegerPair_
    implicit none
		type(IntegerGraph) :: mygraph, mysubgraph
		type(IntegerVector) :: path
		type(Matrix) :: AMatrix, dMatrix, LMatrix, OmegaMatrix
		
		call mygraph%init( directed=.false. )
		call mygraph%newNode()
		call mygraph%newNode()
		call mygraph%newNode()
		call mygraph%newNode()
		call mygraph%newNode()
		
		call mygraph%newEdges( 1, [2] )
		call mygraph%newEdges( 2, [1,3,4] )
		call mygraph%newEdges( 3, [2,5] )
		call mygraph%newEdges( 4, [2,5] )
		call mygraph%newEdges( 5, [3,4] )
		
		call mygraph%deleteEdge( 6 )
		call assert_equal( mygraph%nNodes(), 5, "IntegerGraph_test: example 0 nNodes" )
		call assert_equal( mygraph%nEdges(), 9, "IntegerGraph_test: example 0 nEdges" )
		
		call mygraph%computeDijkstraPaths( 1 )
		call assert_equal_real( mygraph%distance(3), 2.0_8, 1e-10_8, "IntegerGraph_test: distance" )
		path = mygraph%shortestPath(3)
		call assert_equal( path%size(), 3, "IntegerGraph_test: shortest path size" )
		
		AMatrix = mygraph%adjacencyMatrix()
		dMatrix = mygraph%distanceMatrix()
		LMatrix = mygraph%laplacianMatrix()
		OmegaMatrix = mygraph%resistanceDistanceMatrix( laplacianMatrix=LMatrix )
		
		call assert_equal_real( mygraph%randicIndex(), 2.18972270488542_8, 1e-6_8, "IntegerGraph_test: randic" )
		call assert_equal_real( mygraph%wienerIndex( distanceMatrix=dMatrix ), 0.0_8, 1e-10_8, "IntegerGraph_test: wiener" )
		call assert_equal_real( mygraph%kirchhoffIndex( resistanceDistanceMatrix=OmegaMatrix ), 0.0_8, 1e-6_8, "IntegerGraph_test: kirchhoff" )
		
		! Apollonian Network 2
		call mygraph%init()
		call mygraph%newNodes( 7 )
		call mygraph%newEdges( 1, [2,5,4,6,3] )
		call mygraph%newEdges( 2, [1,5,4,7,3] )
		call mygraph%newEdges( 3, [1,6,4,7,2] )
		call mygraph%newEdges( 4, [1,5,2,7,3,6] )
		call mygraph%newEdges( 5, [1,2,4] )
		call mygraph%newEdges( 6, [1,4,3] )
		call mygraph%newEdges( 7, [3,4,2] )
		
		call assert_equal_real( mygraph%wienerIndex(), 0.0_8, 1e-10_8, "IntegerGraph_test: apollonian wiener" )
		call assert_equal_real( mygraph%molecularTopologicalIndex(), 360.0_8, 1e-10_8, "IntegerGraph_test: apollonian molecularTopological" )
		call assert_equal_real( mygraph%kirchhoffIndex(), 0.0_8, 1e-10_8, "IntegerGraph_test: apollonian kirchhoff" )
		call assert_equal_real( mygraph%kirchhoffSumIndex(), 0.0_8, 1e-10_8, "IntegerGraph_test: apollonian kirchhoffSum" )
		
		! inducedSubgraph
		call mygraph%init()
		call mygraph%newNodes( 5, labels=["1.a","2.a","3.a","4.a","5.a"], weights=[1.0_8,2.0_8,3.0_8,4.0_8,5.0_8] )
		call mygraph%newEdges( 1, [2,3], weights=[1.2_8,1.3_8] )
		call mygraph%newEdges( 2, [1,3], weights=[2.1_8,2.3_8] )
		call mygraph%newEdges( 3, [1,2,4,5], weights=[3.1_8,3.2_8,3.4_8,3.5_8] )
		call mygraph%newEdges( 4, [3], weights=[4.3_8] )
		call mygraph%newEdges( 5, [3], weights=[5.3_8] )
		
		mysubgraph = mygraph%inducedSubgraph( [1,2,3] )
		call assert_equal( mysubgraph%nNodes(), 3, "IntegerGraph_test: inducedSubgraph nNodes" )
		
		call mysubgraph%initFromDATLine( "1[label=1.a,weight=1.00000];2[label=3.a,weight=3.00000];3[label=4.a,weight=4.00000];1--2[label=3.a--1.a,weight=1.30000];2--3[label=4.a--3.a,weight=3.40000];" )
		call assert_equal( mysubgraph%nNodes(), 3, "IntegerGraph_test: initFromDATLine nNodes" )
		call assert_equal( mysubgraph%nEdges(), 4, "IntegerGraph_test: initFromDATLine nEdges" )
end program test_IntegerGraph
