package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * An implementation of the CART decision tree learning algorithm
 * based on the sklearn implementation:
 *   https://github.com/scikit-learn/scikit-learn/blob/master/sklearn/tree/_tree.pyx
 *
 * Reference: ftp://ftp.boulder.ibm.com/software/analytics/spss/support/Stats/Docs/Statistics/Algorithms/14.0/TREE-CART.pdf
 */
trait TreeOps {
  this: OptiMLDSL =>

  def importTreeOps() {
    val DenseTrainingSet = lookupTpe("DenseTrainingSet")
    val DenseVector = lookupTpe("DenseVector")
    val IndexVector = lookupTpe("IndexVector")
    val Tup2 = lookupTpe("Tup2")
    val Tup3 = lookupTpe("Tup3")
    val Tup7 = lookupTpe("Tup7")

    /* Identifiers for tree building parameters */
    val TCriterion = tpe("TCriterion", stage = compile)
    identifier (TCriterion) ("MSE")   // for continuous-valued labels
    identifier (TCriterion) ("Gini")  // for discrete-valued labels

    val Tree = tpe("DecisionTree")

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Tree definition

    /*
     * Array-based binary decision tree. Each array is of length N, where N is the
     * number of nodes in the tree, and index i represents information about node i.
     */
    data(Tree,
        ("_numNodes", MInt),               // number of nodes in the tree
        ("_capacity", MInt),               // the size of the internal arrays, >= numNodes
        ("_isLeaf", MArray(MBoolean)),     // isLeaf(i) is true if node i is a leaf, false otherwise
        ("_leftChildren", MArray(MInt)),   // leftChildren(i) is the node id of the left child of i
        ("_rightChildren", MArray(MInt)),  // rightChildren(i) is the node id of the right child of i
        ("_feature", MArray(MInt)),        // feature(i) is the index j of the feature to split on for node i
        ("_threshold", MArray(MDouble)),   // threshold(i) is the splitting threshold for node i
        ("_value", MArray(MDouble)),       // value(i) is the constant prediction value for node i
        ("_impurity", MArray(MDouble)),    // impurity(i) is the value of the splitting criterion for node i
        ("_numNodeSamples", MArray(MInt))) // numNodeSamples(i) is the number of training samples reaching node i

    compiler (Tree) ("alloc_tree", Nil, ("initCapacity", MInt) :: Tree, effect = mutable) implements allocates(Tree,
      {
        s"""unit(0)"""
      },
      {
        s"""initCapacity"""
      },
      {
        s"""array_empty[Boolean](initCapacity)"""
      },
      {
        s"""array_empty[Int](initCapacity)"""
      },
      {
        s"""array_empty[Int](initCapacity)"""
      },
      {
        s"""array_empty[Int](initCapacity)"""
      },
      {
        s"""array_empty[Double](initCapacity)"""
      },
      {
        s"""array_empty[Double](initCapacity)"""
      },
      {
        s"""array_empty[Double](initCapacity)"""
      },
      {
      s"""array_empty[Int](initCapacity)"""
    }
    )

    val TreeOps = withTpe(Tree)
    TreeOps {
      // getters and setters
      infix ("numNodes") (Nil :: MInt) implements getter(0, "_numNodes")
      compiler ("infix_set_num_nodes") (MInt :: MUnit, effect = write(0)) implements setter(0, "_numNodes", {
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

      infix ("capacity") (Nil :: MInt) implements getter(0, "_capacity")
      compiler ("infix_set_capacity") (MInt :: MUnit, effect = write(0)) implements setter(0, "_capacity", {
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

      infix("isLeaf") (Nil :: MArray(MBoolean)) implements getter(0, "_isLeaf")
      compiler ("infix_set_is_leaf") (MArray(MBoolean) :: MUnit, effect = write(0)) implements setter(0, "_isLeaf", {
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

      infix ("leftChildren") (Nil :: MArray(MInt)) implements getter(0, "_leftChildren")
      compiler ("infix_set_left_children") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_leftChildren", {
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

      infix ("rightChildren") (Nil :: MArray(MInt)) implements getter(0, "_rightChildren")
      compiler ("infix_set_right_children") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_rightChildren", {
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

      infix ("feature") (Nil :: MArray(MInt)) implements getter(0, "_feature")
      compiler ("infix_set_feature") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_feature", {
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

      infix ("threshold") (Nil :: MArray(MDouble)) implements getter(0, "_threshold")
      compiler ("infix_set_threshold") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_threshold", {
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

      infix ("value") (Nil :: MArray(MDouble)) implements getter(0, "_value")
      compiler ("infix_set_value") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_value", {
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

      infix ("impurity") (Nil :: MArray(MDouble)) implements getter(0, "_impurity")
      compiler ("infix_set_impurity") (MArray(MDouble) :: MUnit, effect = write(0)) implements setter(0, "_impurity", {
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

      infix ("numNodeSamples") (Nil :: MArray(MInt)) implements getter(0, "_numNodeSamples")
      compiler ("infix_set_num_node_samples") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_numNodeSamples", {
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

      // Adds a node to the tree. The new node registers itself as the child of its parent.
      infix ("addNode") (MethodSignature(List(
                          ("parent", MInt), ("isLeft", MBoolean), ("isLeaf", MBoolean),
                          ("feature", MInt), ("threshold", MDouble), ("impurity", MDouble),
                          ("numNodeSamples", MInt)), MInt), effect = write(0)) implements composite {
  val self = quotedArg("self")
  s"""val nodeId = $self.numNodes
        if (nodeId >= $self.capacity) {
          tree_realloc($self, $self.capacity+1)
        }

        if (parent != -1) { 
          if (isLeft)
            $self.leftChildren(parent) = nodeId
          else
            $self.rightChildren(parent) = nodeId
        }

        if (!isLeaf) {
          $self.feature(nodeId) = feature
          $self.threshold(nodeId) = threshold
        }

        $self.isLeaf(nodeId) = isLeaf
        $self.impurity(nodeId) = impurity
        $self.numNodeSamples(nodeId) = numNodeSamples

        $self.set_num_nodes($self.numNodes + 1)

        nodeId"""
}
    }

    compiler (Tree) ("tree_realloc", Nil, (("tree", Tree), ("minCapacity", MInt)) :: MUnit, effect = write(0)) implements composite {
        val tree = quotedArg("tree")
        s"""var n = max(4, tree.capacity * 2)
while (n < minCapacity) n = n*2

val newIsLeaf = array_empty[Boolean](n)
array_copy(tree.isLeaf, 0, newIsLeaf, 0, $tree.numNodes)
tree.set_is_leaf(newIsLeaf)

val newLeftChildren = array_empty[Int](n)
array_copy(tree.leftChildren, 0, newLeftChildren, 0, $tree.numNodes)
tree.set_left_children(newLeftChildren)

val newRightChildren = array_empty[Int](n)
array_copy(tree.rightChildren, 0, newRightChildren, 0, $tree.numNodes)
tree.set_right_children(newRightChildren)

val newFeature = array_empty[Int](n)
array_copy(tree.feature, 0, newFeature, 0, $tree.numNodes)
tree.set_feature(newFeature)

val newThreshold = array_empty[Double](n)
array_copy(tree.threshold, 0, newThreshold, 0, $tree.numNodes)
tree.set_threshold(newThreshold)

val newValue = array_empty[Double](n)
array_copy(tree.value, 0, newValue, 0, $tree.numNodes)
tree.set_value(newValue)

val newImpurity = array_empty[Double](n)
array_copy(tree.impurity, 0, newImpurity, 0, $tree.numNodes)
tree.set_impurity(newImpurity)

val newNumNodeSamples = array_empty[Int](n)
array_copy(tree.numNodeSamples, 0, newNumNodeSamples, 0, $tree.numNodes)
tree.set_num_node_samples(newNumNodeSamples)

tree.set_capacity(n)"""
      }

    compiler (Tree) ("init_tree", Nil, ("maxDepth", MInt) :: Tree) implements composite {
        s"""val initCapacity =
  if (maxDepth <= 10) (pow(2.0, maxDepth+1.0) - 1).toInt
  else 2047

alloc_tree(initCapacity)"""
      }


    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Tree construction

    /* Construct the decision tree in a depth-first fashion */
    direct (Tree) ("dtree", Nil, MethodSignature(List(
                                   ("trainingSet",DenseTrainingSet(MDouble,MDouble)),
                                   ("maxDepth", MInt, "unit(-1)"),
                                   ("maxNumFeatures", MInt, "unit(-1)"),
                                   ("minSamplesSplit", MInt, "unit(2)"),
                                   ("minSamplesLeaf", MInt, "unit(1)"),
                                   ("useSamples", IndexVector, "unit(null.asInstanceOf[IndexVector])"),
                                   ("criterion", TCriterion, "MSE")
                                 ), Tree)) implements composite {
  s"""val _maxDepth = if (maxDepth < 0) INF.toInt else maxDepth
      val _maxNumFeatures = if (maxNumFeatures < 0) sqrt(trainingSet.numFeatures).toInt else maxNumFeatures
      val _useSamples = if (useSamples == null) trainingSet.data.rowIndices else useSamples

      
      val MIN_IMPURITY_SPLIT = 1e-7

      val tree = init_tree(_maxDepth)

      
      val process = doLambda({(t: Rep[Tup7[IndexVector,Int,Int,Boolean,Double,DenseVector[Boolean],Any]]) =>
        
        val samples = t._1

        
        val depth = t._2

        
        val parent = t._3

        
        val isLeft = t._4

        
        val impurity = t._5

        
        val constantFeatures = t._6

        
        val continuation = t._7
        val next = continuation.AsInstanceOf[Tup7[IndexVector,Int,Int,Boolean,Double,DenseVector[Boolean],Any] => Unit]

        val numNodeSamples = samples.length

        
        val isLeaf =
          (depth >= _maxDepth) ||
          (constantFeatures.forall(f => f == true)) ||
          (numNodeSamples < minSamplesSplit) ||
          (numNodeSamples < 2*minSamplesLeaf) ||
          (impurity <= MIN_IMPURITY_SPLIT)

        if (isLeaf) {
          val nodeId = tree.addNode(parent, isLeft, isLeaf, 0, 0.0, impurity, numNodeSamples)
          tree.value(nodeId) = tree_score(trainingSet, samples, criterion)
          ()
        }
        else {
          
          val (sortedSamples, pos, threshold, feature, impurityLeft, impurityRight, nextConstantFeatures) = unpack(tree_split(tree, trainingSet, samples, _maxNumFeatures, impurity, constantFeatures, criterion))

          if (pos == -1) { 
            val nodeId = tree.addNode(parent, isLeft, true, 0, 0.0, impurity, numNodeSamples)
            tree.value(nodeId) = tree_score(trainingSet, samples, criterion)
            ()
          }
          else {
            
            val nodeId = tree.addNode(parent, isLeft, isLeaf, feature, threshold, impurity, numNodeSamples)

            
            doApply(next, pack((sortedSamples(0::pos), depth+1, nodeId, unit(true), impurityLeft, nextConstantFeatures, continuation)))

            
            doApply(next, pack((sortedSamples(pos::sortedSamples.length), depth+1, nodeId, unit(false), impurityRight, nextConstantFeatures, continuation)))
          }
        }

        ()
      })

      
      
      doApply(process, pack((_useSamples, unit(0), unit(-1), unit(false), compute_impurity(trainingSet, _useSamples, criterion), (0::trainingSet.numFeatures) { i => false }, process.AsInstanceOf[Any])))

      tree.unsafeImmutable"""
}

    compiler (Tree) ("tree_split", Nil, MethodSignature(List(("tree", Tree), ("trainingSet", DenseTrainingSet(MDouble,MDouble)), ("samples", IndexVector), ("maxNumFeatures", MInt), ("impurity", MDouble), ("constantFeatures", DenseVector(MBoolean)), ("criterion", TCriterion)), Tup7(IndexVector,MInt,MDouble,MInt,MDouble,MDouble,DenseVector(MBoolean)))) implements composite {
        s"""fassert(samples.length > 1, "samples to split must be at least 2")


val FEATURE_THRESHOLD = 1e-7


val candidateFeatures = trainingSet.data.colIndices filter { i => !constantFeatures(i) }
fassert(candidateFeatures.length > 0, "a non-constant feature is required to split")
val pct = maxNumFeatures.toDouble / candidateFeatures.length.toDouble
val testFeatures = if (pct < 1.0) sample(candidateFeatures, pct) else candidateFeatures


val improvements = testFeatures { j =>
  
  val featureValues = trainingSet.data.apply(samples).getCol(j)
  val (sortedValues, sortedIndices) = featureValues.sortWithIndex
  val sortedSamples = samples(sortedIndices)

  
  if ((sortedValues(sortedValues.length-1) - sortedValues(0)) < FEATURE_THRESHOLD) {
    pack((-INF, sortedSamples, unit(0), unit(0.0), unit(0.0), unit(0.0)))
  }
  else {
    
    var p = 1
    var bestPos = 1
    var bestScore = -INF
    var bestImpurityLeft = -INF
    var bestImpurityRight = -INF

    while (p < sortedValues.length) {

while ((p < sortedValues.length - 1) && ((sortedValues(p) - sortedValues(p-1)) < FEATURE_THRESHOLD)) {
  p += 1
}

val (improvement, impurityLeft, impurityRight) = unpack(compute_impurity_improvement(trainingSet, impurity, sortedSamples, p, criterion))

if (improvement > bestScore) {
  bestScore = improvement
  bestPos = p
  bestImpurityLeft = impurityLeft
  bestImpurityRight = impurityRight
}

p += 1
    }

    val threshold = (sortedValues(bestPos-1) + sortedValues(bestPos)) / 2.0

    pack((readVar(bestScore), sortedSamples, readVar(bestPos), threshold, readVar(bestImpurityLeft), readVar(bestImpurityRight)))
  }
}

val bestFeatureIndex = improvements.map(_._1).maxIndex
val bestFeature = testFeatures(bestFeatureIndex)
val bestSort = improvements(bestFeatureIndex)._2
val bestPos = improvements(bestFeatureIndex)._3
val threshold = improvements(bestFeatureIndex)._4
val impurityLeft = improvements(bestFeatureIndex)._5
val impurityRight = improvements(bestFeatureIndex)._6

val newConstantFeatures = testFeatures(improvements find { t => t._1 == -INF })
val nextConstantFeatures = constantFeatures.mutable
for (j <- newConstantFeatures) {
  nextConstantFeatures(j) = true
}

if (improvements(bestFeatureIndex)._1 == -INF) {
  
  pack((bestSort, unit(-1), unit(0.0), unit(0), unit(0.0), unit(0.0), nextConstantFeatures))
}
else {
  pack((bestSort, bestPos, threshold, bestFeature, impurityLeft, impurityRight, nextConstantFeatures))
}"""
      }


    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Impurity Criterion

    compiler (Tree) ("compute_impurity_improvement", Nil, (("trainingSet", DenseTrainingSet(MDouble,MDouble)), ("impurity", MDouble), ("samples", IndexVector), ("splitPos", MInt), ("criterion", TCriterion)) :: Tup3(MDouble,MDouble,MDouble)) implements composite {
        s"""val leftSamples = samples(0::splitPos)
val rightSamples = samples(splitPos::samples.length)

val impurityLeft = compute_impurity(trainingSet, leftSamples, criterion)
val impurityRight = compute_impurity(trainingSet, rightSamples, criterion)

val newImpurity = (leftSamples.length.toDouble / samples.length * impurityLeft) + (rightSamples.length.toDouble / samples.length * impurityRight)
val improvement = (samples.length.toDouble / trainingSet.numSamples) * (impurity - newImpurity)

pack((improvement, impurityLeft, impurityRight))"""
      }

    compiler (Tree) ("compute_impurity", Nil, (("trainingSet", DenseTrainingSet(MDouble,MDouble)), ("samples", IndexVector), ("criterion", TCriterion)) :: MDouble) implements composite {
        s"""criterion match {
  case MSE => impurity_mse(trainingSet, samples)
  case Gini => impurity_gini(trainingSet, samples)
}"""
      }

    /*
     * Mean Squared Error (MSE) regression criterion
     */
    compiler (Tree) ("impurity_mse", Nil, (("trainingSet", DenseTrainingSet(MDouble,MDouble)), ("samples", IndexVector)) :: MDouble) implements composite {
        s"""variance(trainingSet.labels.apply(samples))"""
      }

    /**
     * Gini index classification criterion
     */
    compiler (Tree) ("impurity_gini", Nil, (("trainingSet", DenseTrainingSet(MDouble,MDouble)), ("samples", IndexVector)) :: MDouble) implements composite {
        s"""val labels = trainingSet.labels.apply(samples)
val numSamplesByLabel = labels.groupByReduce(l => l, l => 1, (a: Rep[Int],b: Rep[Int]) => a+b)

1.0 - sum(0, numSamplesByLabel.keys.length) { ki =>
  val k = numSamplesByLabel.keys.apply(ki)
  
  val freq = numSamplesByLabel(k).toDouble / samples.length
  square(freq)
}"""
      }

    /**
     * The value to store as the prediction for this leaf.
     */
    compiler (Tree) ("tree_score", Nil, (("trainingSet", DenseTrainingSet(MDouble,MDouble)), ("samples", IndexVector), ("criterion", TCriterion)) :: MDouble) implements composite {
        s"""criterion match {
  case MSE => tree_score_mse(trainingSet, samples)
  case Gini => tree_score_gini(trainingSet, samples)
}"""
      }

    compiler (Tree) ("tree_score_mse", Nil, (("trainingSet", DenseTrainingSet(MDouble,MDouble)), ("samples", IndexVector)) :: MDouble) implements composite {
        s"""mean(trainingSet.labels.apply(samples))"""
      }

    compiler (Tree) ("tree_score_gini", Nil, (("trainingSet", DenseTrainingSet(MDouble,MDouble)), ("samples", IndexVector)) :: MDouble) implements composite {
        s"""val allLabels = trainingSet.labels.apply(samples)
val samplesByLabel = allLabels.groupByReduce(l => l, l => 1, (a: Rep[Int],b: Rep[Int]) => a+b)
val mostFrequent = densevector_fromarray(fhashmap_values(samplesByLabel), true).maxIndex
samplesByLabel.keys.apply(mostFrequent)"""
      }


    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Prediction

    infix (Tree) ("predict", Nil, (("tree", Tree), ("testPt", DenseVector(MDouble))) :: MDouble) implements composite {
        s"""fassert(tree.numNodes > 1, "decision tree is empty")

var node = 0
while (!tree.isLeaf.apply(node)) {
  val feature = tree.feature.apply(node)
  val threshold = tree.threshold.apply(node)
  if (testPt(feature) <= threshold) {
    node = tree.leftChildren.apply(node)
  }
  else {
    node = tree.rightChildren.apply(node)
  }
}


tree.value.apply(node)"""
      }


    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Visualization

    infix (Tree) ("pprint", Nil, (("tree", Tree)) :: MUnit, effect = simple) implements composite {
        s"""val process = doLambda({(t: Rep[Tup2[Int,Any]]) =>
  val node = t._1
  val continuation = t._2
  val next = continuation.AsInstanceOf[Tup2[Int,Any] => Unit]

  val feature = tree.feature.apply(node)
  val threshold = tree.threshold.apply(node)
  val leftChild = tree.leftChildren.apply(node)
  val rightChild = tree.rightChildren.apply(node)
  val isLeaf = tree.isLeaf.apply(node)

  println("Node: " + node)
  if (!isLeaf) {
    println("If feature " + feature + " <= " + threshold + " then node " + leftChild + " else node " + rightChild)
    println()
    doApply(next, pack((leftChild, continuation)))
    doApply(next, pack((rightChild, continuation)))
  }
  else {
    println("Leaf covering " + tree.numNodeSamples.apply(node) + " samples. Prediction value is: " + tree.value.apply(node))
    println()
  }
})

doApply(process, pack((unit(0), process.AsInstanceOf[Any])))"""
      }
  }
}
