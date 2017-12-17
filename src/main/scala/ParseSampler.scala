package lin567_p2.parsers

import lin567_p2.Grammar._

class ParseSampler( pcfg:Map[NonTerminal,Map[RHS,Double]], randomSeed:Int = 15 ) {

  // For convenience, we want a reversal of the rules map; given an expansion, what are the possible
  // dominating non-terminals?
  var reversePCFG = Map[RHS,Map[NonTerminal,Double]]()

  pcfg.foreach{ case ( lhs, rhses ) =>
    rhses.foreach{ case ( rhs, prob ) =>
      if( reversePCFG.isDefinedAt( rhs ) ) {
        reversePCFG += rhs -> ( reversePCFG( rhs ) + ( lhs -> prob ) )
      } else {
        reversePCFG += rhs -> Map( lhs -> prob )
      }
    }
  }

  val maxLength = 20

  // Arrays are always mutable!
  var insideChart = Array.fill( maxLength, maxLength )( Map[NonTerminal,Double]().withDefaultValue(0D) )
  var condEntChart = Array.fill( maxLength, maxLength )( Map[NonTerminal,Double]().withDefaultValue(0D) )

  // Backtraces records the set of possible splits and left and right children for each non-terminal
  var parseBackTraces = Array.fill( maxLength, maxLength )(
    Map[NonTerminal,Set[Tuple3[Int,NonTerminal,NonTerminal]]]().withDefaultValue(Set())
  )

  def lexFill( word:String, i:Int ) {

    val rhs = RHS( Terminal( word ) )
    reversePCFG.getOrElse( rhs, Map() ).foreach{ case ( pos, prob ) =>
      insideChart( i )( i+1 ) += pos -> prob
      condEntChart( i )( i+1 ) += pos -> 0D
    }
  }

  def synFill( i:Int, j:Int ) {
    ( (i+1) to (j-1) ).foreach{ k =>
      val leftChildren = insideChart( i )( k )
      val rightChildren = insideChart( k )( j )

      leftChildren.foreach{ case ( lNode, lProb ) =>
        rightChildren.foreach{ case ( rNode, rProb ) =>
          val rhs = RHS( lNode, rNode )

          // implement me!

          reversePCFG.getOrElse( rhs, Map() ).foreach{ case ( pNode, ruleProb ) =>
            val parentIncrement = lProb * rProb * ruleProb
            insideChart( i )( j ) += pNode -> (
              insideChart( i )( j )( pNode ) + ( 
                parentIncrement
              )
            )


            parseBackTraces( i )( j ) += pNode -> (
              parseBackTraces( i )( j )( pNode ) + Tuple3( k, lNode, rNode )
            )
          }
        }
      }
    }
  }


  def initializeChart( length:Int ) {
    insideChart = Array.fill( maxLength, maxLength+1 )( Map[NonTerminal,Double]().withDefaultValue(0) )

    parseBackTraces = Array.fill( maxLength, maxLength )(
      Map[NonTerminal,Set[Tuple3[Int,NonTerminal,NonTerminal]]]().withDefaultValue(Set())
    )
  }

  def insidePass( s:Array[String] ) {
    // first clean the chart
    initializeChart( s.length )

    (1 to s.length).foreach{ j =>
      lexFill( s( j-1 ), j-1 )
    

      if( j > 1 ) {
        (0 to (j-2)).reverse.foreach{ i =>
          synFill( i, j )
        }
      }
    }

  }

  val rand = new util.Random( randomSeed )

  def sampleTree( s:Array[String], i:Int, j:Int, pNode:NonTerminal ):String = {

    if( j-i == 1 ) {
      s"($pNode ${s( i )})"
    } else {
      var randomSplit:Tuple3[Int,NonTerminal,NonTerminal] = null
      var random = rand.nextDouble

      var runningTotal = 0D

      parseBackTraces( i )( j )( pNode ).foreach{ split =>
        val ( k, lNode, rNode ) = split

        val splitScore =
          insideChart( i )( k )( lNode ) *
            insideChart( k )( j )( rNode ) *
              pcfg( pNode )( RHS( lNode, rNode ) ) /
                insideChart( i )( j )( pNode )
	 val condEntIncrement = splitScore*((scala.math.log(1/splitScore)/scala.math.log(2))+condEntChart( i )( k )( lNode )+condEntChart( k )( j )( rNode ))
	condEntChart( i )( j ) += pNode -> (
                condEntChart( i )( j )( pNode ) + ( 
                  condEntIncrement
                )
              )

        if( runningTotal < random && runningTotal + splitScore > random ) {
          randomSplit = split
        }
        runningTotal += splitScore
      }

      val ( sampledK, sampledLeft, sampledRight ) = randomSplit
	
      s"($pNode " + sampleTree( s, i, sampledK, sampledLeft ) + " "  +
        sampleTree( s, sampledK, j, sampledRight ) + " )"//+    condEntChart( 0 )( s.length )( NonTerminal( "S" ) ).toString


    }
	
  }

  // Takes as input a sequence of words and the number of parses to sample, returns a list of
  // sampled parses.
  def sampleParses( s:Array[String], numParses:Int ) = {
    insidePass( s )

    (0 until numParses).map{ _ =>
condEntChart = Array.fill( maxLength, maxLength )( Map[NonTerminal,Double]().withDefaultValue(0) )
      sampleTree( s, 0, s.length, NonTerminal( "S" ) )
    }
  }



}

