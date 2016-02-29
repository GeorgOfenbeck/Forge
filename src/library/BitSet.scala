///////////////////////////////////////////////////////////////////////////////
//  Author: Christopher R. Aberger (caberger@stanford.edu)
//
//  File: BitSet.scala
//
//  Description: A general BitSet trait for use.  Implemented using an array
//  of longs.  Bits are set by finding word indexes and and shifting proper
//  bit value in.  Uses parallel ops and is defined as a parallel class.
//  Can be distributed and has no set backs of Java BitSet class (although
//  it is based highly off that source code).  For right now mutability is 
//  only allowed if you allocate a bitset based of an integer size.  The
//  bitset does not grow or shrink if you try to set a bit outset of the 
//  initial range and will error out.  If you want this support add it in
//  yourself.  A basic starter class that can be improved.
//
//  64 bits per word.  6 address bits needed per word.  Based off of:
//  http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/6-b14/java/util/BitSet.java
///////////////////////////////////////////////////////////////////////////////


package ppl.dsl.forge
package library

import core.{ForgeApplication,ForgeApplicationRunner}
import templates.Utilities.nl

trait BitSetOps {
  this: ForgeApplication =>

  def importBitSetOps() {
    val BitSet = tpe("BitSet")

    // We just store an array of longs or an array of words
    // Size is defined here as the physical # of bits allocated. (physical size)
    // Length is defined as the known bit set to 1. (logical length)
    data(BitSet,("_words",MArray(MLong)),("_cardinality",MInt))

    // Allocate an bitset from an array of integers, setting each integers bit to 1 in a bitset
    static (BitSet) ("apply", Nil, MArray(MInt) :: BitSet) implements allocates(BitSet, {
  val arg1 = quotedArg(0)
  s"""bs_alloc_from_int_array($arg1)"""
},{
  val arg1 = quotedArg(0)
  s"""array_length($arg1)"""
})

    // Allocate a mutable bitset with a size large enough to store a range 0-N, where N is input arg.
    static (BitSet) ("apply", Nil, MInt :: BitSet, effect=mutable) implements allocates(BitSet, {
  val arg1 = quotedArg(0)
  s"""array_empty[Long](bs_get_alloc_length($arg1))"""
},{
  s"""numeric_zero[Int]"""
})

    // Allocate a bitset where your internal words are already ready.
    static (BitSet) ("apply", Nil, (MArray(MLong),MInt) :: BitSet) implements allocates(BitSet, {
  val arg1 = quotedArg(0)
  s"""$arg1"""
},{
  val arg1 = quotedArg(1)
  s"""$arg1"""
})

    val BitSetOps = withTpe(BitSet)
    BitSetOps{

      //////////////////////////////Basic Accessors////////////////////////////////////////

      // Length is defined here as the physical # of bits allocated. (physical size)
      infix("length")(Nil :: MInt) implements single {
        val self = quotedArg("self")
        s"""$self.numWords << 6"""
      } //multiply by 64
      infix("numWords")(Nil :: MInt) implements single {
        val self = quotedArg("self")
        s"""array_length(bs_get_words($self))"""
      }
      // Number of bits set to 1 in this bitset
      infix("cardinality")(Nil :: MInt) implements getter(0, "_cardinality")
      infix("apply")(MInt :: MBoolean) implements single {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""bs_get($self,$arg1)"""
      }
      infix("set")((MInt,MBoolean) :: MUnit, effect=write(0)) implements single {
          val arg1 = quotedArg(1)
          val arg2 = quotedArg(0)
          val arg3 = quotedArg(2)
          val self = quotedArg("self")
          s"""if ($arg1 < 0 || $arg1 > $arg2.length)  fatal("Cannot set bit set index: " + $arg1 + " BitSet physical range is 0-" + $arg2.length)
else {
  if($arg3) bs_set($self,$arg1)
  else bs_clear($self,$arg1)
}"""
        }

      //////////////////////////////Bit Set Operations////////////////////////////////////////

      infix("&")(BitSet :: BitSet) implements composite {
          val arg1 = quotedArg(0)
          val arg2 = quotedArg(1)
          s"""val smallLength = if ($arg1.numWords > $arg2.numWords) $arg2.numWords else $arg1.numWords
val a1 = bs_get_words($arg1)
val a2 = bs_get_words($arg2)

val mapper = array_fromfunction[Int](smallLength, e=>e)
val result = array_map[Int,Long](mapper, {e => a1(e) & a2(e)})
val cardinality = array_reduce[Int](array_map[Long,Int](result,{e => math_object_bitcount(e) }), {(a,b) => a+b}, numeric_zero[Int])
BitSet(result,cardinality)"""
        }

      infix("andCardinality")(BitSet :: MInt) implements composite {
          val arg1 = quotedArg(0)
          val arg2 = quotedArg(1)
          s"""val smallLength = if ($arg1.numWords > $arg2.numWords) $arg2.numWords else $arg1.numWords
val a1 = bs_get_words($arg1)
val a2 = bs_get_words($arg2)

val mapper = array_fromfunction[Int](smallLength, e=>e)
val cardinality = array_reduce[Int](array_map[Int,Int](mapper,{e => math_object_bitcount(a1(e) & a2(e)) }), {(a,b) => a+b},numeric_zero[Int])
cardinality"""
        }

      infix("|")(BitSet :: BitSet) implements composite {
          val arg1 = quotedArg(0)
          val arg2 = quotedArg(1)
          s"""val smallLength = if ($arg1.numWords > $arg2.numWords) $arg2.numWords else $arg1.numWords
val a1 = bs_get_words($arg1)
val a2 = bs_get_words($arg2)

val mapper = array_fromfunction[Int](smallLength, e=>e)
val result = array_map[Int,Long](mapper, {e => a1(e) | a2(e)})
val cardinality = array_reduce[Int](array_map[Long,Int](result,{e => math_object_bitcount(e) }), {(a,b) => a+b}, numeric_zero[Int])
BitSet(result,cardinality)"""
        }

      infix("xor")(BitSet :: BitSet) implements composite {
          val arg1 = quotedArg(0)
          val arg2 = quotedArg(1)
          s"""val smallLength = if ($arg1.numWords > $arg2.numWords) $arg2.numWords else $arg1.numWords
val a1 = bs_get_words($arg1)
val a2 = bs_get_words($arg2)

val mapper = array_fromfunction[Int](smallLength, e=>e)
val result = array_map[Int,Long](mapper, {e => a1(e) ^ a2(e)})
val cardinality = array_reduce[Int](array_map[Long,Int](result,{e => math_object_bitcount(e) }), {(a,b) => a+b}, numeric_zero[Int])
BitSet(result,cardinality)"""
        }

      //////////////////////////////Debug////////////////////////////////////////

      infix ("print") (Nil :: MUnit, effect = simple) implements single {
          val self = quotedArg("self")
          s"""var i = 0
println("cardinality: " + $self.cardinality)
println("length: " + $self.length)
while(i < $self.length){
  if ($self(i)) println("Set: " + i)
  i += 1
}"""
        }

      //////////////////////////////Internal Operations////////////////////////////////////////

      compiler("bs_set")(("bitIndex",MInt) :: MUnit, effect=write(0)) implements single {
          val self = quotedArg("self")
          s"""val wordIndex = bs_word_index(bitIndex)
val oldValue = bs_get_word($self,wordIndex)
val value = oldValue | (1L << bitIndex)
if (value != oldValue) bs_set_cardinality($self,$self.cardinality+1)
bs_set_word($self,wordIndex,value)"""
        }

      compiler("bs_clear")(("bitIndex",MInt) :: MUnit, effect=write(0)) implements single {
          val self = quotedArg("self")
          s"""val wordIndex = bs_word_index(bitIndex)
val oldValue = bs_get_word($self,wordIndex)
val value = bs_get_word($self,wordIndex) & ~(1L << bitIndex)
if (value != oldValue) bs_set_cardinality($self,$self.cardinality-1)
bs_set_word($self,wordIndex,value)"""
        }

      compiler("bs_set_cardinality")(MInt :: MUnit, effect = write(0)) implements setter(0, "_cardinality", quotedArg(1))
      compiler("bs_get")(("bitIndex",MInt) :: MBoolean) implements single {
        val self = quotedArg("self")
        s"""(bs_get_word($self,bs_word_index(bitIndex)) & (1L << bitIndex)) != 0"""
      }
      compiler("bs_get_word")(("wordIndex",MInt) :: MLong) implements single {
        val self = quotedArg("self")
        s"""array_apply(bs_get_words($self),wordIndex)"""
      }
      compiler("bs_get_words")(Nil :: MArray(MLong)) implements getter(0, "_words")
      compiler("bs_set_word")(( ("wordIndex",MInt),("value",MLong)) :: MUnit, effect=write(0)) implements single {
        val self = quotedArg("self")
        s"""array_update(bs_get_words($self),wordIndex,value)"""
      }
      
      compiler("bs_raw_alloc")(MInt :: BitSet) implements single {
        val arg1 = quotedArg(1)
        s"""BitSet($arg1)"""
      }
      compiler("bs_apply")(MInt :: MBoolean) implements single {
        val self = quotedArg("self")
        val arg1 = quotedArg(1)
        s"""$self($arg1)"""
      }
      parallelize as ParallelCollection(MBoolean, lookupOp("bs_raw_alloc"), lookupOp("length"), lookupOp("bs_apply"), lookupOp("set"))
    }

    //////////////////////////////Operations Needed for Alloc////////////////////////////////////////

    // Given an index, give me the word it lies in. 6 bits per word.
    compiler (BitSet) ("bs_word_index", Nil, ("bitIndex",MInt) :: MInt) implements single {
  s"""bitIndex >> 6"""
} //ADDRESS BITS PER WORD

    // Gives you how many words you need to store a bitset with given input maximum value
    compiler (BitSet) ("bs_get_alloc_length", Nil, MInt :: MInt) implements single {
  val arg1 = quotedArg(0)
  s"""bs_word_index($arg1)+1"""
}

    /* 
     * Allocates a bit set that sets the corresponding indexes in the array of incoming
     * integers.  There are some special optimizations here that make it quicker than
     * just looping around the integers and calling the set method.
     * This could happen in parallel.  Theoretically seemed more complex to code using parallel OPs though.
     */
    compiler (BitSet) ("bs_alloc_from_int_array", Nil, MArray(MInt) :: MArray(MLong)) implements single {
        val arg1 = quotedArg(0)
        s"""val sortedInput = bs_alloc_sort_array_in($arg1)

val words = array_empty[Long](bs_get_alloc_length(sortedInput(array_length(sortedInput)-1)))

var i = 0
while (i < array_length(sortedInput)){
  var cur = sortedInput(i)
  var wordIndex = bs_word_index(cur)
  var setValue = 1L << cur
  var sameWord = true
  
  i += 1
  while (i < array_length(sortedInput) && sameWord){
    
    if (bs_word_index(sortedInput(i))==wordIndex){
cur = sortedInput(i)
setValue = setValue | (1L << cur)
i += 1
    } else sameWord = false
  }
  
  array_update(words,wordIndex,setValue)
}
words"""
      }

    // I separate this out in hopes that the compiler is smart enough not to do this work twice on allocations.
    compiler (BitSet) ("bs_alloc_sort_array_in", Nil, MArray(MInt) :: MArray(MInt)) implements single {
    val arg1 = quotedArg(0)
    s"""array_sort($arg1)"""
  }
  }
}


