package ppl.dsl.forge
package templates
package shallow

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.tools.nsc.io._
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._
import core._
import Utilities._
import shared.BaseGenDataStructures

trait ShallowGenOps extends ForgeCodeGenBase with BaseGenDataStructures {
  val IR: ForgeApplicationRunner with ForgeExp with ForgeOpsExp
  import IR._

  /**
   * Quoting for formatted code-gen
   */

  def inline(o: Rep[DSLOp], str: Exp[String], quoter: Exp[Any] => String = quote) = {
    var b = quoter(str)
    for (i <- 0 until o.args.length) {
      val name = o.args.apply(i).name
      b = b.replaceAllLiterally(quoter(quotedArg(name)), name)
      // allow named args to be referred to by position as well
      b = b.replaceAllLiterally(quoter(quotedArg(i)), name)
    }
    for (i <- 0 until o.implicitArgs.length) {
      val name = o.implicitArgs.apply(i).name
      // implicit args can only be quoted by name so there is no ambiguity
      b = b.replaceAllLiterally(quoter(quotedArg(name)), name)
    }
    for (i <- 0 until o.tpePars.length) {
      b = b.replaceAllLiterally(quoter(quotedTpe(i,o)), o.tpePars.apply(i).name)
    }
    b
  }

  def replaceWildcards(s: String) = {
    var o = s
    o = s.replaceAll(qu, "\"")

    // splice in the quoted symbol. we use a wildcard instead of an expression tree here
    // because string interpolation does not have a corresponding IR node.
    while (o.contains(symMarker)) {
      val st = o.indexOf(symMarker)
      val end = o.indexOf(symMarker, st+1)
      val symNum: Int = o.slice(st+symMarker.length, end).toInt
      val sym = globalDefs.find(_.lhs.apply(0).id == symNum).get.lhs.apply(0)
      o = o.slice(0,st) + quoteLiteral(sym) + o.slice(end+symMarker.length,o.length)
    }
    o
  }

  override def quote(x: Exp[Any]) : String = x match {
    case Def(QuoteBlockResult(func,args,ret,captured)) if (isThunk(func.tpe)) => func.name
    case Def(QuoteBlockResult(func,args,ret,captured)) => func.name + "(" + replaceWildcards(captured.mkString(",")) + ")"
    case Def(QuoteSeq(argName)) => argName
    case Const(s: String) => replaceWildcards(s) // don't add quotes TODO rework
    case _ => super.quote(x)
  }

  /**
   * For dc_alloc. By convention, dc_alloc's return tpe par must be its last tpe par, if it has one.
   */
  def instAllocReturnTpe(o: Rep[DSLOp], colTpePar: Rep[DSLType], elemTpePar: Rep[DSLType]): List[Rep[DSLType]] = {
    // dc_alloc is context sensitive: if the first argument is a tpe parameter, we assume a type signature of [R,CR]. otherwise, we assume a signature of [_,R]
    // note that dc_alloc always takes exactly 2 arguments (a collection and a size). this is still a bit hokey.
    if (o.tpePars.length > 0) {
      val colTpe = o.args.apply(0).tpe
      if (isTpePar(colTpe)) List(elemTpePar,colTpePar) else o.tpePars.dropRight(1).map(p => if (p == colTpe.tpePars.apply(0)) colTpePar.tpePars.apply(0) else p) :+ elemTpePar
    }
    else Nil
  }


  /**
   * Overloading resolution, using implicit hack.
   * We need overloads for both front-end signatures (e.g., "+", universal) as well as abstract methods (e.g. vector_plus, grp).
   */

  // if after converting Ts and Vars to Reps there are duplicates, remove them.
  def unique(ops: List[Rep[DSLOp]]) = uniqueMap(ops)._1

  def uniqueMap(ops: List[Rep[DSLOp]]) = {
    // we maintain a separate ArrayBuffer along with the map to retain order
    val filtered = scala.collection.mutable.ArrayBuffer[Rep[DSLOp]]()
    val canonicalMap = scala.collection.mutable.HashMap[String,Rep[DSLOp]]()
    // add to filtered only if canonical version doesn't already exist
    for (o <- ops) {
      val t = canonicalize(o)
      if (!canonicalMap.contains(t)) {
        filtered += o
        canonicalMap(t) = o
      }
    }
    (filtered.toList,canonicalMap)
  }

  def canonicalize(o: Rep[DSLOp]) = o.grp.name + o.name + makeOpArgsWithType(o) + makeOpImplicitArgs(o)

  // precomputed for performance
  lazy val allOps = OpsGrp.values.toList.flatMap(g => g.ops)
  lazy val allOpsCanonicalMap = uniqueMap(allOps)._2
  def canonical(o: Rep[DSLOp]): Rep[DSLOp] = allOpsCanonicalMap.getOrElse(canonicalize(o), err("no canonical version of " + o.name + " found"))

  // The actual op comparison is a performance hotspot; checks should be decomposed to allow early exits.

  // Here we try to remove unnecessary overloaded implicits; unfortunately, it doesn't appear to have much of an impact (on compile times).
  // def nameClash(o1: Rep[DSLOp], o2: Rep[DSLOp]) = {
  //   val simpleChecks = o1.style == o2.style && o1.name == o2.name && o1.args.length == o2.args.length
  //   simpleChecks && {
  //     val o1infix = !noInfix(o1)
  //     val o2infix = !noInfix(o2)
  //     (o1infix == o2infix) && {
  //       // with implicit classes we don't need to consider the first argument for method overloading resolution,
  //       // unless it is the same type (after promotion) in both ops
  //       val usingImplicits = o1.style == infixMethod && !o1infix && !o2infix
  //       val (co1, co2) = (canonical(o1), canonical(o2)) // required to have a consistent view
  //       co1 == co2 || {
  //         val (o1args, o2args) = if (usingImplicits) (co1.args.drop(1), co2.args.drop(1)) else (co1.args, co2.args)
  //         val lhsMatch = {
  //           !usingImplicits || o1.args.length == 0 || {
  //             val t1 = o1.args.apply(0).tpe
  //             val t2 = o2.args.apply(0).tpe
  //             getHkTpe(t1).name == getHkTpe(t2).name || repify(t1) == repify(t2)
  //           }
  //         }
  //         lhsMatch && o1args.zip(o2args).forall { a =>
  //           getHkTpe(a._1.tpe).name == getHkTpe(a._2.tpe).name ||
  //           isTpePar(a._1.tpe) || isTpePar(a._2.tpe) ||
  //           (isFuncArg(a._1) && isFuncArg(a._2)) ||
  //           (repify(a._1).startsWith("Rep") && repify(a._2).startsWith("Rep")) // check for promotions
  //         }
  //       }
  //     }
  //   }
  // }

  def nameClash(o1: Rep[DSLOp], o2: Rep[DSLOp]) = simpleNameClash(o1,o2)

  def simpleNameClash(o1: Rep[DSLOp], o2: Rep[DSLOp]) = o1.style == o2.style && o1.name == o2.name // forces a global numbering

  def nameClashesGrp(o: Rep[DSLOp]) = opsGrpOf(o).map(_.ops.filter(o2 => o.grp.name == o2.grp.name && nameClash(o,o2))).getOrElse(Nil)

  def nameClashesUniversal(o: Rep[DSLOp]) = allOps.filter(o2 => nameClash(o,o2))

  def nameClashesSimple(o: Rep[DSLOp]) = allOps.filter(o2 => simpleNameClash(o,o2))

  def nameClashId(o: Rep[DSLOp], clasher: Rep[DSLOp] => List[Rep[DSLOp]] = nameClashesUniversal) = {
    val clashes = clasher(o)
    if (clashes.length > 1) (clashes.indexOf(o)+1).toString else ""
  }

  /**
   * Op argument formatting
   */

  def simpleArgName(t: Rep[DSLArg]): String = t.name

  def makeArgs(args: List[Rep[DSLArg]], makeArgName: (Rep[DSLArg] => String) = simpleArgName, addParen: Boolean = true) = {
    if (args.length == 0 && !addParen) {
      ""
    }
    else {
      "(" + args.map(makeArgName).mkString(",") + ")"
    }
  }

  def makeArgsWithType(args: List[Rep[DSLArg]], typify: Rep[DSLType] => String = typify, addParen: Boolean = true) = makeArgs(args, t => argify(t, typify), addParen)

  def makeArgsWithNowType(args: List[Rep[DSLArg]], addParen: Boolean = true) = makeArgsWithType(args, typifySome, addParen)

  def makeOpArgs(o: Rep[DSLOp], addParen: Boolean = true) = makeArgs(o.args, addParen = addParen)

  def makeOpFutureArgs(o: Rep[DSLOp], makeArgName: (Rep[DSLArg] => String)) = makeArgs(o.args, t => { val arg = makeArgName(t); if (t.tpe.stage == now && !isTpeInst(t.tpe)) "unit("+arg+")" else arg })

  def makeOpArgsWithType(o: Rep[DSLOp], typify: Rep[DSLType] => String = typify, addParen: Boolean = true) = makeArgsWithType(o.args, typify, addParen)

  def makeOpArgsWithNowType(o: Rep[DSLOp], addParen: Boolean = true) = makeArgsWithNowType(o.args, addParen)

  def makeFullArgs(o: Rep[DSLOp], makeArgs: Rep[DSLOp] => String) = {
    // we always pass implicit arguments explicitly (in practice, less issues arise this way)
    val implicitArgs = makeOpImplicitArgs(o)
    makeTpeParsAsArgs(o.tpePars, o.tpePars) + makeArgs(o) + implicitArgs
  }


  /**
   * Higher-kinded type parameter manipulation
   */

  def withoutHkTpePars(tpePars: List[Rep[TypePar]]): List[Rep[TypePar]] = {
    tpePars.filter {
      case Def(HkTpePar(n,args,c,s)) => false
      case _ => true
    }
  }

  def getHkTpeParInstantiations(tpePars: List[Rep[TypePar]], args: List[Rep[DSLArg]], implicitArgs: List[Rep[DSLArg]]): List[Rep[DSLArg]] = {
    // hkTpePars context bounds must be included, since we cannot use context bounds with them
    // We have to be careful -- we only want to include manifests for any concrete instantiations of the hkTpePar in the op.
    val hkTpePars = (tpePars collect { case d@Def(HkTpePar(n,t,sigs,s)) => (n,sigs) }).toMap
    val hkTpeParInsts =
      ((args ++ implicitArgs).map(_.tpe) flatMap {
        case d@Def(TpeInst(hkTpe, hkArgs)) if hkTpePars.contains(hkTpe.name) =>
          val sigs = hkTpePars(hkTpe.name)
          sigs.map(b => ephemeralTpe(b.name+"["+quote(d)+"]", stage = now))
        case _ => Nil
      }).distinct

    hkTpeParInsts.zipWithIndex.map(t => arg(implicitCtxBoundArgPrefix+"_hk_"+t._2, t._1))
  }


  /**
   * Op implicit argument formatting
   */

  // untyped implicit args
  def makeImplicitCtxBoundsArgs(tpePars: List[Rep[TypePar]]): List[Rep[DSLArg]]  = {
    tpePars.flatMap(
      tp => tp.ctxBounds.map(
        cb => arg(
          "implicitly["+cb.name+"["+quote(tp)+"]]", 
          tpe(cb.name, List(tpePar(quote(tp), List(), now)), now),
          None
        )
      )
    )
  }

  def makeImplicitArgs(tpePars: List[Rep[TypePar]], args: List[Rep[DSLArg]], implicitArgs: List[Rep[DSLArg]]) = {
    val hkInstantiations = getHkTpeParInstantiations(tpePars, args, implicitArgs)

    // passing order is: regular ctxBounds, then regular implicits, and finally hkInstantiations context bounds
    val allImplicitArgs = makeImplicitCtxBoundsArgs(tpePars) ++ (implicitArgs filter (_.tpe != MSourceContext)) ++ hkInstantiations
    if (allImplicitArgs.length > 0) "(" + allImplicitArgs.map(quote).mkString(",") + ")"
    else ""
  }

  def makeOpImplicitArgs(o: Rep[DSLOp]) = {
    makeImplicitArgs(o.tpePars, o.args, o.implicitArgs)
  }

  // typed implicit args with context bounds (only needed for instance methods)
  // 'without' is used to subtract bounds that are already in scope
  def implicitCtxBoundsWithType(tpePars: List[Rep[TypePar]], without: List[Rep[TypePar]] = Nil) = {
    val withoutBounds = without.flatMap(a => a.ctxBounds)
    withoutHkTpePars(tpePars).flatMap(a => a.ctxBounds.diff(withoutBounds).map(b => ephemeralTpe(b.name+"["+quote(a)+"]", stage = now))).distinct
  }

  def makeImplicitArgsWithCtxBoundsWithType(tpePars: List[Rep[TypePar]], args: List[Rep[DSLArg]], implicitArgs: List[Rep[DSLArg]], without: List[Rep[TypePar]] = Nil, asVals: Boolean = false) = {
    val addArgs = implicitCtxBoundsWithType(tpePars, without).zipWithIndex.map(t => arg(implicitCtxBoundArgPrefix+t._2, t._1))
    makeImplicitArgsWithType(tpePars, args, addArgs ++ implicitArgs, asVals)
  }

  // typed implicit args without context bounds (but hkTpePar context bounds are passed explicitly)
  def makeImplicitArgsWithType(tpePars: List[Rep[TypePar]], args: List[Rep[DSLArg]], implicitArgs: List[Rep[DSLArg]], asVals: Boolean = false) = {
    val prefix = if (asVals == true) "val " else ""
    val hkInstantiations = getHkTpeParInstantiations(tpePars, args, implicitArgs)
    val allImplicitArgs = (implicitArgs filter (_.tpe != MSourceContext)) ++ hkInstantiations
    if (allImplicitArgs.length > 0) "(implicit " + allImplicitArgs.map(t => prefix + argify(t,typifySome)).mkString(",") + ")"
    else ""
  }

  def makeOpImplicitArgsWithType(o: Rep[DSLOp], asVals: Boolean = false) = {
    makeImplicitArgsWithType(o.tpePars, o.args, o.implicitArgs, asVals)
  }


  /**
   * Op method names
   */

  val specialCharacters = scala.collection.immutable.Map("+" -> "pl", "-" -> "sub", "/" -> "div", "*" -> "mul", "=" -> "eq", "<" -> "lt", ">" -> "gt", "&" -> "and", "|" -> "or", "!" -> "bang", ":" -> "cln")
  def sanitize(x: String) = {
    var out = x
    specialCharacters.keys.foreach { k => if (x.contains(k)) out = out.replace(k, specialCharacters(k)) }
    out
  }

  def makeDefWithOverride(o: Rep[DSLOp]) = {
    if (overrideList.contains(o.name)) "override def"
    else "def"
  }

  def makeOpMethodName(o: Rep[DSLOp]) = {
    Labels.getOrElse(o, {
      // adding the nameClashId is another way to avoid chaining the Overload implicit, but the weird method names that result are confusing
      val i = /*if (Config.fastCompile) nameClashId(canonical(o), nameClashesGrp) else*/ ""
      o.style match {
        case `staticMethod` => o.grp.name.toLowerCase + "_object_" + sanitize(o.name).toLowerCase + i
        case `compilerMethod` =>
          if (o.name != sanitize(o.name)) err("compiler op name has special characters that require reformatting: " + o.name)
          o.name // should be callable directly from impl code
        case _ => o.grp.name.toLowerCase + "_" + sanitize(o.name).toLowerCase + i
      }
    })
  }

  def makeOpMethodNameWithArgs(o: Rep[DSLOp]) = makeOpMethodName(o) + makeFullArgs(o, o => makeOpArgs(o))

  def makeOpMethodSignature(o: Rep[DSLOp], withReturnTpe: Option[Boolean] = None) = {
    val addRet = withReturnTpe.getOrElse(Config.fastCompile)
    val ret = if (addRet || isRedirect(o)) ": " + typifySome(o.retTpe) else ""
    val implicitArgs = makeOpImplicitArgsWithType(o)
    // if (Config.fastCompile) {
    //   "def " + makeOpMethodName(o) + makeTpeParsWithBounds(o.tpePars) + makeOpArgsWithType(o) + makeOpImplicitArgsWithType(o) + ret
    // }
    // else {
      "def " + makeOpMethodName(o) + makeTpeParsWithBounds(o.tpePars) + makeOpArgsWithType(o) + implicitArgs + ret
    // }
  }

  def makeSyntaxMethod(o: Rep[DSLOp], prefix: String = "def ", withReturnTpe: Option[Boolean] = None) = {
    // adding the return type increases verbosity in the generated code, so we omit it by default
    val addRet = withReturnTpe.getOrElse(Config.fastCompile)
    val ret = if (addRet || isRedirect(o)) ": " + typifySome(o.retTpe) else ""
    val curriedArgs = o.curriedArgs.map(a => makeArgsWithNowType(a)).mkString("")
    prefix + o.name + makeTpeParsWithBounds(o.tpePars) + makeArgsWithNowType(o.firstArgs, o.effect != pure || o.name == "apply") + curriedArgs + makeOpImplicitArgsWithType(o) + ret + " = " + makeOpMethodNameWithArgs(o)
  }

  def makeOpImplMethodName(o: Rep[DSLOp]) = makeOpMethodName(o) + "_impl" + nameClashId(o)

  def makeOpImplMethodNameWithArgs(o: Rep[DSLOp], postfix: String = "") = makeOpImplMethodName(o) + postfix + makeTpeParsAsArgs(o.tpePars, o.tpePars) + makeOpArgs(o) + makeOpImplicitArgs(o)

  def makeOpImplMethodSignature(o: Rep[DSLOp], postfix: String = "", returnTpe: Option[String] = None) = {
    "def " + makeOpImplMethodName(o) + postfix + makeTpeParsWithBounds(o.tpePars) + makeOpArgsWithType(o) + makeOpImplicitArgsWithType(o) + ": " + (returnTpe getOrElse typifySome(o.retTpe))
  }


  /**
   * Op sanity checking
   *
   * These are mostly repetitive right now, but we could specialize the error checking more (or generalize it to be more concise).
   */

  def validTpePar(o: Rep[DSLOp], tpePar: Rep[DSLType]) = tpePar match {
    case Def(TpePar(name,_,_)) => o.tpePars.exists(_.name == name)
    case _ => true
  }

  def check(o: Rep[DSLOp]) {
    if (!Impls.contains(o)) err("op " + o.name + " has no impl")

    // arguments to DeliteOpType ops should always be Rep, List, or Seq (or the transformer in mirror will have a hard time)
    def validOpArgs(a: Rep[DSLArg]) = {
      a.tpe.stage == future || isFuncArg(a) || isThunk(a.tpe) || a.tpe.name.startsWith("List") || a.tpe.name.startsWith("Seq")
    }

    def isParallelCollection(t: Rep[DSLType]) = ForgeCollections.contains(t)
    def isParallelCollectionBuffer(t: Rep[DSLType]) = ForgeCollections.get(t).exists(_.isInstanceOf[ParallelCollectionBuffer])

    Impls(o) match {
      case _:DeliteOpType =>
        if (o.args.exists(a => !validOpArgs(a))) err("op " + o.name + " is a Delite op, but has non-Rep arguments")
      case _ =>
    }

    // op-specific checks
    Impls(o) match {
      case Allocates(tpe,init) =>
        if (!DataStructs.contains(tpe)) err("op " + o.name + " allocates tpe " + tpe.name + " with no corresponding data definition")
        val data = DataStructs(tpe)
        if (init.length != data.fields.length)
          err("allocator " + o.name + " has a different number of fields than the data definition for " + tpe.name)
      case Getter(structArgIndex,field) =>
        if (structArgIndex > o.args.length) err("arg index " + structArgIndex + " does not exist for op " + o.name)
        val struct = getHkTpe(o.args.apply(structArgIndex).tpe)
        val data = DataStructs.get(struct)
        if (data.isEmpty) err("no struct definitions found for arg index " + structArgIndex + " in op " + o.name)
        if (!data.get.fields.map(_.name).contains(field)) err("struct arg " + structArgIndex + " does not contain field " + field + " in op " + o.name)
      case Setter(structArgIndex,field,value) =>
        if (structArgIndex > o.args.length) err("arg index " + structArgIndex + " does not exist for op " + o.name)
        val struct = getHkTpe(o.args.apply(structArgIndex).tpe)
        val data = DataStructs.get(struct)
        if (data.isEmpty) err("no struct definitions found for arg index " + structArgIndex + " in op " + o.name)
        if (!data.get.fields.map(_.name).contains(field)) err("struct arg " + structArgIndex + " does not contain field " + field + " in op " + o.name)
      case map:Map =>
        val col = getHkTpe(o.args.apply(map.argIndex).tpe)
        if (!isParallelCollection(col)) err("map argument " + col.name + " is not a ParallelCollection")
        if (map.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("map op with undefined type par: " + o.name)
        if (map.argIndex < 0 || map.argIndex > o.args.length) err("map op with illegal arg parameter: " + o.name)
      case zip:Zip =>
        val colA = getHkTpe(o.args.apply(zip.argIndices._1).tpe)
        val colB = getHkTpe(o.args.apply(zip.argIndices._2).tpe)
        if (!isParallelCollection(colA)) err("zip argument " + colA.name + " is not a ParallelCollection")
        if (!isParallelCollection(colB)) err("zip argument " + colB.name + " is not a ParallelCollection")
        if (zip.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("zipWith op with undefined type parg: " + o.name)
        if (zip.argIndices.productIterator.asInstanceOf[Iterator[Int]].exists(a => a < 0 || a > o.args.length)) err("zipWith op with illegal arg parameter: " + o.name)
      case reduce:Reduce =>
        val col = getHkTpe(o.args.apply(reduce.argIndex).tpe)
        if (!isParallelCollection(col)) err("reduce argument " + col.name + " is not a ParallelCollection")
        if (!validTpePar(o,reduce.tpePar)) err("reduce op with undefined type par: " + o.name)
        if (reduce.argIndex < 0 || reduce.argIndex > o.args.length) err("reduce op with illegal arg parameter: " + o.name)
        // if (reduce.zero.retTpe != reduce.tpePar) err("reduce op with illegal zero parameter: " + o.name)
      case mapreduce:MapReduce =>
        val col = getHkTpe(o.args.apply(mapreduce.argIndex).tpe)
        if (!isParallelCollection(col)) err("mapReduce argument " + col.name + " is not a ParallelCollection")
        if (mapreduce.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("mapreduce op with undefined type par: " + o.name)
        if (mapreduce.argIndex < 0 || mapreduce.argIndex > o.args.length) err("mapreduce op with illegal arg parameter: " + o.name)
      case filter:Filter =>
        val col = getHkTpe(o.args.apply(filter.argIndex).tpe)
        if (!isParallelCollection(col)) err("filter argument " + col.name + " is not a ParallelCollection")
        if (!isParallelCollectionBuffer(getHkTpe(o.retTpe))) err("filter return type " + col.name + " is not a ParallelCollectionBuffer")
        if (filter.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("filter op with undefined type par: " + o.name)
        if (filter.argIndex < 0 || filter.argIndex > o.args.length) err("filter op with illegal arg parameter: " + o.name)
      case flatmap:FlatMap =>
        val col = getHkTpe(o.args.apply(flatmap.argIndex).tpe)
        if (!isParallelCollection(col)) err("flatmap argument " + col.name + " is not a ParallelCollection")
        if (!isParallelCollectionBuffer(getHkTpe(o.retTpe))) err("flatmap return type " + col.name + " is not a ParallelCollectionBuffer")
        if (flatmap.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("flatmap op with undefined type par: " + o.name)
        if (flatmap.argIndex < 0 || flatmap.argIndex > o.args.length) err("flatmap op with illegal arg parameter: " + o.name)
      case gb:GroupBy =>
        val col = getHkTpe(o.args.apply(gb.argIndex).tpe)
        if (!isParallelCollection(col)) err("groupBy argument " + col.name + " is not a ParallelCollection")
        val innerCol = getHkTpe(gb.tpePars._4)
        if (!isParallelCollectionBuffer(innerCol)) err("groupBy inner collection type " + innerCol.name + " is not a ParallelCollectionBuffer")
        if (gb.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("groupBy op with undefined type par: " + o.name)
        if (gb.argIndex < 0 || gb.argIndex > o.args.length) err("groupBy op with illegal arg parameter: " + o.name)
      case gbr:GroupByReduce =>
        val col = getHkTpe(o.args.apply(gbr.argIndex).tpe)
        if (!isParallelCollection(col)) err("groupByReduce argument " + col.name + " is not a ParallelCollection")
        if (gbr.tpePars.productIterator.exists(a => !validTpePar(o,a.asInstanceOf[Rep[DSLType]]))) err("groupByReduce op with undefined type par: " + o.name)
        if (gbr.argIndex < 0 || gbr.argIndex > o.args.length) err("groupByReduce op with illegal arg parameter: " + o.name)
      case foreach:Foreach =>
        val col = getHkTpe(o.args.apply(foreach.argIndex).tpe)
        if (!isParallelCollection(col)) err("foreach argument " + col.name + " is not a ParallelCollection")
        if (!validTpePar(o,foreach.tpePar)) err("foreach op with undefined type par: " + o.name)
        if (foreach.argIndex < 0 || foreach.argIndex > o.args.length) err("foreach op with illegal arg parameter: " + o.name)
      case _ => // nothing to check
    }
  }

  def emitImpl(o: Rep[DSLOp], stream: PrintWriter, indent: Int) {
    // val indent = 4
    // val ret = typifySome(o.retTpe)
    // emitWithIndent("{", stream, indent)
    def tpeParser(prod: Product): String = 
      makeTpePars(prod.productIterator.toList.asInstanceOf[List[Rep[DSLType]]]/*.filter(isTpePar)*/.:+(o.retTpe).asInstanceOf[List[Rep[TypePar]]])
    def emitFunc(func: Rep[String], indent: Int = 0) {
      inline(o, func, quoteLiteral).split(nl).toList match {
        case List(line) => stream.print(line)
        case lines => {
          stream.println("{")
          lines.foreach { line => emitWithIndent(line, stream, indent+2 )}
          // emitWithIndent("}", stream, indent+2)
          stream.print((" " * indent) + "}" )
        }
      }
    }

    emitWithIndent(" {", stream)
    // stream.print(" "*indent)
    Impls(o) match {
      case single:SingleTask => {
        // stream.print("Delite.single(")
        // inline(o, single.func, quoteLiteral).split(nl).foreach { line => emitWithIndent(line, stream, indent+4 )}
        // emitFunc(single.func)
          inline(o, single.func, quoteLiteral).split(nl).foreach { line => emitWithIndent(line, stream, indent+2 )}
        // emitWithIndent(")", stream, indent+2)
        // stream.println(")")
      }
      case composite:Composite => {
        // stream.print("Delite.composite(")
        // inline(o, composite.func, quoteLiteral).split(nl).foreach { line => emitWithIndent(line, stream, indent+4 )}
        // emitFunc(composite.func)
        inline(o, composite.func, quoteLiteral).split(nl).foreach { line => emitWithIndent(line, stream, indent+2 )}
        // emitWithIndent(")", stream, indent+2)
        // stream.println(")")
      }
      case map:Map => {
        val outDc = ForgeCollections(getHkTpe(o.retTpe))
        val in = o.args.apply(map.argIndex)
        val inDc = ForgeCollections(getHkTpe(in.tpe))
        emitWithIndent("def func: " + repify(map.tpePars._1) + " => " + repify(map.tpePars._2) + " = " + inline(o, map.func), stream, indent+2)
        emitWithIndent("val in = " + in.name, stream, indent+2)
        // TODO: why not just makeTpePars(o.retTpe.tpePars)? and sanity check with map.tpePars._2:
        //  need to call the dcAlloc function, which may have different tpe pars than the output..
        emitWithIndent("val out = " + makeOpMethodName(outDc.alloc) + makeTpePars(instAllocReturnTpe(outDc.alloc,in.tpe,map.tpePars._2)) + "(in, " + makeOpMethodName(inDc.size) + "(in)" + ")", stream, indent+2) // TODO - makeArg
        emitWithIndent("var i = 0", stream, indent+2)
        emitWithIndent("while (i < " + makeOpMethodName(inDc.size) + "(in)" + ") {", stream, indent+2)
        emitWithIndent(makeOpMethodName(outDc.update) + "(out, i, func(" + makeOpMethodName(inDc.apply) + "(in, i)))", stream, indent+4)
        emitWithIndent("i += 1", stream, indent+4)
        emitWithIndent("}", stream, indent+2)
        emitWithIndent("out", stream, indent+2)
      }
      case zip:Zip => {
        val outDc = ForgeCollections(getHkTpe(o.retTpe))
        val inA = o.args.apply(zip.argIndices._1)
        val inB = o.args.apply(zip.argIndices._2)
        val inADc = ForgeCollections(getHkTpe(inA.tpe))
        val inBDc = ForgeCollections(getHkTpe(inB.tpe))
        emitWithIndent("def func: (" + repify(zip.tpePars._1) + "," + repify(zip.tpePars._2) + ") => " + repify(zip.tpePars._3) + " = " + inline(o, zip.func), stream, indent+2)
        emitWithIndent("val inA = " + inA.name, stream, indent+2)
        emitWithIndent("val inB = " + inB.name, stream, indent+2)
        emitWithIndent("val out = " + makeOpMethodName(outDc.alloc) + makeTpePars(instAllocReturnTpe(outDc.alloc,inA.tpe,zip.tpePars._3)) + "(inA, " + makeOpMethodName(inADc.size) + "(inA)" + ")", stream, indent+2)
        emitWithIndent("var i = 0", stream, indent+2)
        emitWithIndent("while (i < " + makeOpMethodName(inADc.size) + "(inA)" + ") {", stream, indent+2)
        emitWithIndent(makeOpMethodName(outDc.update) + "(out, i, func(" + makeOpMethodName(inADc.apply) + "(inA, i)," + makeOpMethodName(inBDc.apply) + "(inB, i)))", stream, indent+4)
        emitWithIndent("i += 1", stream, indent+4)
        emitWithIndent("}", stream, indent+2)
        emitWithIndent("out", stream, indent+2)
      }
      case reduce:Reduce => 
        val c = o.args.apply(reduce.argIndex)
        val dc = ForgeCollections(getHkTpe(c.tpe))
        emitWithIndent("def func: (" + repify(reduce.tpePar) + "," + repify(reduce.tpePar) + ") => " + repify(reduce.tpePar) + " = " + inline(o, reduce.func), stream, indent+2)
        emitWithIndent("def zero: " + repify(reduce.tpePar) + " = " + inline(o, reduce.zero), stream, indent+2)
        emitWithIndent("val in = " + c.name, stream, indent+2)
        emitWithIndent("var acc = if (" + makeOpMethodName(dc.size) + "(in) == 0) zero else " + makeOpMethodName(dc.apply) + "(in, 0)", stream, indent+2)
        emitWithIndent("var i = 1", stream, indent+2)
        emitWithIndent("while (i < " + makeOpMethodName(dc.size) + "(in)" + ") {", stream, indent+2)
        emitWithIndent("acc = " + " func(acc, " + makeOpMethodName(dc.apply) + "(in, i))", stream, indent+4)
        emitWithIndent("i += 1", stream, indent+4)
        emitWithIndent("}", stream, indent+2)
        emitWithIndent("acc", stream, indent+2)
      case mapreduce:MapReduce => 
        val c = o.args.apply(mapreduce.argIndex)
        val dc = ForgeCollections(getHkTpe(c.tpe))
        emitWithIndent("def map: " + repify(mapreduce.tpePars._1) + " => " + repify(mapreduce.tpePars._2) + " = " + inline(o, mapreduce.map), stream, indent+2)
        emitWithIndent("def reduce: (" + repify(mapreduce.tpePars._2) + "," + repify(mapreduce.tpePars._2) + ") => " + repify(mapreduce.tpePars._2) + " = " + inline(o, mapreduce.reduce), stream, indent+2)
        emitWithIndent("def zero: " + repify(mapreduce.tpePars._2) + " = " + inline(o, mapreduce.zero), stream, indent+2)
        emitWithIndent("val in = " + c.name, stream, indent+2)
        if (mapreduce.cond.isDefined) {
          emitWithIndent("def cond: " + repify(mapreduce.tpePars._1) + " => " + repify(MBoolean) + " = " + inline(o, mapreduce.cond.get), stream, indent+2)
          emitWithIndent("var acc = null.asInstanceOf["+quote(mapreduce.tpePars._2)+"]", stream, indent+2)
          emitWithIndent("var i = 0", stream, indent+2)
        }
        else {
          emitWithIndent("var acc = if (" + makeOpMethodName(dc.size) + "(in) == 0) zero else map(" + makeOpMethodName(dc.apply) + "(in, 0))", stream, indent+2)
          emitWithIndent("var i = 1", stream, indent+2)
        }
        emitWithIndent("while (i < " + makeOpMethodName(dc.size) + "(in)" + ") {", stream, indent+2)
        emitWithIndent("val e = " + makeOpMethodName(dc.apply) + "(in, i)", stream, indent+4)
        if (mapreduce.cond.isDefined) {
          emitWithIndent("if (cond(e)) {", stream, indent+4)
          emitWithIndent("if (acc == null) acc = map(e)", stream, indent+6)
          emitWithIndent("else acc = reduce(acc, map(e))", stream, indent+6)
          emitWithIndent("}", stream, indent+4)
        }
        else {
          emitWithIndent("acc = " + " reduce(acc, map(e))", stream, indent+4)
        }
        emitWithIndent("i += 1", stream, indent+4)
        emitWithIndent("}", stream, indent+2)
        emitWithIndent("acc", stream, indent+2)
      case filter:Filter => 
        val outDc = ForgeCollections(getHkTpe(o.retTpe)).asInstanceOf[ParallelCollectionBuffer]
        val in = o.args.apply(filter.argIndex)
        val inDc = ForgeCollections(getHkTpe(in.tpe))
        emitWithIndent("def func: " + repify(filter.tpePars._1) + " => " + repify(filter.tpePars._2) + " = " + inline(o, filter.func), stream, indent+2)
        emitWithIndent("def cond: " + repify(filter.tpePars._1) + " => " + repify(MBoolean) + " = " + inline(o, filter.cond), stream, indent+2)
        emitWithIndent("val in = " + in.name, stream, indent+2)
        emitWithIndent("val out = " + makeOpMethodName(outDc.alloc) + makeTpePars(instAllocReturnTpe(outDc.alloc,in.tpe,filter.tpePars._2)) + "(in,0)", stream, indent+2)
        emitWithIndent("var i = 0", stream, indent+2)
        emitWithIndent("while (i < " + makeOpMethodName(inDc.size) + "(in)"  + ") {", stream, indent+2)
        emitWithIndent("val e = " + makeOpMethodName(inDc.apply) + "(in, i)", stream, indent+4)
        emitWithIndent("if (cond(e)) {", stream, indent+4)
        emitWithIndent(makeOpMethodName(outDc.append) + "(out, i, func(e))", stream, indent+6)
        emitWithIndent("}", stream, indent+4)
        emitWithIndent("i += 1", stream, indent+4)
        emitWithIndent("}", stream, indent+2)
        emitWithIndent("out", stream, indent+2)
      case flatmap:FlatMap =>
        val outCol = getHkTpe(o.retTpe)
        val outDc = ForgeCollections(outCol).asInstanceOf[ParallelCollectionBuffer]
        val in = o.args.apply(flatmap.argIndex)
        val inDc = ForgeCollections(getHkTpe(in.tpe))
        emitWithIndent("def func: " + repify(flatmap.tpePars._1) + " => " + repify(tpeInst(outCol, flatmap.tpePars._2)) + " = " + inline(o, flatmap.func), stream, indent+2)
        emitWithIndent("val in = " + in.name, stream, indent+2)
        emitWithIndent("val out = " + makeOpMethodName(outDc.alloc) + makeTpePars(instAllocReturnTpe(outDc.alloc,in.tpe,flatmap.tpePars._2)) + "(in,0)", stream, indent+2)
        emitWithIndent("var sz = 0", stream, indent+2)
        emitWithIndent("var i = 0", stream, indent+2)
        emitWithIndent("while (i < " + makeOpMethodName(inDc.size) + "(in)) {", stream, indent+2)
        emitWithIndent("val e = " + makeOpMethodName(inDc.apply) + "(in, i)", stream, indent+4)
        emitWithIndent("val buf = func(e)", stream, indent+4)
        emitWithIndent("var j = 0", stream, indent+4)
        emitWithIndent("while (j < " + makeOpMethodName(outDc.size) + "(buf)) {", stream, indent+4)
        emitWithIndent(makeOpMethodName(outDc.append) + "(out, sz, buf(j))", stream, indent+6)
        emitWithIndent("sz += 1", stream, indent+6)
        emitWithIndent("j += 1", stream, indent+6)
        emitWithIndent("}", stream, indent+4)
        emitWithIndent("i += 1", stream, indent+4)
        emitWithIndent("}", stream, indent+2)
        emitWithIndent("out", stream, indent+2)
      case gb:GroupBy =>
        // val outerColTpe = getHkTpe(o.retTpe)
        val outerColTpe = MArrayBuffer
        val outDc = ForgeCollections(outerColTpe).asInstanceOf[ParallelCollectionBuffer]
        val innerColTpe = getHkTpe(gb.tpePars._4)
        val innerDc = ForgeCollections(innerColTpe).asInstanceOf[ParallelCollectionBuffer]
        val in = o.args.apply(gb.argIndex)
        val inDc = ForgeCollections(getHkTpe(in.tpe))
        if (gb.cond.isDefined) {
          emitWithIndent("def cond: " + repify(gb.tpePars._1) + " => " + repify(MBoolean) + " = " + inline(o, gb.cond.get), stream, indent+2)
        }
        emitWithIndent("def key: " + repify(gb.tpePars._1) + " => " + repify(gb.tpePars._2) + " = " + inline(o, gb.key), stream, indent+2)
        emitWithIndent("def map: " + repify(gb.tpePars._1) + " => " + repify(gb.tpePars._3) + " = " + inline(o, gb.map), stream, indent+2)
        emitWithIndent("val in = " + in.name, stream, indent+2)
        emitWithIndent("val out = new SHashMap["+quote(gb.tpePars._2)+","+quote(tpeInst(innerColTpe, gb.tpePars._3))+"]()", stream, indent+2)
        emitWithIndent("var i = 0", stream, indent+2)
        emitWithIndent("while (i < " + makeOpMethodName(inDc.size) + "(in)"  + ") {", stream, indent+2)
        emitWithIndent("val e = " + makeOpMethodName(inDc.apply) + "(in, i)", stream, indent+4)
        if (gb.cond.isDefined) {
          emitWithIndent("if (cond(e)) {", stream, indent+4)
        }
        emitWithIndent("val k = key(e)", stream, indent+6)
        emitWithIndent("if (!out.contains(k)) {", stream, indent+6)

        // FIXME: passing 'in' to dc_alloc is a bit of a hack - what do we do when 'in' is not the same type as 'out'? currently (and only for groupby) we pass null.
        // this is only safe when the output collection type is known to not use the input argument, such as the current case, ArrayBuffer.
        // to really fix this, either the dc_alloc design needs to be revisited, or we need to use a different allocation mechanism than dc_alloc.
        val innerDcArg = if (quote(in.tpe) == quote(innerColTpe)) "in" else "null.asInstanceOf["+repify(tpeInst(innerColTpe, gb.tpePars._3))+"]"

        emitWithIndent("val bucket = " + makeOpMethodName(innerDc.alloc) + makeTpePars(instAllocReturnTpe(innerDc.alloc, in.tpe, gb.tpePars._3)) + "("+innerDcArg+",0)", stream, indent+8)
        emitWithIndent(makeOpMethodName(innerDc.append) + "(bucket, 0, map(e))", stream, indent+8)
        emitWithIndent("out(k) = bucket", stream, indent+8)
        emitWithIndent("}", stream, indent+6)
        emitWithIndent("else {", stream, indent+6)
        emitWithIndent("val bucket = out(k)", stream, indent+8)
        emitWithIndent(makeOpMethodName(innerDc.append) + "(bucket, " + makeOpMethodName(innerDc.size) + "(bucket), map(e))", stream, indent+8)
        emitWithIndent("}", stream, indent+6)
        if (gb.cond.isDefined) {
          emitWithIndent("}", stream, indent+4)
        }
        emitWithIndent("i += 1", stream, indent+4)
        emitWithIndent("}", stream, indent+2)
        emitWithIndent("fhashmap_from_shashmap(out)", stream, indent+2) // convert to ForgeHashMap
      case gbr:GroupByReduce =>
        val in = o.args.apply(gbr.argIndex)
        val inDc = ForgeCollections(getHkTpe(in.tpe))
        if (gbr.cond.isDefined) {
          emitWithIndent("def cond: " + repify(gbr.tpePars._1) + " => " + repify(MBoolean) + " = " + inline(o, gbr.cond.get), stream, indent+2)
        }
        emitWithIndent("def key: " + repify(gbr.tpePars._1) + " => " + repify(gbr.tpePars._2) + " = " + inline(o, gbr.key), stream, indent+2)
        emitWithIndent("def map: " + repify(gbr.tpePars._1) + " => " + repify(gbr.tpePars._3) + " = " + inline(o, gbr.map), stream, indent+2)
        emitWithIndent("def reduce: (" + repify(gbr.tpePars._3) + "," + repify(gbr.tpePars._3) + ") => " + repify(gbr.tpePars._3) + " = " + inline(o, gbr.reduce), stream, indent+2)
        emitWithIndent("val in = " + in.name, stream, indent+2)
        emitWithIndent("val out = new SHashMap["+quote(gbr.tpePars._2)+","+quote(gbr.tpePars._3)+"]()", stream, indent+2)
        emitWithIndent("var i = 0", stream, indent+2)
        emitWithIndent("while (i < " + makeOpMethodName(inDc.size) + "(in)"  + ") {", stream, indent+2)
        emitWithIndent("val e = " + makeOpMethodName(inDc.apply) + "(in, i)", stream, indent+4)
        if (gbr.cond.isDefined) {
          emitWithIndent("if (cond(e)) {", stream, indent+4)
        }
        emitWithIndent("val k = key(e)", stream, indent+6)
        emitWithIndent("if (!out.contains(k)) {", stream, indent+6)
        emitWithIndent("out(k) = map(e)", stream, indent+8)
        emitWithIndent("}", stream, indent+6)
        emitWithIndent("else {", stream, indent+6)
        emitWithIndent("out(k) = reduce(out(k), map(e))", stream, indent+8)
        emitWithIndent("}", stream, indent+6)
        if (gbr.cond.isDefined) {
          emitWithIndent("}", stream, indent+4)
        }
        emitWithIndent("i += 1", stream, indent+4)
        emitWithIndent("}", stream, indent+2)
        emitWithIndent("fhashmap_from_shashmap(out)", stream, indent+2)
      case foreach:Foreach =>
        val c = o.args.apply(foreach.argIndex)
        val dc = ForgeCollections(getHkTpe(c.tpe))
        emitWithIndent("def func: " + repify(foreach.tpePar) + " => " + repify(MUnit) + " = " + inline(o, foreach.func), stream, indent+2)
        emitWithIndent("val in = " + c.name, stream, indent+2)
        emitWithIndent("var i = 0", stream, indent+2)
        emitWithIndent("while (i < " + makeOpMethodName(dc.size) + "(in)" + ") {", stream, indent+2)
        emitWithIndent("func(" + makeOpMethodName(dc.apply) + "(in, i))", stream, indent+4)
        emitWithIndent("i += 1", stream, indent+4)
        emitWithIndent("}", stream, indent+2)
      case redirect:Redirect => {
        stream.print("/*redirect*/")
        emitFunc(redirect.func)
      }
      case codegen:CodeGen =>
        stream.print("/*codegen*/")
        val rule = codegen.decls.getOrElse($cala, err("could not find Scala codegen rule for op: " + o.name))
        // inline(o, rule.decl, quoteLiteral).split(nl).foreach { line => emitWithIndent(line, stream, indent) }
        emitFunc(rule.decl)
        stream.println()
      case Getter(structArgIndex,field) =>
        // stream.print("/*getter*/")
        val c = o.args.apply(structArgIndex).name
        // stream.print("Forge.getter" + "(" + c + ", " + c + ".")
        // emitFunc(field)
        // stream.println(")")
        inline(o, field, quoteLiteral).split(nl).foreach { line => emitWithIndent(c+"."+line, stream, indent+2 )}
        // emitWithIndent(inline(o, quotedArg(o.args.apply(structArgIndex).name)) + "." + field, stream, indent)
      case Setter(structArgIndex,field,value) =>
        // stream.print("/*setter*/")
        val c = o.args.apply(structArgIndex).name
        stream.print(" "*(indent+2)+c+".")
        // stream.print("Forge.setter" + "(" + c + ", " + c + ".")
        emitFunc(field)
        stream.print(" = ")
        emitFunc(value)
        stream.println()
        // stream.println(")")
        // emitWithIndent(inline(o, quotedArg(o.args.apply(structArgIndex).name)) + "." + field + " = " + inline(o,value), stream, indent)
        // emitWithIndent(inline(o, quotedArg(o.args.apply(structArgIndex).name)) + "." + field + " = ", stream, indent)
        // emitFunc(value)
      case Allocates(tpe,init) =>
        // stream.print("/*allocates*/")
        val initialVals = init.map(i => inline(o,i, quoteLiteral)).mkString(",")
        emitWithIndent("new " + quote(tpe) + "(" + initialVals + ")", stream, indent+2)
      case _ => stream.println("???")
    }
    emitWithIndent("}", stream, indent)
    // stream.println()
  }


  /**
   * Front-end codegen
   */

  def checkOps(opsGrp: DSLOps) {
    for (o <- unique(opsGrp.ops)) check(o)
  }

  def emitOpSyntax(opsGrp: DSLOps, stream: PrintWriter) {
    emitBlockComment("Operations", stream)
    stream.println()


    val objectOps = opsGrp.ops.filter(e=>e.style==staticMethod || e.style==directMethod || e.style==compilerMethod)
    val objects = objectOps.groupBy(_.grp.name)
    for ((name, ops) <- objects) {
      stream.println("object " + name + " {")

      // static ops
      emitComment("static ops", stream, 2)
      for (o <- ops if o.style == staticMethod) {
        stream.println("  " + makeSyntaxMethod(o))
      }
      if(ops.count(_.style == staticMethod)>0) stream.println()

      // direct ops
      emitComment("direct ops", stream, 2)
      for (o <- ops if o.style == directMethod) {
        stream.println("  " + makeSyntaxMethod(o))
      }
      if(ops.count(_.style == directMethod)>0) stream.println()

      // compiler ops
      emitComment("compiler ops", stream, 2)
      for (o <- ops if o.style == compilerMethod) {
        stream.print("  " + makeOpMethodSignature(o, withReturnTpe = Some(true)) + " =")
        emitImpl(o, stream, 2)
      }
      if(ops.count(_.style == compilerMethod)>0) stream.println()

      // abstract methods
      emitComment("abstract implemented ops", stream, 2)
      for (o <- unique(opsGrp.ops.filter(e=>e.style != compilerMethod && e.style != implicitMethod && !Impls(e).isInstanceOf[Redirect]))) {
        stream.print("  " + makeOpMethodSignature(o, withReturnTpe = Some(true)) + " =")
        emitImpl(o, stream, 2)
      }

      stream.println("}")
      stream.println()
    }

    // // implicits go in a base class for lower priority
    // val implicitOps = opsGrp.ops.filter(e=>e.style==implicitMethod)
    // if (!implicitOps.isEmpty) {
    //   if (unique(implicitOps).length != implicitOps.length) err("non-unique implicit op variants (e.g. Var, T args) are not yet supported")
    //   stream.println("trait " + opsGrp.name + "Base extends " + baseOpsCls(opsGrp.grp) + " {")
    //   stream.println("  this: " + dsl + " => ")
    //   stream.println()
    //   for (o <- implicitOps) {
    //     stream.println("  implicit " + makeSyntaxMethod(o, withReturnTpe = Some(true)))
    //   }
    //   stream.println()
    //   // we separate these just for generated code readability
    //   for (o <- implicitOps if !Impls(o).isInstanceOf[Redirect]) {
    //     stream.println("  " + makeOpMethodSignature(o, withReturnTpe = Some(true)))
    //   }
    //   stream.println("}")
    //   stream.println()
    // }

    // val base = if (implicitOps.length > 0) opsGrp.name + "Base" else baseOpsCls(opsGrp.grp)
    // stream.println("trait " + opsGrp.name + " extends " + base + " {")
    // stream.println("  this: " + dsl + " => ")
    // stream.println()

    // // static ops
    // val staticOps = opsGrp.ops.filter(e=>e.style==staticMethod)
    // val objects = staticOps.groupBy(_.grp.name)
    // for ((name, ops) <- objects) {
    //   stream.println("  object " + name + " {")
    //   for (o <- ops) {
    //     stream.println("    " + makeSyntaxMethod(o))
    //   }
    //   stream.println("  }")
    //   stream.println()
    // }

    // // direct ops
    // val directOps = opsGrp.ops.filter(e=>e.style==directMethod)
    // for (o <- directOps) {
    //   stream.println("  " + makeSyntaxMethod(o))
    // }
    // if (directOps.length > 0) {
    //   stream.println()
    // }

    // infix ops
    val allInfixOps = opsGrp.ops.filter(e=>e.style==infixMethod)

    val pimpOps = allInfixOps.filter(_.args.length > 0)
    if (pimpOps.nonEmpty) {
      // set up a pimp-my-library style promotion
      val ops = pimpOps.filterNot(o => getHkTpe(o.args.apply(0).tpe).name == "Var" ||
                                       (o.args.apply(0).tpe.stage == now && pimpOps.exists(o2 => o.args.apply(0).tpe.name == o2.args.apply(0).tpe.name && o2.args.apply(0).tpe.stage == future)))
      val classTpes = ops.map(_.args.apply(0).tpe).distinct
      for (classTpe <- classTpes) {
        val tpePars = classTpe match {
          case Def(TpeInst(_,args)) => args.filter(isTpePar).asInstanceOf[List[Rep[TypePar]]]
          case Def(TpePar(_,_,_)) => List(classTpe.asInstanceOf[Rep[TypePar]])
          case _ => classTpe.tpePars
        }
        val tpeArgs = classTpe match {
          case Def(TpeInst(hk,args)) => args.filterNot(isTpePar)
          case _ => Nil
        }

        val className = classTpe.name
        val dataStruct: Option[Exp[DSLData]] = DataStructs.get(classTpe)
        val fieldArgsString = dataStruct map makeFieldArgs getOrElse "" // can apply map to Option - returns result or None
        stream.println("class " + className + makeTpeParsWithBounds(classTpe.tpePars) + "(" + fieldArgsString + ")" + " { self => ")
        stream.println(dataStruct map (makeFieldsWithInitArgs) getOrElse "")
        val objectName = classTpe.name //opsGrp.grp.name
        stream.println("  import " + objectName + "._")

        // println()
        // println(className)
        for (o <- ops if quote(o.args.apply(0).tpe) == quote(classTpe)){// && !(!dataStruct.isEmpty && dataStruct.get.fields.contains((o.name, tpe(o.retTpe))))){
          // stream.println("  " + makeSyntaxMethod(o))
          // println(o.name + " "*(20-o.name.length) + o.args.length + "    " + o.args.map(x => (x.name,x.tpe.name)))
          // if there's an op with no arguments (only 1), same name and type as a field, skip it to avoid clash
          if (o.args.length > 1 || dataStruct.isEmpty || (o.args.length == 1 && !dataStruct.get.fields.contains((o.name, tpe(o.retTpe.name))))) {
            val otherArgs = makeArgsWithNowType(o.firstArgs.drop(1), o.effect != pure || o.name == "apply")
            val curriedArgs = o.curriedArgs.map(a => makeArgsWithNowType(a)).mkString("")
            val otherTpePars = o.tpePars.filterNot(p => tpePars.map(_.name).contains(p.name))
            val ret = if (Config.fastCompile) ": " + typifySome(o.retTpe) else ""
            stream.println("  def " + o.name + makeTpeParsWithBounds(otherTpePars) + otherArgs + curriedArgs
              + (makeImplicitArgsWithCtxBoundsWithType(o.tpePars diff otherTpePars, o.args, o.implicitArgs, without = tpePars)) + ret + " = " + makeOpMethodNameWithArgs(o))
          }
        }
        // println()
        stream.println("}")
        stream.println()
      }
      stream.println()
    }

    // for (o <- infixOps) {
    //   stream.println(makeSyntaxMethod(o, prefix = "  def infix_"))
    // }
    // stream.println()

    // // abstract methods
    // for (o <- unique(opsGrp.ops.filter(e=>e.style != compilerMethod && e.style != implicitMethod && !Impls(e).isInstanceOf[Redirect]))) {
    //   stream.println("  " + makeOpMethodSignature(o, withReturnTpe = Some(true)))
    // }

    // stream.println("}")

    // // compiler ops
    // val compilerOps = opsGrp.ops.filter(e=>e.style == compilerMethod)
    // if (!compilerOps.isEmpty) {
    //   stream.println("trait " + opsGrp.grp.name + "CompilerOps extends " + opsGrp.name + " {")
    //   stream.println("  this: " + dsl + " => ")
    //   stream.println()
    //   if (unique(compilerOps).length != compilerOps.length) err("non-unique compiler op variants (e.g. Var, T args) are not yet supported")
    //   for (o <- compilerOps) {
    //     if (Impls(o).isInstanceOf[Redirect]) {
    //       stream.println("  " + makeOpMethodSignature(o) + " = " + makeOpMethodNameWithFutureArgs(o, a => if (a.name ==  o.args.apply(0).name) "self" else simpleArgName(a)))
    //     }
    //     else {
    //       stream.println("  " + makeOpMethodSignature(o, withReturnTpe = Some(true)))
    //     }
    //   }
    //   stream.println("}")
    //   stream.println()
    // }
  }
}
