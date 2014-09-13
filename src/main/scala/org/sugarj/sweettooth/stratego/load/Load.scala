package org.sugarj.sweettooth.stratego.load

import common.FileCommands
import common.path.{AbsolutePath, Path, RelativePath}
import org.spoofax.interpreter.terms.{IStrategoString, IStrategoList, IStrategoAppl, IStrategoTerm}
import org.spoofax.terms.{StrategoConstructor, StrategoList, StrategoAppl, TermFactory}
import org.spoofax.terms.io.TAFTermReader
import org.strategoxt.lang.StrategoExit
import org.strategoxt.strj.main_strj_0_0
import org.sugarj.sweettooth.stratego.Syntax._
import org.sugarj.sweettooth.stratego.lib
import org.sugarj.sweettooth.stratego.lib.Library

/**
 * Created by seba on 11/09/14.
 */
object Load {
  def makeAbsolute(f: String) =
    if (AbsolutePath.acceptable(f))
      new AbsolutePath(f)
    else
      new AbsolutePath(s"./$f")

  def preload(path: String, cp: List[String]): Path = {
    val preloadDir = makeAbsolute(FileCommands.dropFilename(path))
    val preloadName = FileCommands.fileName(path) + "-norm." + FileCommands.getExtension(path)
    val preloadPath = new RelativePath(preloadDir, preloadName)
    val preloadHashName = FileCommands.fileName(path) + "-hash"
    val preloadHashPath = new RelativePath(preloadDir, preloadHashName)

    val nowHash = FileCommands.fileHash(makeAbsolute(path))

    if (FileCommands.exists(preloadPath) &&
        FileCommands.exists(preloadHashPath) &&
        FileCommands.readFileAsString(preloadHashPath).toInt == nowHash)
      return preloadPath

    val normalizedPath = normalizeStratego(path, cp)
    FileCommands.copyFile(normalizedPath, preloadPath)
    FileCommands.writeToFile(preloadHashPath, nowHash.toString)

    preloadPath
  }

  def load(path: String, cp: List[String], base: Library): Library = {
    val normalizedPath = preload(path, cp)
    val moduleTree = parseTerm(normalizedPath)
    val defs = readModuleTree(moduleTree, base)
    base + (new Library {
      override val DEFS: Map[Symbol, Def] = defs
    })
  }

  def normalizeStratego(path: String, cp: List[String]): RelativePath = {
    val ctx = org.strategoxt.strj.Main.init()

    val outdir = FileCommands.newTempDir()

    var cmd = List[String]()
    cmd ++= List("-i", path)
    cmd ++= List("-o", outdir.getAbsolutePath)
    cmd ++= List("-F")

    for (p <- cp)
      cmd ++= List("-I", p)

    try {
      ctx.invokeStrategyCLI(main_strj_0_0.instance, "strj", cmd: _*);
    } catch {
      case e: StrategoExit => if (e.getValue != 0) throw e
    }

    val fname = outdir.getFile.listFiles()(0).getName
    new RelativePath(outdir, fname)
  }

  val factory = new TermFactory

  def parseTerm(path: Path): IStrategoTerm =
    new TAFTermReader(factory).parseFromFile(path.getAbsolutePath)


  object STerm {
    def apply(name: String, kid: IStrategoTerm, kids: IStrategoTerm*): IStrategoAppl = apply(name, kid::List(kids:_*))
    def apply(name: String, kids: List[IStrategoTerm]): IStrategoAppl =
      new StrategoAppl(new StrategoConstructor(name, kids.size), kids.toArray, factory.makeList(), IStrategoTerm.MUTABLE)

    def unapplySeq(t: IStrategoTerm): Option[(String, List[IStrategoTerm])] =
      if (t.isInstanceOf[IStrategoAppl]) {
        val appl = t.asInstanceOf[IStrategoAppl]
        Some((appl.getConstructor.getName, appl.getAllSubterms.toList))
      }
      else
        None
  }
  object SList {
    def apply(kid: IStrategoTerm, kids: IStrategoTerm*): IStrategoList = apply(kid::List(kids:_*))
    def apply(kids: List[IStrategoTerm]): IStrategoList =
      if (kids.isEmpty)
        factory.makeList()
      else
        factory.makeListCons(kids.head, apply(kids.tail))

    def unapplySeq(t: IStrategoTerm): Option[List[IStrategoTerm]] =
      if (t.isInstanceOf[IStrategoList])
        Some(t.getAllSubterms.toList)
      else
        None
  }
  object SString {
    def unapply(t: IStrategoString) = Some(t.stringValue)
  }

  def readModuleTree(tree: IStrategoTerm, base: Library): Map[Symbol, Def] = tree match {
      case STerm("Specification",
            SList(
             STerm("Signature", _),
             STerm("Strategies", SList(strats@_*)))) => Map() ++ strats.flatMap(readStrategyDef(_, base))
    }

  def readStrategyDef(tree: IStrategoTerm, base: Library): Option[(Symbol, Def)] = tree match {
    case STerm("SDefT", 
          SString(name),
          SList(sargs@_*), 
          SList(targs@_*),
          body) => Some(Symbol(name) -> Def(sargs.toList map readSArg, targs.toList map readTArg, readStrategy(body)))
    case STerm("ExtSDef", SString(name), _, _) =>
      if (base.DEFS.contains(Symbol(name)))
        None
      else
        throw new IllegalArgumentException(s"External strategy $name is undefined in base lib")
    case _ => ???
  }

  def readSArg(tree: IStrategoTerm): Symbol = ???
  def readTArg(tree: IStrategoTerm): Symbol = ???

  def readStrategy(tree: IStrategoTerm): Exp = {
    import scala.language.implicitConversions
    implicit def read(tree: IStrategoTerm) = readStrategy(tree)
    tree match {
      case STerm("Match", t) => Match(readPat(t))
      case STerm("Build", t) => Build(readPat(t))
      case STerm("Id") => Scoped('x, Seq(Match('x), Build('x)))
      case STerm("Fail") => Seq(Build('Fail0@@()), Match('Fail1@@()))
      case STerm("Seq", e1, e2) => Seq(e1, e2)
      case STerm("GuardedLChoice", cnd, thn, els) => If(cnd, thn, els)
      case STerm("CallT",
            STerm("SVar", SString(name)),
            SList(sargs@_*),
            SList(targs@_*)) => Call(Symbol(name), sargs.toList map read, targs.toList map readPat)
      case STerm("Scope", SList(SString(v), vars@_*), e) =>
        if (vars.isEmpty)
          Scoped(Symbol(v), e)
        else
          Scoped(Symbol(v), STerm("Scope", SList(vars.toList), e))
      case _ => ???
    }
  }

  def readPat(tree: IStrategoTerm): Pat = {
    import scala.language.implicitConversions
    implicit def read(tree: IStrategoTerm) = readPat(tree)
    tree match {
      case STerm("Var", SString(name)) => Pat.Var(Symbol(name))
      case STerm("Anno", t, _) => t
      case STerm("Op", SString(""), SList(kids@_*)) => Pat.App('_, kids.toList map readPat)
      case STerm("Op", SString(cons), SList(kids@_*)) => Pat.App(Symbol(cons), kids.toList map readPat)
      case STerm("Str", SString(s)) => lib.String.makeString(s)
      case _ => ???
    }
  }
}
