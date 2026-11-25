package reachablelinkage.linkage

import java.util.BitSet
import scala.annotation.tailrec

import reachablelinkage.classfile.{CallKind, CallRef, ClassInfo, Keys, Probe}
import reachablelinkage.classfile.AccessFlags.*
import reachablelinkage.classpath.ClassIndex
import reachablelinkage.symbols.{ClassId, DescId, NameId}

/** Which methods the application can reach. Static and special calls are exact; virtual and interface calls go to the declared method plus every override in a
  * class that has been reached (class hierarchy analysis over reached classes only, so classes nothing references never add edges); `invokedynamic` handles are
  * edges; reaching a class reaches its static initialiser and its supertypes'; a method that overrides a JDK method is a root, because the JDK calls it; and a
  * `ServiceLoader` provider's no-argument constructor is a root once its service interface is reached (always, for a JDK interface).
  *
  * It also records the guard each call site has (the `catch` handlers around it) and solves, for every method, which linkage errors are caught by every call
  * that can reach it: a method called only from inside handlers for an error is itself guarded against that error. A root, and the target of a method handle,
  * inherit nothing, since they can run from anywhere.
  *
  * Constant class names in reached classes are reflective roots. Not modelled yet, so these can be missed: computed reflection and framework callbacks. A class
  * is treated as instantiable as soon as anything references it (no RTA).
  */
private[linkage] final class MethodGraph private (
    reachedClasses: BitSet,
    reachedMethods: Slots[BitSet],
    virtualCalls: Slots[SlotTable],
    ancestors: Slots[Array[Int]],
    nodeBase: Slots[Integer],
    entryGuards: Array[Int],
    traverse: BitSet,
    provenance: Provenance,
    reflectedNames: BitSet
):

  def classReached(c: ClassId): Boolean = c.value >= 0 && reachedClasses.get(c.value)

  def methodReached(c: ClassId, method: Int): Boolean =
    method >= 0 && {
      val bits = reachedMethods.get(c.value)
      bits != null && bits.get(method)
    }

  /** How is the method (or, for -1, the class) reached? The walk runs from direct roots alone first, then adds what a reflection rule, a keep rule or the
    * class-load log makes a root, then the `$deserializeLambda$` hooks, so what a later phase adds has no path through an earlier one.
    */
  private def reach(c: ClassId, method: Int): Reach = provenance.of(c.value, method)

  /** The [[reachablelinkage.classfile.Guards]] bits that every call reaching method `method` of `c` passes through a handler for; 0 for a root or a method the
    * graph did not reach.
    */
  def inheritedGuard(c: ClassId, method: Int): Int =
    nodeBase.get(c.value) match
      case null => 0
      case base =>
        val node = base.intValue + method
        if method < 0 || node >= entryGuards.length || entryGuards(node) == MethodGraph.NotSolved then 0 else entryGuards(node)

  /** Could the abstract method `key`, declared by `declaring`, be called on an instance of `c`? Only if some reached code makes a virtual call on it through
    * `c` or one of its ancestors. A method of a JDK type is assumed to be called, since the JDK is not walked.
    */
  private def calledByCode(index: ClassIndex, c: ClassId, declaring: ClassId, key: Long): Boolean =
    val source = index.sourceIndex(declaring)
    val walked = source >= 0 && traverse.get(source)
    !walked || {
      def called(owner: Int): Boolean =
        virtualCalls.get(owner) match
          case null  => false
          case table => table.contains(key)
      called(c.value) || (ancestors.get(c.value) match
        case null  => false
        case chain => chain.exists(called))
    }

  /** A method some reached class looks up by a constant name (`getMethod("b")`) can be called through reflection, whatever its owner. */
  private def calledByReflection(name: NameId): Boolean = name.value >= 0 && reflectedNames.get(name.value)

  /** `Reachable` if the method holding the offending reference is reachable; else `Latent`. */
  def label(index: ClassIndex, f: Finding): Label =
    f.conflict match
      case Conflict.AbstractMethodNotImplemented(c, declaring, name, desc) =>
        // Breaks only when called: the class must be reached, and the method called through it.
        if classReached(c) && (calledByCode(index, c, declaring, Keys.of(name.value, desc.value)) || calledByReflection(name)) then Label.Reachable
        else Label.Latent
      case _ => labelReference(index, f)

  /** The method that holds the reference a finding points at, or -1 if it is not an instruction. */
  def holderOf(index: ClassIndex, f: Finding): Int =
    f.kind match
      case RefKind.Hierarchy | RefKind.Service => -1
      case kind                                =>
        index.get(f.conflict.inClass) match
          case null => -1
          case info =>
            kind match
              case RefKind.Method => info.methodOfMethodRef(f.refIndex)
              case RefKind.Field  => info.methodOfFieldRef(f.refIndex)
              case _              => info.methodOfClassRef(f.refIndex)

  /** How the finding's holder (its method, or its class when it is not an instruction) is reached. */
  def reach(index: ClassIndex, f: Finding): Reach =
    f.conflict match
      case Conflict.AbstractMethodNotImplemented(c, declaring, name, desc) =>
        // Called only by a name a rule looked up: that is a rule's reach, though the class itself may be reached directly.
        val onlyByName = !calledByCode(index, c, declaring, Keys.of(name.value, desc.value)) && calledByReflection(name)
        val base       = reach(c, -1)
        if onlyByName && base.ordinal < Reach.Rule.ordinal then Reach.Rule else base
      case _ if f.kind == RefKind.Service => Reach.Direct
      case _                              => reach(f.conflict.inClass, holderOf(index, f))

  /** The guard a finding inherits from the calls that reach the method it is in. */
  def inheritedGuard(index: ClassIndex, f: Finding): Int =
    val holder = holderOf(index, f)
    if holder < 0 then 0 else inheritedGuard(f.conflict.inClass, holder)

  private def labelReference(index: ClassIndex, f: Finding): Label =
    val c       = f.conflict.inClass
    val holder  = holderOf(index, f)
    val reached =
      if f.kind == RefKind.Service then classReached(c) || (index.exists(c) && !traverse.get(index.sourceIndex(c)))
      else if holder >= 0 then methodReached(c, holder)
      else classReached(c)
    if reached then Label.Reachable else Label.Latent

private[linkage] object MethodGraph:
  /** An entry guard no edge has narrowed: the node was not reached from a root. */
  private val NotSolved: Int = -1

  /** The mask of an edge whose target inherits nothing from its caller (a method handle). */
  private val Unguarded: Int = Int.MinValue

  /** @param appClasses
    *   every method of these classes starts the walk
    * @param traverse
    *   classpath entries the graph may enter (a bit per entry); the JDK is left out
    */
  def build(
      index: ClassIndex,
      resolver: Resolver,
      rules: KeepRules,
      dispatch: Dispatch,
      appClasses: Iterable[ClassId],
      loaded: Iterable[ClassId],
      traverse: BitSet
  ): MethodGraph =
    build(index, resolver, rules, dispatch, appClasses, loaded, traverse, new FrameworkTargets(index, rules, Generated.of(index, rules), traverse))

  /** As above, with the framework targets already prepared (the class walk asks the same questions). */
  def build(
      index: ClassIndex,
      resolver: Resolver,
      rules: KeepRules,
      dispatch: Dispatch,
      appClasses: Iterable[ClassId],
      loaded: Iterable[ClassId],
      traverse: BitSet,
      targets: FrameworkTargets
  ): MethodGraph =
    val b = new Builder(index, resolver, traverse, rules, dispatch, targets)
    b.run(appClasses, loaded)
    MethodGraph(b.reachedClasses, b.reachedMethods, b.virtualCalls, b.ancestorCache, b.nodeBase, b.solveGuards(), traverse, b.provenance, b.reflectedNames)

  private final class Builder(index: ClassIndex, resolver: Resolver, traverse: BitSet, rules: KeepRules, dispatch: Dispatch, targets: FrameworkTargets):
    private val syms      = index.syms
    private val initName  = syms.nameId("<init>")
    private val clinit    = key("<clinit>", "()V")
    private val noArgInit = key("<init>", "()V")
    // One function value for the whole walk: asked about every class a rule looks at.
    private val present: ClassId => Boolean = index.exists
    // The JDK calls this on a class when it deserialises one of its lambdas; it names every lambda body of the class.
    private val deserializeLambda = key("$deserializeLambda$", "(Ljava/lang/invoke/SerializedLambda;)Ljava/lang/Object;")
    // Methods the JDK calls by name when serializing, so no call site in the program mentions them.
    private val hooks: Array[Long] = Array(
      key("readObject", "(Ljava/io/ObjectInputStream;)V"),
      key("writeObject", "(Ljava/io/ObjectOutputStream;)V"),
      key("readObjectNoData", "()V"),
      key("readResolve", "()Ljava/lang/Object;"),
      key("writeReplace", "()Ljava/lang/Object;")
    )

    val reachedClasses = new BitSet
    // Under RTA, the classes something instantiates; under CHA a reached class dispatches, so this is unused.
    private val instantiated          = new BitSet
    val reachedMethods: Slots[BitSet] = new Slots[BitSet]
    private val work                  = new IntList
    // The walk runs in phases, one per kind of root, strongest first (see `Provenance`). During the first only the direct roots are
    // followed and the classes whose reflection rules are owed are noted; the next adds those and the class-load log, the last the
    // deserialisation hooks.
    val provenance                     = new Provenance
    private val owed                   = new IntList
    private def deferring: Boolean     = provenance.phase == Reach.Direct
    private def deserializing: Boolean = provenance.phase == Reach.Deserialization
    val ancestorCache                  = new Slots[Array[Int]]
    private val subtypes               = new Slots[IntList]
    val virtualCalls                   = new Slots[SlotTable]

    // The call graph for guard solving. A node is a method (a class's methods are numbered
    // consecutively from `nodeBase`) or the join of one virtual call (`owner`, method key); the join
    // fans a call out to the declared method and every override, however many call sites share it.
    val nodeBase         = new Slots[Integer]
    private var nextNode = 0
    private val roots    = new BitSet
    private val edgeFrom = new IntList
    private val edgeTo   = new IntList
    private val edgeMask = new IntList

    private def key(name: String, desc: String): Long = Keys.of(syms.nameId(name).value, syms.descId(desc).value)

    private def traversable(c: ClassId): Boolean =
      val s = index.sourceIndex(c)
      s >= 0 && traverse.get(s)

    private def newNode(): Int =
      val n = nextNode
      nextNode += 1
      n

    private def nodeOf(c: ClassId, method: Int): Int =
      val base = nodeBase.get(c.value) match
        case null =>
          val count = index.get(c) match
            case null => 0
            case info => info.methods.length
          val b = nextNode
          nextNode += count
          nodeBase.put(c.value, Integer.valueOf(b))
          b
        case existing => existing.intValue
      base + method

    private def edge(from: Int, to: Int, mask: Int): Unit =
      edgeFrom.push(from); edgeTo.push(to); edgeMask.push(mask)

    /** The walk, one phase per kind of root in order of strength; each phase adds its roots and follows them to the end before the next starts, so what a phase
      * newly reaches is recorded for it (`Provenance`). A new kind of root is a case of [[Reach]] and an entry here.
      */
    def run(appClasses: Iterable[ClassId], loaded: Iterable[ClassId]): Unit =
      Reach.values.foreach: reach =>
        provenance.enter(reach)
        reach match
          case Reach.Direct          => directRoots(appClasses)
          case Reach.Rule            => ruleRoots(loaded)
          case Reach.Deserialization => deserializationRoots()
        drain()

    /** The application's own code, and what the JDK itself runs. */
    private def directRoots(appClasses: Iterable[ClassId]): Unit =
      appClasses.foreach: c =>
        reachClass(c)
        index.get(c) match
          case null => ()
          case info =>
            @tailrec def roots(m: Int): Unit =
              if m < info.methods.length then { reachRoot(c, m); roots(m + 1) }
            roots(0)
      // JDK code loads providers of JDK service interfaces; nothing in the program names them.
      index.services.interfaces.foreach: service =>
        if index.exists(service) && !traversable(service) then rootProviders(service)

    /** What a reflection rule, a keep rule or the class-load log makes a root: the owed rules of the classes reached so far, and the log. */
    private def ruleRoots(loaded: Iterable[ClassId]): Unit =
      @tailrec def settle(i: Int): Unit =
        if i < owed.size then
          val c = ClassId(owed(i))
          index.get(c) match
            case null => ()
            case info => reflectionRoots(c, info)
          settle(i + 1)
      settle(0)
      // A class a real run loaded: it is initialised, and one of its constructors may have run, so each is a
      // root (which also makes the class instantiated under RTA). Which methods ran is not known.
      loaded.foreach: c =>
        reachClass(c)
        index.get(c) match
          case null => ()
          case info =>
            @tailrec def constructors(m: Int): Unit =
              if m < info.methods.length then
                if info.methods(m).name == initName then reachRoot(c, m)
                constructors(m + 1)
            constructors(0)

    /** The deserialisation hook of every class reached so far (and, from here on, of each one reached). */
    private def deserializationRoots(): Unit =
      @tailrec def hooks(r: Int): Unit =
        if r >= 0 then
          index.get(ClassId(r)) match
            case null => ()
            case info => rootDeserializeHook(ClassId(r), info)
          hooks(reachedClasses.nextSetBit(r + 1))
      hooks(reachedClasses.nextSetBit(0))

    @tailrec private def drain(): Unit =
      if !work.isEmpty then
        val method = work.pop()
        val cls    = work.pop()
        expand(ClassId(cls), method)
        drain()

    /** A method that something outside the graph can call, so it inherits no guard. */
    private def reachRoot(c: ClassId, method: Int): Unit =
      reachMethod(c, method)
      if method >= 0 && traversable(c) then
        roots.set(nodeOf(c, method))
        index.get(c) match
          case null => ()
          case info => if info.methods(method).name == initName then instantiate(c)

    private def reachMethod(c: ClassId, method: Int): Unit =
      if method >= 0 && traversable(c) then
        val bits = reachedMethods.getOrCreate(c.value)(new BitSet)
        if !bits.get(method) then
          bits.set(method)
          provenance.reachedMethod(c.value, method)
          reachClass(c)
          work.push(c.value)
          work.push(method)

    /** Reaches a method and records the edge into it from `from`, guarded by `mask` at the call. */
    private def reachVia(from: Int, mask: Int, c: ClassId, method: Int): Unit =
      reachMethod(c, method)
      if method >= 0 && traversable(c) then edge(from, nodeOf(c, method), mask)

    private def reachClass(c: ClassId): Unit =
      if !c.isNone && !reachedClasses.get(c.value) && traversable(c) then
        reachedClasses.set(c.value)
        provenance.reachedClass(c.value)
        index.get(c) match
          case null => ()
          case info => onClassReached(c, info)

    /** `ServiceLoader` makes a provider with its public no-argument constructor. */
    private def rootProviders(service: ClassId): Unit =
      index.services
        .providersOf(service)
        .foreach: provider =>
          reachClass(provider)
          index.get(provider) match
            case null => ()
            case info => reachRoot(provider, info.methodIndex(noArgInit, CallKind.Instance))

    /** Constant-name reflection: a string constant in a reached class that names a class on the classpath (`Class.forName("x.Y")`, `loadClass`, a
      * `Class.forName(...).newInstance()`) loads it, runs its initialiser and may construct it, so the class and its constructors are reached
      * (`getConstructor(types).newInstance(args)` picks any of them). An over-approximation (any such string counts, whatever consumes it), which errs towards
      * Reachable.
      */
    private def reflectiveRoots(info: ClassInfo): Unit =
      info.probes.foreach:
        case Probe.ClassName(name) => buildNamedClass(syms.findClass(name))
        // A fragment (`.kqueue.KQueue`) a class loads by name names every class that ends with it.
        case Probe.NameTail(suffix) => index.endingWith(suffix).foreach(buildNamedClass)
        // A lambda is a direct fact, handled when the class is reached; a method name is for `reflectedMethods`.
        case Probe.MethodName(_) | Probe.LambdaInterface(_) => ()

    /** A class a constant names is loaded and built, unless the framework would ignore it for want of a class its conditions name. */
    private def buildNamedClass(target: ClassId): Unit =
      if !target.isNone && isEnabledClass(target) then buildThroughConstructors(target)

    /** Is `target` a traversed class on the classpath that no failing condition rules out? */
    private def isEnabledClass(target: ClassId): Boolean =
      traversable(target) && (index.get(target) match
        case null => false
        case info => targets.considers(info))

    /** The class is reached, counts as instantiated, and every constructor is a root: whatever builds it picks one by the arguments it has. */
    private def buildThroughConstructors(target: ClassId): Unit =
      if traversable(target) then
        reachClass(target)
        instantiate(target)
        index.get(target) match
          case null => ()
          case info => rootConstructors(target, info)

    // Method names some reached class looks up by constant name (`getMethod("run")`), as name ids.
    val reflectedNames = new BitSet

    /** A reflectively looked-up method can be any method of that name, in any class reached, so each is a root: the receiver class is not tracked. Names are
      * rare, so scanning the reached classes once per new name is cheap.
      */
    private def reflectedMethods(c: ClassId, info: ClassInfo): Unit =
      info.reflectedMethodNames.foreach: n =>
        val id = syms.nameId(n).value
        if !reflectedNames.get(id) then
          reflectedNames.set(id)
          @tailrec def eachReached(r: Int): Unit =
            if r >= 0 then
              index.get(ClassId(r)) match
                case null => ()
                case ri   => rootNamed(ClassId(r), ri, id)
              eachReached(reachedClasses.nextSetBit(r + 1))
          eachReached(reachedClasses.nextSetBit(0))
      if !reflectedNames.isEmpty then
        @tailrec def named(m: Int): Unit =
          if m < info.methods.length then
            val member = info.methods(m)
            if reflectedNames.get(member.name.value) && member.name != initName then reachRoot(c, m)
            named(m + 1)
        named(0)

    private def rootNamed(c: ClassId, info: ClassInfo, name: Int): Unit =
      @tailrec def from(m: Int): Unit =
        if m < info.methods.length then
          if info.methods(m).name.value == name then reachRoot(c, m)
          from(m + 1)
      from(0)

    /** The framework packs: what Guice, Jackson and the like call by reflection on a class that was reached. */
    private def frameworkRoots(c: ClassId, info: ClassInfo): Unit =
      if targets.considers(info) then enabledFrameworkRoots(c, info)

    private def enabledFrameworkRoots(c: ClassId, info: ClassInfo): Unit =
      rootKeptMembers(c, info)
      targets.eachBuiltOfConsidered(c, info)(buildThroughConstructors)
      targets.eachMemberClassOfConsidered(c, info)(reachIfTraversable)
      rootNoArgConstructorIfMarked(c, info)
      rootAllConstructorsIfMarked(c, info)
      overrideRoots(c, info)
      beanRoots(info)
      builtRoots(info)
      constructedRoots(info)

    /** A framework that calls a method of the class has an instance. */
    private def rootKeptMembers(c: ClassId, info: ClassInfo): Unit =
      if rules.keptMethods(info, present)(m => reachRoot(c, m)) > 0 then instantiate(c)

    private def reachIfTraversable(c: ClassId): Unit = if traversable(c) then reachClass(c)

    private def rootNoArgConstructorIfMarked(c: ClassId, info: ClassInfo): Unit =
      if rules.needsNoArgConstructor(info) then
        instantiate(c)
        reachRoot(c, info.methodIndex(noArgInit, CallKind.Instance))

    private def rootAllConstructorsIfMarked(c: ClassId, info: ClassInfo): Unit =
      if rules.needsConstructors(info) then
        instantiate(c)
        rootConstructors(c, info)

    /** A `BeanArguments` rule: a class literal this class hands to a call the rule names is a class a framework builds through its no-argument constructor and
      * reads and writes through its bean accessors.
      */
    private def beanRoots(info: ClassInfo): Unit =
      rules.beanTargets(info): target =>
        if traversable(target) then
          reachClass(target)
          instantiate(target)
          index.get(target) match
            case null => ()
            case ti   =>
              reachRoot(target, ti.methodIndex(noArgInit, CallKind.Instance))
              beanAccessors(target, ti)

    /** A `ConstructedArguments` rule: a class handed to a call it names is built by the framework through whichever constructor fits, so all of them are roots.
      */
    private def constructedRoots(info: ClassInfo): Unit =
      rules.constructedTargets(info)(buildThroughConstructors)

    /** Every constructor of `c` is a root: whatever builds it by reflection picks one by the arguments it has. */
    private def rootConstructors(c: ClassId, ci: ClassInfo): Unit =
      @tailrec def from(m: Int): Unit =
        if m < ci.methods.length then
          if ci.methods(m).name == initName then reachRoot(c, m)
          from(m + 1)
      from(0)

    /** A `BuiltArguments` rule: a class handed to a call it names is built through its no-argument constructor, and nothing else of it is called. */
    private def builtRoots(info: ClassInfo): Unit =
      rules.builtTargets(info): target =>
        if traversable(target) then
          reachClass(target)
          instantiate(target)
          index.get(target) match
            case null => ()
            case ti   => reachRoot(target, ti.methodIndex(noArgInit, CallKind.Instance))

    /** The public `getX`, `isX` and `setX` methods of `c` and of its ancestors on the classpath. */
    private def beanAccessors(c: ClassId, info: ClassInfo): Unit =
      def isAccessor(name: String): Boolean =
        ((name.startsWith("get") || name.startsWith("set")) && name.length > 3) || (name.startsWith("is") && name.length > 2)
      def accessorsOf(cl: ClassId, i: ClassInfo): Unit =
        @tailrec def from(m: Int): Unit =
          if m < i.methods.length then
            val member = i.methods(m)
            if member.access.has(Public) && !member.access.has(Static) && isAccessor(syms.name(member.name)) then reachRoot(cl, m)
            from(m + 1)
        from(0)
      accessorsOf(c, info)
      ancestorsOf(c, info).foreach: a =>
        if traversable(ClassId(a)) then
          index.get(ClassId(a)) match
            case null => ()
            case ai   => accessorsOf(ClassId(a), ai)

    /** An `Overrides` rule: a reached class below a named type is built by a framework, which calls the methods of it that override ones the named type (or
      * what it extends) declares.
      */
    private def overrideRoots(c: ClassId, info: ClassInfo): Unit =
      if rules.hasOverrideRules && !info.isInterface then
        val named = ancestorsOf(c, info).filter(rules.isOverrideType)
        if named.nonEmpty then
          if !info.isAbstract then
            instantiate(c)
            reachRoot(c, info.methodIndex(noArgInit, CallKind.Instance))
          val declaring = named.flatMap: n =>
            index.get(ClassId(n)) match
              case null => Array(n)
              case t    => n +: ancestorsOf(ClassId(n), t)
          @tailrec def from(m: Int): Unit =
            if m < info.methods.length then
              val member = info.methods(m)
              if !member.access.has(Static) && !member.access.has(Private) && member.name != initName
                && overridable(declaring, Keys.of(member.name.value, member.desc.value))
              then reachRoot(c, m)
              from(m + 1)
          from(0)

    /** Everything reflection and the framework rules make a root on a reached class. */
    private def reflectionRoots(c: ClassId, info: ClassInfo): Unit =
      reflectiveRoots(info)
      reflectedMethods(c, info)
      frameworkRoots(c, info)

    /** Only a lambda rebuilt from its serialised form runs through this, without the code that created it. */
    private def rootDeserializeHook(c: ClassId, info: ClassInfo): Unit =
      reachRoot(c, info.methodIndex(deserializeLambda, CallKind.Static))

    private def onClassReached(c: ClassId, info: ClassInfo): Unit =
      rootProviders(c)
      if deferring then owed.push(c.value) else reflectionRoots(c, info)
      if deserializing then rootDeserializeHook(c, info)
      // A class loads and initialises its supertypes first.
      if !info.superName.isNone then reachClass(info.superName)
      info.interfaces.foreach(reachClass)
      reachRoot(c, info.methodIndex(clinit, CallKind.Static))

      // The JDK deserialises a class with a serialization hook without a `new` anyone can see.
      if dispatch == Dispatch.Rta && hasHook(info) then instantiate(c)
      // A lambda or method reference in the class makes an instance of its functional interface: the program does it, so it is not a rule's doing.
      if dispatch == Dispatch.Rta then instantiateLambdaInterfaces(info)
      // Under CHA a reached class dispatches; under RTA it waits until something instantiates it.
      if dispatch == Dispatch.Cha then becomeDispatchable(c, info)

    /** A class that virtual calls can now land in: it joins the subtype registry, its overrides of methods outside the graph become roots, and calls already
      * seen on it or an ancestor dispatch here.
      */
    private def becomeDispatchable(c: ClassId, info: ClassInfo): Unit =
      val ancestors = ancestorsOf(c, info)
      register(c.value, c.value)
      ancestors.foreach(register(_, c.value))
      overridesFromOutside(c, info, ancestors)
      dispatchPending(c, c.value)
      ancestors.foreach(dispatchPending(c, _))

    /** Something builds an instance of `c`: under RTA it can now be the target of a virtual call. */
    /** A lambda creates an instance of its functional interface, so under RTA the interface dispatches: a call on it, or on a superinterface, lands in the
      * default methods the lambda's class inherits (Scala's `JFunction1$mcII$sp.apply(Object)` bridge). CHA already reaches them.
      */
    private def instantiateLambdaInterfaces(info: ClassInfo): Unit =
      info.probes.foreach:
        case Probe.LambdaInterface(name) => instantiateLambdaType(syms.findClass(name))
        case _                           => ()

    private def instantiateLambdaType(c: ClassId): Unit =
      if dispatch == Dispatch.Rta && !c.isNone && !instantiated.get(c.value) && traversable(c) then
        index.get(c) match
          case null => ()
          case info =>
            if info.isInterface then
              instantiated.set(c.value)
              reachClass(c)
              becomeDispatchable(c, info)

    private def instantiate(c: ClassId): Unit =
      if dispatch == Dispatch.Rta && !c.isNone && !instantiated.get(c.value) && traversable(c) then
        index.get(c) match
          case null => ()
          case info =>
            if !info.isAbstract && !info.isInterface then
              instantiated.set(c.value)
              reachClass(c)
              becomeDispatchable(c, info)

    private def hasHook(info: ClassInfo): Boolean = hooks.exists(info.declaresMethod(_, CallKind.Instance))

    /** Reaches the method a call on an instance of `sub` runs: the nearest declaration up the chain, so a method `sub` inherits counts. Under CHA `sub` is any
      * reached subtype, under RTA only one something instantiates. (CHA once looked only at what `sub` declares itself, which missed an implementation
      * inherited from a superclass that is not a subtype of the call's owner: netty's `AbstractChannel` runs `DefaultAttributeMap.attr` for a call on
      * `Channel`.)
      */
    private def reachTarget(slot: Int, sub: ClassId, k: Long): Unit =
      resolver.method(sub, NameId((k >>> 32).toInt), DescId(k.toInt), CallKind.Instance) match
        case Resolution.Found(declaring, member) => reachVia(slot, 0, declaring, member)
        case _                                   => ()

    private def register(owner: Int, sub: Int): Unit = subtypes.getOrCreate(owner)(new IntList).push(sub)

    private def dispatchPending(c: ClassId, owner: Int): Unit =
      virtualCalls.get(owner) match
        case null  => ()
        case table => table.foreach((k, slot) => reachTarget(slot, c, k))

    /** Overrides of methods declared outside the traversed classes, and serialization hooks. */
    private def overridesFromOutside(c: ClassId, info: ClassInfo, ancestors: Array[Int]): Unit =
      val external                    = ancestors.filter(a => !traversable(ClassId(a)))
      @tailrec def from(m: Int): Unit =
        if m < info.methods.length then
          val member    = info.methods(m)
          val isStatic  = member.access.has(Static)
          val isPrivate = member.access.has(Private)
          if !isStatic && member.name != initName then
            val k = Keys.of(member.name.value, member.desc.value)
            if isHook(k) || (!isPrivate && declaredBy(external, k)) then reachRoot(c, m)
          from(m + 1)
      from(0)

    /** Does one of `classes` declare an instance method `k` that a subclass can override (not private)? */
    private def overridable(classes: Array[Int], k: Long): Boolean =
      @tailrec def from(i: Int): Boolean =
        i < classes.length && (index.get(ClassId(classes(i))) match
          case null => from(i + 1)
          case info =>
            val at = info.methodIndex(k, CallKind.Instance)
            (at >= 0 && !info.methods(at).access.has(Private)) || from(i + 1))
      from(0)

    private def isHook(k: Long): Boolean =
      @tailrec def from(i: Int): Boolean = i < hooks.length && (hooks(i) == k || from(i + 1))
      from(0)

    private def declaredBy(classes: Array[Int], k: Long): Boolean =
      @tailrec def from(i: Int): Boolean =
        i < classes.length && (index.get(ClassId(classes(i))) match
          case null => from(i + 1)
          case info => info.declaresMethod(k, CallKind.Instance) || from(i + 1))
      from(0)

    /** Every supertype of `c` that exists on the classpath, transitively. */
    private def ancestorsOf(c: ClassId, info: ClassInfo): Array[Int] =
      ancestorCache.get(c.value) match
        case null =>
          val out                      = new IntList
          val seen                     = new BitSet
          def visit(id: ClassId): Unit =
            if !id.isNone && !seen.get(id.value) then
              seen.set(id.value)
              index.get(id) match
                case null => ()
                case i    =>
                  out.push(id.value)
                  visit(i.superName)
                  i.interfaces.foreach(visit)
          visit(info.superName)
          info.interfaces.foreach(visit)
          val result = out.toArray
          ancestorCache.put(c.value, result)
          result
        case cached => cached

    /** Follows every reference made by one reachable method. */
    private def expand(c: ClassId, method: Int): Unit =
      index.get(c) match
        case null => ()
        case info =>
          val caller                                 = nodeOf(c, method)
          @tailrec def calls(i: Int, end: Int): Unit =
            if i < end then { call(caller, info, i); calls(i + 1, end) }
          calls(info.methodRefStart(method), info.methodRefStart(method + 1))
          val fields                                       = info.fieldRefs
          @tailrec def fieldOwners(i: Int, end: Int): Unit =
            if i < end then { reachClass(ClassId(fields(i * 3))); fieldOwners(i + 1, end) }
          fieldOwners(info.fieldRefStart(method), info.fieldRefStart(method + 1))
          val types = info.classRefs
          // Both lists are in order, so the `new` operands of this method merge with its type references.
          @tailrec def typeRefs(i: Int, end: Int, n: Int): Unit =
            if i < end then
              val target = ClassId(types(i))
              reachClass(target)
              val isNew = n < info.newRefs.length && info.newRefs(n) == i
              if isNew then instantiate(target)
              typeRefs(i + 1, end, if isNew then n + 1 else n)
          val firstType = info.classRefStart(method)
          typeRefs(firstType, info.classRefStart(method + 1), info.newRefFrom(firstType))

    /** One call or method handle: reference `entry` of `info`, made from node `caller`. */
    private def call(caller: Int, info: ClassInfo, entry: Int): Unit =
      val refs  = info.methodRefs
      val at    = entry * 4
      val owner = ClassId(refs(at))
      val name  = NameId(refs(at + 1))
      val desc  = DescId(refs(at + 2))
      val code  = refs(at + 3)
      val kind  = CallRef.kind(code)
      // A handle's target runs later, elsewhere: it inherits nothing, not even this site's handlers.
      val mask = if CallRef.isHandle(code) then Unguarded else info.guardOfMethodRef(entry)
      reachClass(owner)
      // A constructor reference (`Foo::new`) builds a Foo when it is called.
      if CallRef.isHandle(code) && name == initName then instantiate(owner)
      val found = resolver.method(owner, name, desc, kind)
      // Constructors are exact; any other instance call can land in an override.
      if kind == CallKind.Instance && name != initName then
        val slot = slotFor(owner, Keys.of(name.value, desc.value), found)
        edge(caller, slot, mask)
      else
        found match
          case Resolution.Found(declaring, member) => reachVia(caller, mask, declaring, member)
          case _                                   => ()

    /** The join node for virtual calls to method `k` on `owner`, made on first use: it leads to the declared method and to the override in each class reached
      * so far; later classes add theirs.
      */
    private def slotFor(owner: ClassId, k: Long, found: Resolution): Int =
      val table    = virtualCalls.getOrCreate(owner.value)(new SlotTable)
      val existing = table.get(k)
      if existing >= 0 then existing
      else
        val slot = newNode()
        table.put(k, slot)
        found match
          case Resolution.Found(declaring, member) => reachVia(slot, 0, declaring, member)
          case _                                   => ()
        subtypes.get(owner.value) match
          case null => ()
          case subs =>
            @tailrec def from(i: Int): Unit =
              if i < subs.size then
                reachTarget(slot, ClassId(subs(i)), k)
                from(i + 1)
            from(0)
        slot

    /** For every node, the guard bits that all calls reaching it carry: the greatest solution of `entry(v) = AND over edges (u, v, mask) of (mask | entry(u))`,
      * with roots fixed at 0 and a method handle's edge contributing 0. Narrowing only, so a worklist reaches it.
      */
    def solveGuards(): Array[Int] =
      val n                            = nextNode
      val entry                        = Array.fill(n)(NotSolved)
      val edges                        = edgeFrom.size
      val start                        = new Array[Int](n + 1)
      @tailrec def count(e: Int): Unit =
        if e < edges then { start(edgeFrom(e) + 1) += 1; count(e + 1) }
      count(0)
      @tailrec def accumulate(v: Int): Unit =
        if v < n then { start(v + 1) += start(v); accumulate(v + 1) }
      accumulate(0)
      val fill                         = java.util.Arrays.copyOf(start, n)
      val order                        = new Array[Int](edges)
      @tailrec def place(e: Int): Unit =
        if e < edges then
          val u = edgeFrom(e)
          order(fill(u)) = e
          fill(u) += 1
          place(e + 1)
      place(0)

      val queue                       = new IntList
      val queued                      = new BitSet
      @tailrec def seed(r: Int): Unit =
        if r >= 0 && r < n then
          entry(r) = 0
          queue.push(r)
          queued.set(r)
          seed(roots.nextSetBit(r + 1))
      seed(roots.nextSetBit(0))
      // Narrows the target of each edge leaving `u`, queueing the ones that changed.
      @tailrec def relax(u: Int, k: Int): Unit =
        if k < start(u + 1) then
          val edgeId   = order(k)
          val target   = edgeTo(edgeId)
          val mask     = edgeMask(edgeId)
          val carried  = if mask == Unguarded then 0 else mask | entry(u)
          val narrowed = if entry(target) == NotSolved then carried else entry(target) & carried
          if narrowed != entry(target) then
            entry(target) = narrowed
            if !queued.get(target) then
              queue.push(target)
              queued.set(target)
          relax(u, k + 1)
      @tailrec def solve(): Unit =
        if !queue.isEmpty then
          val u = queue.pop()
          queued.clear(u)
          relax(u, start(u))
          solve()
      solve()
      entry
