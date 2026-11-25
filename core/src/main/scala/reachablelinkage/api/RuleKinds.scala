package reachablelinkage.api

import reachablelinkage.api.KeepRuleKind.*
import reachablelinkage.linkage.Keep
import reachablelinkage.linkage.RuleNames.*

/** What the facade knows about one kind of rule: its directive, the form of its names, how to make the engine's rule from them and how to read them back. */
private[api] final case class RuleKind(directive: String, form: NameForm, build: Set[String] => Keep, names: PartialFunction[Keep, Set[String]])

/** Every kind of rule described once. `build` is a match over `KeepRuleKind`, so a kind without a description does not compile; the reverse direction (an
  * engine rule that no kind reads back) is pinned by a test that counts the cases of `Keep`. The descriptions are made once, not on every lookup.
  */
private[api] object RuleKinds:

  // Making the engine's rule from the text of its names, one helper per form of name.
  private def classes(make: Set[ClassName] => Keep): Set[String] => Keep   = names => make(names.map(ClassName(_)))
  private def calls(make: Set[CallName] => Keep): Set[String] => Keep      = names => make(names.map(CallName(_)))
  private def files(make: Set[FileName] => Keep): Set[String] => Keep      = names => make(names.map(FileName(_)))
  private def suffixes(make: Set[NameSuffix] => Keep): Set[String] => Keep = names => make(names.map(NameSuffix(_)))

  private def build(kind: KeepRuleKind): RuleKind = kind match
    case KEEP_MEMBERS            => RuleKind("keep-members", NameForm.ClassNames, classes(Keep.Members(_)), { case Keep.Members(a) => a.map(_.value) })
    case KEEP_NO_ARG_CONSTRUCTOR =>
      RuleKind("keep-no-arg-constructor", NameForm.ClassNames, classes(Keep.NoArgConstructor(_)), { case Keep.NoArgConstructor(a) => a.map(_.value) })
    case FOLLOW_CLASS_VALUES =>
      RuleKind("follow-class-values", NameForm.ClassNames, classes(Keep.ClassValues(_)), { case Keep.ClassValues(a) => a.map(_.value) })
    case KEEP_OVERRIDES      => RuleKind("keep-overrides", NameForm.ClassNames, classes(Keep.Overrides(_)), { case Keep.Overrides(t) => t.map(_.value) })
    case KEEP_BEAN_ARGUMENTS =>
      RuleKind("keep-bean-arguments", NameForm.CallNames, calls(Keep.BeanArguments(_)), { case Keep.BeanArguments(c) => c.map(_.value) })
    case KEEP_BUILT_ARGUMENTS =>
      RuleKind("keep-built-arguments", NameForm.CallNames, calls(Keep.BuiltArguments(_)), { case Keep.BuiltArguments(c) => c.map(_.value) })
    case KEEP_CONSTRUCTED_ARGUMENTS =>
      RuleKind("keep-constructed-arguments", NameForm.CallNames, calls(Keep.ConstructedArguments(_)), { case Keep.ConstructedArguments(c) => c.map(_.value) })
    case KEEP_CLASSES_NAMED_IN =>
      RuleKind("keep-classes-named-in", NameForm.FileNames, files(Keep.ClassesNamedIn(_)), { case Keep.ClassesNamedIn(f) => f.map(_.value) })
    case KEEP_CONSTRUCTORS => RuleKind("keep-constructors", NameForm.ClassNames, classes(Keep.Constructors(_)), { case Keep.Constructors(a) => a.map(_.value) })
    case REQUIRES_CLASSES  =>
      RuleKind("requires-classes", NameForm.ClassNames, classes(Keep.RequiresClasses(_)), { case Keep.RequiresClasses(a) => a.map(_.value) })
    case KEEP_GENERATED_CLASSES =>
      RuleKind("keep-generated-classes", NameForm.SuffixNames, suffixes(Keep.GeneratedClasses(_)), { case Keep.GeneratedClasses(s) => s.map(_.value) })

  private val descriptions: Map[KeepRuleKind, RuleKind] = KeepRuleKind.values.toList.map(kind => kind -> build(kind)).toMap

  /** What the facade knows about `kind`. */
  def describe(kind: KeepRuleKind): RuleKind = descriptions.getOrElse(kind, build(kind))

  /** The directive of a rules file for each kind. */
  val directives: Map[String, KeepRuleKind] =
    descriptions.map((kind, description) => description.directive -> kind)

  /** The kind that reads `keep` back, with its names, if there is one. */
  def kindOf(keep: Keep): Option[(KeepRuleKind, Set[String])] =
    descriptions.iterator.flatMap((kind, description) => description.names.lift(keep).map(names => kind -> names)).nextOption()
