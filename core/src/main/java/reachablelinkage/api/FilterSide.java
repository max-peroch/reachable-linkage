package reachablelinkage.api;

/** Which end of a conflict a package filter looks at. */
public enum FilterSide {
  /** The class that holds the broken reference (`fromClass`). */
  SOURCE,
  /** The class the reference points at (`targetClass`). */
  DESTINATION;
}
