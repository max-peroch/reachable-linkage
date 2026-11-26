package rl;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

/** What the agent records: one flag per instrumented method, and its name, written when the JVM exits. */
public final class Cov {
  private static final int CAPACITY = 1 << 23;
  private static final boolean[] HITS = new boolean[CAPACITY];
  private static final ArrayList<String> NAMES = new ArrayList<>();

  private Cov() { }

  /** A method that will be instrumented: its id, or -1 if the table is full. */
  public static synchronized int register(String owner, String name, String desc) {
    if (NAMES.size() >= CAPACITY) return -1;
    NAMES.add(owner + "." + name + desc);
    return NAMES.size() - 1;
  }

  /** Called on entry to every instrumented method. */
  public static void hit(int id) { HITS[id] = true; }

  static synchronized void dump(String path) {
    try (PrintWriter out = new PrintWriter(path, StandardCharsets.UTF_8)) {
      for (int i = 0; i < NAMES.size(); i++) if (HITS[i]) out.println(NAMES.get(i));
    } catch (IOException e) {
      System.err.println("coverage agent: cannot write " + path + ": " + e);
    }
  }
}
