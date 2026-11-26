package rl;

import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.Instrumentation;
import java.security.ProtectionDomain;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * A java agent for measuring, not part of the product: every method of every class the application loads records that it ran, and the names
 * of the ones that did are written to the file given as the agent argument when the JVM exits. Classes of the JDK and of this agent are left alone.
 *
 * Usage: {@code java -javaagent:coverage-agent.jar=/path/to/executed.txt -cp ... Main}
 */
public final class CoverageAgent {
  private CoverageAgent() { }

  public static void premain(String path, Instrumentation inst) {
    inst.addTransformer(new Transformer(), false);
    Runtime.getRuntime().addShutdownHook(new Thread(() -> Cov.dump(path)));
  }

  private static final class Transformer implements ClassFileTransformer {
    @Override
    public byte[] transform(ClassLoader loader, String name, Class<?> redefined, ProtectionDomain domain, byte[] bytes) {
      if (loader == null || name == null) return null;
      if (name.startsWith("java/") || name.startsWith("jdk/") || name.startsWith("sun/") || name.startsWith("rl/")
          || name.startsWith("org/objectweb/asm/")) return null;
      try {
        ClassReader reader = new ClassReader(bytes);
        ClassWriter writer = new ClassWriter(reader, ClassWriter.COMPUTE_MAXS);
        reader.accept(new ClassVisitor(Opcodes.ASM9, writer) {
          private String owner;

          @Override
          public void visit(int version, int access, String className, String signature, String superName, String[] interfaces) {
            owner = className;
            super.visit(version, access, className, signature, superName, interfaces);
          }

          @Override
          public MethodVisitor visitMethod(int access, String method, String desc, String signature, String[] exceptions) {
            MethodVisitor mv = super.visitMethod(access, method, desc, signature, exceptions);
            if (mv == null || (access & (Opcodes.ACC_ABSTRACT | Opcodes.ACC_NATIVE)) != 0) return mv;
            return new MethodVisitor(Opcodes.ASM9, mv) {
              @Override
              public void visitCode() {
                super.visitCode();
                int id = Cov.register(owner, method, desc);
                if (id >= 0) {
                  super.visitLdcInsn(id);
                  super.visitMethodInsn(Opcodes.INVOKESTATIC, "rl/Cov", "hit", "(I)V", false);
                }
              }
            };
          }
        }, 0);
        return writer.toByteArray();
      } catch (Throwable t) {
        return null;   // a class that cannot be instrumented runs as it is
      }
    }
  }
}
