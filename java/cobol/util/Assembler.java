package cobol.util;

import java.io.*;
import java.util.*;
import org.apache.bcel.*;
import org.apache.bcel.classfile.*;
import org.apache.bcel.generic.*;
import org.apache.regexp.RE;

public class Assembler implements Constants {
    String file_name;
    String class_name;
    ClassGen cg;
    MethodGen mg;
    ConstantPoolGen cp;
    InstructionList il;
    InstructionFactory fc;
    Hashtable labels;
    ArrayList branches;
    int pindex;
    int pthrough;
    int preturn;

    static JavaClass cobolClass;
    static Type typeField = new ObjectType("cobol.runtime.Field");
    static Type typeFieldAttr = new ObjectType("cobol.runtime.FieldAttr");
    static Type typeIntArray = new ArrayType(Type.INT, 1);
    static Type typeByteArray = new ArrayType(Type.BYTE, 1);
    static Type[] typeNewField =
	new Type[] { typeByteArray, Type.INT, Type.INT, typeFieldAttr };

    InstructionHandle addBranch(short opcode, String label) {
	BranchInstruction b = fc.createBranchInstruction(opcode, null);
	InstructionHandle h = il.append(b);
	branches.add(b);
	branches.add(label);
	return h;
    }

    void parseLine(String line) throws Exception {
	line = line.trim();

	// skip empty lines
	if (line.length() == 0)
	    return;

	// skip comments
	if (line.startsWith(";"))
	    return;

	// instructions
	String[] tokens = line.split("[ \t]");

	// .file FILENAME
	//  Specify the source file name.
	if (tokens[0].equals(".file")) {
	    file_name = tokens[1];
	}
	// .class CLASSNAME
	//   Specify the class name.
	else if (tokens[0].equals(".class")) {
	    class_name = tokens[1];
	    cg = new ClassGen(class_name, "java.lang.Object", file_name,
			      ACC_PUBLIC | ACC_SUPER, null);
	    cp = cg.getConstantPool();
	    fc = new InstructionFactory(cg);
	}
	// .static
	//   Start static data area.
	else if (tokens[0].equals(".static")) {
	    il = new InstructionList();
	    mg = new MethodGen(ACC_STATIC | ACC_FINAL, Type.VOID,
			       Type.NO_ARGS, new String[] {  }, "<clinit>",
			       class_name, il, cp);
	}
	// .method METHODNAME
	//   Start a method definition.
	else if (tokens[0].equals(".method")) {
	    il = new InstructionList();
	    mg = new MethodGen(ACC_PUBLIC | ACC_STATIC, Type.VOID,
			       new Type[] {new ArrayType(Type.STRING, 1)},
			       new String[] {"args"},
			       "main", class_name, il, cp);
	    labels = new Hashtable();
	    branches = new ArrayList();

	    LocalVariableGen lg;
	    // int pindex = 0;
	    lg = mg.addLocalVariable("pindex", Type.INT, null, null);
	    pindex = lg.getIndex();
	    il.append(new PUSH(cp, 0));
	    lg.setStart(il.append(new ISTORE(pindex)));
	    // int pthrough[] = new int[24];
	    lg = mg.addLocalVariable("pthrough", typeIntArray, null, null);
	    pthrough = lg.getIndex();
	    il.append(new PUSH(cp, 24));
	    il.append(fc.createNewArray(Type.INT, (short) 1));
	    lg.setStart(il.append(new ASTORE(pthrough)));
	    // int preturn[] = new int[24];
	    lg = mg.addLocalVariable("preturn", typeIntArray, null, null);
	    preturn = lg.getIndex();
	    il.append(new PUSH(cp, 24));
	    il.append(fc.createNewArray(Type.INT, (short) 1));
	    lg.setStart(il.append(new ASTORE(preturn)));
	    // pthrough[pindex] = -1;
	    il.append(new ALOAD(pthrough));
	    il.append(new ILOAD(pindex));
	    il.append(new PUSH(cp, -1));
	    il.append(InstructionConstants.IASTORE);
	}
	// .main
	//   Start the main method.
	else if (tokens[0].equals(".main")) {
	}
	else if (tokens[0].equals("nop")) {
	}
	else if (tokens[0].equals("base")) {
	    String name = tokens[1];
	    int size = Integer.parseInt(tokens[2]);
	    FieldGen f = new FieldGen(ACC_STATIC, typeByteArray, name, cp);
	    cg.addField(f.getField());
	    il.append(new PUSH(cp, size));
	    il.append(fc.createNewArray(Type.BYTE, (short) 1));
	    il.append(fc.createFieldAccess(class_name, name, typeByteArray,
					   PUTSTATIC));
	}
	else if (tokens[0].equals("apush")) {
	    il.append(InstructionConstants.ACONST_NULL);
	}
	else if (tokens[0].equals("bpush")) {
	    String name = tokens[1];
	    il.append(fc.createFieldAccess(class_name, name, typeByteArray,
					   GETSTATIC));
	}
	else if (tokens[0].equals("cpush")) {
	    String text = line.replaceAll("(^[^\"]*\")|\"", "");
	    il.append(fc.createNew("cobol.runtime.Field"));
	    il.append(InstructionConstants.DUP);
	    il.append(new PUSH(cp, text));
	    il.append(fc.createInvoke("cobol.runtime.Field", "<init>",
				      Type.VOID, new Type[] { Type.STRING },
				      INVOKESPECIAL));
	}
	else if (tokens[0].equals("ipush")) {
	    il.append(new PUSH(cp, Integer.parseInt(tokens[1])));
	}
	else if (tokens[0].equals("spush")) {
	    String text = line.replaceAll("(^[^\"]*\")|\"", "");
	    il.append(new PUSH(cp, text));
	}
	else if (tokens[0].equals("new")) {
	    il.append(fc.createNew("cobol.runtime.Field"));
	    il.append(InstructionConstants.DUP);
	}
	else if (tokens[0].equals("init")) {
	    il.append(fc.createInvoke("cobol.runtime.Field", "<init>",
				      Type.VOID, typeNewField, INVOKESPECIAL));
	}
	else if (tokens[0].equals("call")) {
	    String name = tokens[1];
	    Method[] methods = cobolClass.getMethods();
	    for (int i = 0; i < methods.length; i++)
		if (name.equals(methods[i].getName())) {
		    il.append(fc.createInvoke(cobolClass.getClassName(), name,
					      methods[i].getReturnType(),
					      methods[i].getArgumentTypes(),
					      INVOKESTATIC));
		    return;
		}
	    throw new Exception("Unknown method: " + name);
	}
	else if (tokens[0].equals("return")) {
	    il.append(InstructionConstants.RETURN);

	    /* resolve labels */
	    if (branches != null)
		for (int i = 0; i < branches.size(); i += 2) {
		    BranchInstruction b = (BranchInstruction) branches.get(i);
		    String l = (String) branches.get(i + 1);
		    InstructionHandle ih = (InstructionHandle) labels.get(l);
		    if (ih != null)
			b.setTarget(ih);
		    else
			throw new Exception("Undefined label: " + l);
		}

	    mg.setMaxStack();
	    mg.setMaxLocals();
	    cg.addMethod(mg.getMethod());
	    il.dispose();
	}
	else if (tokens[0].equals("perform-table")) {
	    int n = Integer.parseInt(tokens[1]);
	    int match[] = new int[n];
	    InstructionHandle target[] = new InstructionHandle[n];
	    for (int i = 0; i < n; i++) {
		match[i] = i;
		target[i] = (InstructionHandle) labels.get(".LP" + i);
		if (target[i] == null)
		    throw new Exception("Undefined label: .LP" + i);
	    }
	    Select table = new TABLESWITCH(match, target, null);
	    labels.put("perform_table", il.append(new ALOAD(preturn)));
	    il.append(new ILOAD(pindex));
	    il.append(InstructionConstants.IALOAD);
	    il.append(table);
	    table.setTarget(il.append(InstructionConstants.NOP));
	}
	// perform ID FROM UNTIL
	else if (tokens[0].equals("perform")) {
	    int id = Integer.parseInt(tokens[1]);
	    String from = tokens[2];
	    int until = Integer.parseInt(tokens[3]);
	    // pindex++;
	    il.append(new IINC(pindex, 1));
	    // pthrough[pindex] = UNTIL;
	    il.append(new ALOAD(pthrough));
	    il.append(new ILOAD(pindex));
	    il.append(new PUSH(cp, until));
	    il.append(InstructionConstants.IASTORE);
	    // preturn[pindex] = ID;
	    il.append(new ALOAD(preturn));
	    il.append(new ILOAD(pindex));
	    il.append(new PUSH(cp, id));
	    il.append(InstructionConstants.IASTORE);
	    // goto FROM;
	    addBranch(GOTO, from);
	    // .LP{ID}: pindex--;
	    labels.put(".LP" + id, il.append(new IINC(pindex, -1)));
	}
	else if (tokens[0].equals("perform-exit")) {
	    int lbl = Integer.parseInt(tokens[1]);
	    il.append(new ALOAD(pthrough));
	    il.append(new ILOAD(pindex));
	    il.append(InstructionConstants.IALOAD);
	    il.append(new PUSH(cp, lbl));
	    addBranch(IF_ICMPEQ, "perform_table");
	}
	else if (tokens[0].equals("ifeq")) {
	    addBranch(IFEQ, tokens[1]);
	}
	else if (tokens[0].equals("ifne")) {
	    addBranch(IFNE, tokens[1]);
	}
	else if (tokens[0].equals("iflt")) {
	    addBranch(IFLT, tokens[1]);
	}
	else if (tokens[0].equals("ifle")) {
	    addBranch(IFLE, tokens[1]);
	}
	else if (tokens[0].equals("ifgt")) {
	    addBranch(IFGT, tokens[1]);
	}
	else if (tokens[0].equals("ifge")) {
	    addBranch(IFGE, tokens[1]);
	}
	else if (tokens[0].equals("goto")) {
	    addBranch(GOTO, tokens[1]);
	}
	else if (tokens[0].endsWith(":")) {
	    labels.put(tokens[0].replaceFirst(":$", ""),
		       il.append(InstructionConstants.NOP));
	}
	else {
	    throw new Exception("Unknown opcode: " + tokens[0]);
	}
    }

    public void assemble(String input) throws Exception {
	String line;
	BufferedReader in = new BufferedReader(new FileReader(input));

	while ((line = in.readLine()) != null) {
	    parseLine(line);
	}

	cg.getJavaClass().dump(class_name + ".class");
    }

    static {
	try {
	    cobolClass = new ClassParser("cobol/runtime/COBOL.class").parse();
	} catch (Exception ex) {
	    ex.printStackTrace();
	    System.exit(1);
	}
    }

    public static void main(String[] args) {
	if (args.length == 0) {
	    System.err.println("no input file");
	    System.exit(1);
	}

	if (args.length > 1) {
	    System.err.println("too many input files");
	    System.exit(1);
	}

	try {
	    new Assembler().assemble(args[0]);
	} catch (Exception ex) {
	    ex.printStackTrace();
	}
    }
}
