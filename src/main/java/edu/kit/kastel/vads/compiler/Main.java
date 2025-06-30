package edu.kit.kastel.vads.compiler;

import edu.kit.kastel.vads.compiler.asm.Asm;
import edu.kit.kastel.vads.compiler.asm.node.build_in_funcs.PrintAsm;
import edu.kit.kastel.vads.compiler.backend.aasm.CodeGenerator;
import edu.kit.kastel.vads.compiler.backend.regalloc.GraphColoringRegisterAllocator;
import edu.kit.kastel.vads.compiler.backend.regalloc.AasmToAttAsmTranslator;
import edu.kit.kastel.vads.compiler.ir.IrGraph;
import edu.kit.kastel.vads.compiler.ir.SsaTranslation;
import edu.kit.kastel.vads.compiler.ir.optimize.LocalValueNumbering;
import edu.kit.kastel.vads.compiler.ir.util.YCompPrinter;
import edu.kit.kastel.vads.compiler.ir_tac.IrTac;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Instruction;
import edu.kit.kastel.vads.compiler.ir.util.GraphVizPrinter;
import edu.kit.kastel.vads.compiler.lexer.Lexer;
import edu.kit.kastel.vads.compiler.parser.ParseException;
import edu.kit.kastel.vads.compiler.parser.Parser;
import edu.kit.kastel.vads.compiler.parser.TokenSource;
import edu.kit.kastel.vads.compiler.parser.ast.FunctionTree;
import edu.kit.kastel.vads.compiler.parser.ast.ProgramTree;
import edu.kit.kastel.vads.compiler.semantic.SemanticAnalysis;
import edu.kit.kastel.vads.compiler.semantic.SemanticException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Main {
    public static void main(String[] args) throws IOException {
        if (args.length != 2) {
            System.err.println("Invalid arguments: Expected one input file and one output file");
            System.exit(3);
        }
        Path input = Path.of(args[0]);
        Path output = Path.of(args[1]);
        ProgramTree program = lexAndParse(input);
        try {
            new SemanticAnalysis(program).analyze();
        } catch (SemanticException e) {
            e.printStackTrace();
            System.exit(7);
            return;
        }
       

        IrTac ir = new IrTac(program);
        String asm = "";
        for (List<Instruction> instructionsPerFunction : ir.instructionMatrix()) {
            Asm asmAst = new Asm(instructionsPerFunction);
            asm += asmAst.toString();
        }
        asm += new PrintAsm().toString();
        System.out.println(asm);

        String starter =    """
        .global main
        .text
        main:
        call _main
        movq %rax, %rdi
        movq $0x3C, %rax
        syscall
        """;



        // List<IrGraph> graphs = new ArrayList<>();
        // for (FunctionTree function : program.topLevelTrees()) {
        //     SsaTranslation translation = new SsaTranslation(function, new LocalValueNumbering());
        //     graphs.add(translation.translate());
        // }

        // if ("vcg".equals(System.getenv("DUMP_GRAPHS")) || "vcg".equals(System.getProperty("dumpGraphs"))) {
        //     Path tmp = output.toAbsolutePath().resolveSibling("graphs");
        //     Files.createDirectories(tmp);
        //     for (IrGraph graph : graphs) {
        //         dumpGraph(graph, tmp, "before-codegen", "vcg");
        //     }
        // }

        // if ("dot".equals(System.getenv("DUMP_GRAPHS")) || "dot".equals(System.getProperty("dumpGraphs"))) {
        //     Path tmp = output.toAbsolutePath().resolveSibling("graphs");
        //     Files.createDirectories(tmp);
        //     for (IrGraph graph : graphs) {
        //         dumpGraph(graph, tmp, "before-codegen", "dot");
        //     }
        // }

        // TODO: generate assembly and invoke gcc instead of generating abstract assembly
        // String aasm = new CodeGenerator().generateCode(graphs);
        // List<String> aasmLines = Arrays.asList(aasm.split("\n"));
        // GraphColoringRegisterAllocator allocator = new GraphColoringRegisterAllocator();
        // java.util.Map<String, String> regMap = allocator.allocate(aasmLines);
        // AasmToAttAsmTranslator translator = new AasmToAttAsmTranslator();
        // String attAsm = translator.translate(aasmLines, regMap);
        // 写入临时汇编文件
        Path asmFile = output.resolveSibling(output.getFileName().toString() + ".s");
        Files.writeString(asmFile, starter + asm);
        // 调用gcc编译汇编为可执行文件
        try {
            Process gcc = new ProcessBuilder(
                "gcc", "-no-pie", "-o", output.toString(), asmFile.toString()
            ).inheritIO().start();
            int exitCode = gcc.waitFor();
            if (exitCode != 0) {
                System.err.println("gcc 编译失败，退出码: " + exitCode);
                System.exit(8);
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new IOException("gcc 调用被中断", e);
        }
        // 可选：删除临时汇编文件
        // Files.deleteIfExists(asmFile);
    }

    private static ProgramTree lexAndParse(Path input) throws IOException {
        try {
            Lexer lexer = Lexer.forString(Files.readString(input));
            TokenSource tokenSource = new TokenSource(lexer);
            Parser parser = new Parser(tokenSource);
            return parser.parseProgram();
        } catch (ParseException e) {
            e.printStackTrace();
            System.exit(42);
            throw new AssertionError("unreachable");
        }
    }

    private static void dumpGraph(IrGraph graph, Path path, String key, String format) throws IOException {
        if (format.equals("dot")) {
            Files.writeString(  
                path.resolve(graph.name() + "-" + key + ".dot"),
                GraphVizPrinter.print(graph)
            );
        } 
        if (format.equals("vcg")) {
            Files.writeString(
                path.resolve(graph.name() + "-" + key + ".vcg"),
                YCompPrinter.print(graph)
            );
        }
    }
}
