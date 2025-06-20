package edu.kit.kastel.vads.compiler.backend.regalloc;

import java.util.*;

/**
 * 将aasm指令翻译为AT&T格式x86-64汇编。
 */
public class AasmToAttAsmTranslator {
    /**
     * @param aasmLines aasm指令列表
     * @param regMap 虚拟寄存器到物理寄存器的映射
     * @return AT&T汇编代码字符串
     */
    public String translate(List<String> aasmLines, Map<String, String> regMap) {
        StringBuilder sb = new StringBuilder();
        // 添加模板头部
        sb.append(".global main\n");
        sb.append(".global _main\n");
        sb.append(".text\n");
        sb.append("main:\n");
        sb.append("    call _main\n");
        sb.append("    movq %rax, %rdi\n");
        sb.append("    movq $0x3C, %rax\n");
        sb.append("    syscall\n");
        sb.append("_main:\n");

        // Allocate stack space for spilling
        if (GraphColoringRegisterAllocator.stackOffset > 0) {
            sb.append("    subq $").append(GraphColoringRegisterAllocator.stackOffset).append(", %rsp\n");
        }

        // 生成原有的汇编代码
        for (String line : aasmLines) {
            String asm = translateLine(line.trim(), regMap);
            if (asm != null && !asm.isEmpty()) {
                sb.append(asm).append("\n");
            }
        }
        return sb.toString();
    }

    private String translateLine(String line, Map<String, String> regMap) {
        if (line.isEmpty()) return "";
        // 处理函数头和尾
        if (line.startsWith("function ")) {
            if (line.startsWith("function main")) {
                return null;
            } else {
                System.exit(42);
                throw new AssertionError("No main function");
            }
        }
        if (line.equals("}")) return "";
        // 处理aasm指令
        if (line.startsWith("ret ")) {
            String[] parts = line.split(" ");
            String src = regMap.getOrDefault(parts[1], parts[1]);
            
            String mov = String.format("    movl %s, %%eax", src);
            String deallocStack = GraphColoringRegisterAllocator.stackOffset > 0 ? String.format("    addq $%d, %%rsp", GraphColoringRegisterAllocator.stackOffset) : "";
            String ret = String.format("    ret");
            return String.format("%s\n%s\n%s", mov, deallocStack, ret);
        }
        if (line.contains(" = const ")) {
            String[] parts = line.split(" = const ");
            String dst = regMap.getOrDefault(parts[0].trim(), parts[0].trim());
            String imm = parts[1].trim();
            return String.format("    movl $%s, %s", imm, dst);
        }
        if (line.contains(" = add ")) {
            // dst = src1 + src2
            String[] parts = line.split(" = add ");
            String dst = regMap.getOrDefault(parts[0].trim(), parts[0].trim());
            String[] srcs = parts[1].trim().split(" ");
            String src1 = regMap.getOrDefault(srcs[0], srcs[0]);
            String src2 = regMap.getOrDefault(srcs[1], srcs[1]);
            return String.format("    movl %s, %%eax\n    addl %s, %%eax\n    movl %%eax, %s", src1, src2, dst);
        }
        if (line.contains(" = sub ")) {
            // dst = src1 - src2
            String[] parts = line.split(" = sub ");
            String dst = regMap.getOrDefault(parts[0].trim(), parts[0].trim());
            String[] srcs = parts[1].trim().split(" ");
            String src1 = regMap.getOrDefault(srcs[0], srcs[0]);
            String src2 = regMap.getOrDefault(srcs[1], srcs[1]);
            return String.format("    movl %s, %%eax\n    subl %s, %%eax\n    movl %%eax, %s", src1, src2, dst);
        }
        if (line.contains(" = mul ")) {
            // dst = src1 * src2
            String[] parts = line.split(" = mul ");
            String dst = regMap.getOrDefault(parts[0].trim(), parts[0].trim());
            String[] srcs = parts[1].trim().split(" ");
            String src1 = regMap.getOrDefault(srcs[0], srcs[0]);
            String src2 = regMap.getOrDefault(srcs[1], srcs[1]);
            return String.format("    movl %s, %%eax\n    imull %s, %%eax\n    movl %%eax, %s", src1, src2, dst);
        }
        if (line.contains(" = div ")) {
            // dst = dividend / divisor
            // ==> %eax = %eax / x
            String[] parts = line.split(" = div ");
            String dst = regMap.getOrDefault(parts[0].trim(), parts[0].trim());
            String[] srcs = parts[1].trim().split(" ");
            String dividend = regMap.getOrDefault(srcs[0], srcs[0]);
            String divisor = regMap.getOrDefault(srcs[1], srcs[1]);
            return String.format("    movl %s, %%eax\n    cltd\n    idivl %s\n    movl %%eax, %s", dividend, divisor, dst);
        }
        if (line.contains(" = mod ")) {
            // dst = dividend % divisor
            // ==> %edx = %eax % x
            String[] parts = line.split(" = mod ");
            String dst = regMap.getOrDefault(parts[0].trim(), parts[0].trim());
            String[] srcs = parts[1].trim().split(" ");
            String dividend = regMap.getOrDefault(srcs[0], srcs[0]);
            String divisor = regMap.getOrDefault(srcs[1], srcs[1]);
            return String.format("    movl %s, %%eax\n    cltd\n    idivl %s\n    movl %%edx, %s", dividend, divisor, dst);
        }
        if (line.contains(" = not ")) {
            // dst = !src
            String[] parts = line.split(" = not ");
            String dst = regMap.getOrDefault(parts[0].trim(), parts[0].trim());
            String src = regMap.getOrDefault(parts[1].trim(), parts[1].trim());
            return String.format("    movl %s, %%eax\n    cmpl $0, %%eax\n    sete %%al\n    movzbl %%al, %%eax\n    movl %%eax, %s", src, dst);
        }
        if (line.contains(" = bitnot ")) {
            // dst = ~src
            String[] parts = line.split(" = bitnot ");
            String dst = regMap.getOrDefault(parts[0].trim(), parts[0].trim());
            String src = regMap.getOrDefault(parts[1].trim(), parts[1].trim());
            return String.format("    movl %s, %%eax\n    notl %%eax\n    movl %%eax, %s", src, dst);
        }
        if (line.startsWith("  if ")) {
            // if condition then [else]
            String[] parts = line.trim().split(" if ");
            String[] conditionParts = parts[1].split(" then");
            String condition = regMap.getOrDefault(conditionParts[0].trim(), conditionParts[0].trim());
            String label = "L" + System.identityHashCode(line);
            if (conditionParts[1].contains("else")) {
                return String.format("    cmpl $0, %s\n    je %s_else\n%s_then:\n    # then block\n    jmp %s_end\n%s_else:\n    # else block\n%s_end:", 
                    condition, label, label, label, label, label);
            } else {
                return String.format("    cmpl $0, %s\n    je %s_end\n%s_then:\n    # then block\n%s_end:", 
                    condition, label, label, label);
            }
        }
        if (line.startsWith("  while ")) {
            // while condition
            String[] parts = line.trim().split(" while ");
            String condition = regMap.getOrDefault(parts[1].trim(), parts[1].trim());
            String label = "L" + System.identityHashCode(line);
            return String.format("%s_start:\n    cmpl $0, %s\n    je %s_end\n    # while body\n    jmp %s_start\n%s_end:", 
                label, condition, label, label, label);
        }
        if (line.startsWith("  for ")) {
            // for condition
            String[] parts = line.trim().split(" for ");
            String condition = regMap.getOrDefault(parts[1].trim(), parts[1].trim());
            String label = "L" + System.identityHashCode(line);
            return String.format("%s_start:\n    cmpl $0, %s\n    je %s_end\n    # for body\n    jmp %s_start\n%s_end:", 
                label, condition, label, label, label);
        }
        // 其它情况直接忽略                             
        return null;
    }
} 