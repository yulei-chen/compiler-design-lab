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
            // 统一替换为_main标签
            return null;
        }
        if (line.equals("}")) return "";
        // 处理aasm指令
        if (line.startsWith("ret ")) {
            String[] parts = line.split(" ");
            String src = regMap.getOrDefault(parts[1], parts[1]);
            return String.format("    movq %s, %%rax\n    ret", src);
        }
        if (line.contains(" = const ")) {
            String[] parts = line.split(" = const ");
            String dst = regMap.getOrDefault(parts[0].trim(), parts[0].trim());
            String imm = parts[1].trim();
            return String.format("    movq $%s, %s", imm, dst);
        }
        if (line.contains(" = add ")) {
            // dst = src1 + src2
            String[] parts = line.split(" = add ");
            String dst = regMap.getOrDefault(parts[0].trim(), parts[0].trim());
            String[] srcs = parts[1].trim().split(" ");
            String src1 = regMap.getOrDefault(srcs[0], srcs[0]);
            String src2 = regMap.getOrDefault(srcs[1], srcs[1]);
            // movq src1 dst may cover src2 if dst == src2
            if (dst.equals(src2)) {
                return String.format("    addq %s, %s", src1, dst);
            }
            return String.format("    movq %s, %s\n    addq %s, %s", src1, dst, src2, dst);
        }
        if (line.contains(" = sub ")) {
            // dst = src1 - src2
            String[] parts = line.split(" = sub ");
            String dst = regMap.getOrDefault(parts[0].trim(), parts[0].trim());
            String[] srcs = parts[1].trim().split(" ");
            String src1 = regMap.getOrDefault(srcs[0], srcs[0]);
            String src2 = regMap.getOrDefault(srcs[1], srcs[1]);
            // movq src1 dst may cover src2 if dst == src2
            if (dst.equals(src2)) {
                // subq src1 dst; negq dst
                return String.format("    subq %s, %s\n    negq %s", src1, dst, dst);
            }
            // movq src1 dst; subq src2 dst
            return String.format("    movq %s, %s\n    subq %s, %s", src1, dst, src2, dst);
        }
        if (line.contains(" = mul ")) {
            // dst = src1 * src2
            String[] parts = line.split(" = mul ");
            String dst = regMap.getOrDefault(parts[0].trim(), parts[0].trim());
            String[] srcs = parts[1].trim().split(" ");
            String src1 = regMap.getOrDefault(srcs[0], srcs[0]);
            String src2 = regMap.getOrDefault(srcs[1], srcs[1]);
            if (dst.equals(src2)) {
                return String.format("    imulq %s, %s", src1, dst);
            }
            // movq src1 dst; imulq src2 dst
            return String.format("    movq %s, %s\n    imulq %s, %s", src1, dst, src2, dst);
        }
        if (line.contains(" = div ")) {
            // dst = dividend / divisor
            // ==> %rax = %rax / x
            String[] parts = line.split(" = div ");
            String dst = regMap.getOrDefault(parts[0].trim(), parts[0].trim());
            String[] srcs = parts[1].trim().split(" ");
            String dividend = regMap.getOrDefault(srcs[0], srcs[0]);
            String divisor = regMap.getOrDefault(srcs[1], srcs[1]);
            // mov dividend, %rax; cqto; idiv divisor; mov %rax, dst
            if (dst.equals("%rax")) {
                return String.format("    movq %s, %%rax\n    cqto\n    idivq %s", dividend, divisor);
            }
            return String.format("    pushq %%rax\n    movq %s, %%rax\n    cqto\n    idivq %s\n    movq %%rax, %s\n    popq %%rax", dividend, divisor, dst);
        }
        if (line.contains(" = mod ")) {
            // dst = dividend % divisor
            // ==> %rdx = %rax % x
            String[] parts = line.split(" = mod ");
            String dst = regMap.getOrDefault(parts[0].trim(), parts[0].trim());
            String[] srcs = parts[1].trim().split(" ");
            String dividend = regMap.getOrDefault(srcs[0], srcs[0]);
            String divisor = regMap.getOrDefault(srcs[1], srcs[1]);
            // mov dividend, %rax; cqto; idiv divisor; mov %rdx, dst
            if (dst.equals("%rdx")) {
                return String.format("    pushq %%rax\n    movq %s, %%rax\n    cqto\n    idivq %s\n  popq %%rax", dividend, divisor);
            }
            return String.format("    pushq %%rax\n    pushq %%rdx\n    movq %s, %%rax\n    cqto\n    idivq %s\n    movq %%rdx, %s\n    popq %%rdx\n    popq %%rax", dividend, divisor, dst);
        }
        // 其它情况直接忽略                             
        return null;
    }
} 