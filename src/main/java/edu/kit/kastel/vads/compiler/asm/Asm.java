package edu.kit.kastel.vads.compiler.asm;

import java.util.ArrayList;
import java.util.List;

import edu.kit.kastel.vads.compiler.asm.node.instruction.InstructionAsm;
import edu.kit.kastel.vads.compiler.asm.node.instruction.MovAsm;
import edu.kit.kastel.vads.compiler.asm.node.instruction.RetAsm;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Instruction;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Return;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Unary;
import edu.kit.kastel.vads.compiler.asm.node.instruction.UnaryAsm;
import edu.kit.kastel.vads.compiler.asm.node.instruction.UnaryOperator;
import edu.kit.kastel.vads.compiler.asm.node.operand.RegAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.RegType;
import edu.kit.kastel.vads.compiler.asm.reg_alloc.RegAlloc;
import edu.kit.kastel.vads.compiler.ir_tac.node.val.Constant;
import edu.kit.kastel.vads.compiler.ir_tac.node.val.Val;
import edu.kit.kastel.vads.compiler.ir_tac.node.val.Var;
import edu.kit.kastel.vads.compiler.lexer.Operator.OperatorType;
import edu.kit.kastel.vads.compiler.asm.node.operand.ImmAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.OperandAsm;
import edu.kit.kastel.vads.compiler.asm.node.operand.PseudoAsm;

public class Asm {
    private final List<InstructionAsm> asmInstructions;

    public Asm(List<Instruction> irInstructions) {
        this.asmInstructions = new ArrayList<InstructionAsm>();
        for (Instruction irInstruction : irInstructions) {
            this.asmInstructions.addAll(generate(irInstruction));
        }
        new RegAlloc(this.asmInstructions).allocate();
    }

    private List<InstructionAsm> generate(Instruction irInstruction) {
        switch (irInstruction) {
            case Return irReturn -> {
                return generateReturn(irReturn);
            }
            case Unary irUnary -> {
                return generateUnary(irUnary);
            }
            default -> throw new IllegalArgumentException("Unknown instruction type: " + irInstruction.getClass().getName());
        }
    }

    private List<InstructionAsm> generateReturn(Return irReturn) {
        OperandAsm src = generateOperand(irReturn.value());
        return List.of(
            new MovAsm(src, new RegAsm(RegType.AX)),
            new RetAsm()
        );
    }

    private List<InstructionAsm> generateUnary(Unary irUnary) {
        OperandAsm src = generateOperand(irUnary.src());
        UnaryOperator operator = generateUnaryOperator(irUnary.operator());
        OperandAsm dst = generateOperand(irUnary.dst());

        return List.of(
            new MovAsm(src, dst),
            new UnaryAsm(operator, dst)
        );
    }

    private UnaryOperator generateUnaryOperator(OperatorType irOperator) {
        switch (irOperator) {
            case NEGATE -> {
                return UnaryOperator.NEG;
            }
            case COMPLEMENT -> {
                return UnaryOperator.NOT;
            }
            default -> throw new IllegalArgumentException("Unknown unary operator: " + irOperator.getClass().getName());
        }
    }

    private OperandAsm generateOperand(Val irValue) {
        switch (irValue) {
            case Constant irConstant -> {
                return new ImmAsm(irConstant.value());
            }
            case Var irVar -> {
                return new PseudoAsm(irVar.identifier());
            }
            default -> throw new IllegalArgumentException("Unknown operand type: " + irValue.getClass().getName());
        }
    }

    @Override
    public String toString() {
        StringBuilder asm = new StringBuilder();
        for (InstructionAsm instruction : asmInstructions) {
            asm.append(instruction.toString());
            asm.append("\n");
        }
        return asm.toString();
    }
}
