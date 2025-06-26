package edu.kit.kastel.vads.compiler.asm;

import java.util.ArrayList;
import java.util.List;
import java.util.OptionalLong;

import edu.kit.kastel.vads.compiler.asm.node.instruction.BinaryAsm;
import edu.kit.kastel.vads.compiler.asm.node.instruction.BinaryOperator;
import edu.kit.kastel.vads.compiler.asm.node.instruction.CdqAsm;
import edu.kit.kastel.vads.compiler.asm.node.instruction.CmpAsm;
import edu.kit.kastel.vads.compiler.asm.node.instruction.CondCode;
import edu.kit.kastel.vads.compiler.asm.node.instruction.IdivAsm;
import edu.kit.kastel.vads.compiler.asm.node.instruction.InstructionAsm;
import edu.kit.kastel.vads.compiler.asm.node.instruction.JmpAsm;
import edu.kit.kastel.vads.compiler.asm.node.instruction.JmpCCAsm;
import edu.kit.kastel.vads.compiler.asm.node.instruction.LabelAsm;
import edu.kit.kastel.vads.compiler.asm.node.instruction.MovAsm;
import edu.kit.kastel.vads.compiler.asm.node.instruction.RetAsm;
import edu.kit.kastel.vads.compiler.asm.node.instruction.SetCCAsm;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Binary;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Copy;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Instruction;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Jump;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.JumpIfNotZero;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.JumpIfZero;
import edu.kit.kastel.vads.compiler.ir_tac.node.instruction.Label;
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
    private List<InstructionAsm> asmInstructions;

    public Asm(List<Instruction> irInstructions) {
        this.asmInstructions = new ArrayList<InstructionAsm>();
        for (Instruction irInstruction : irInstructions) {
            this.asmInstructions.addAll(generate(irInstruction));
        }
        // TODO: ir & asm don't know declarations....
        // So liveness should be done with syntax tree... OMG
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
            case Binary irBinary -> {
                return generateBinary(irBinary);
            }
            case Jump irJump -> {
                return generateJump(irJump);
            }
            case JumpIfZero irJumpIfZero -> {
                return generateJumpIfZero(irJumpIfZero);
            }
            case JumpIfNotZero irJumpIfNotZero -> {
                return generateJumpIfNotZero(irJumpIfNotZero);
            }
            case Copy irCopy -> {
                return generateCopy(irCopy);
            }
            case Label irLabel -> {
                return generateLabel(irLabel);
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
        OperandAsm dst = generateOperand(irUnary.dst());

        switch (irUnary.operator()) {
            case NOT -> {
                return List.of(
                    new CmpAsm(new ImmAsm(OptionalLong.of(0)), src),
                    // HACK: zero out*
                    new MovAsm(new ImmAsm(OptionalLong.of(0)), dst),
                    new SetCCAsm(CondCode.E, dst)
                );
            }
            default -> {
                UnaryOperator operator = generateUnaryOperator(irUnary.operator());
                return List.of(
                    new MovAsm(src, dst),
                    new UnaryAsm(operator, dst)
                );
            }
        }
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

    private List<InstructionAsm> generateBinary(Binary irBinary) {
        OperandAsm src1 = generateOperand(irBinary.src1());
        OperandAsm src2 = generateOperand(irBinary.src2());
        OperandAsm dst = generateOperand(irBinary.dst());
        OperatorType irOp = irBinary.operator();

        switch (irOp) {
            case DIV -> {
                return List.of(
                    new MovAsm(src1, new RegAsm(RegType.AX)),
                    new CdqAsm(),
                    // NOTE: imm operand is not supported by `idiv` instruction
                    new MovAsm(src2, new RegAsm(RegType.BX)),
                    new IdivAsm(new RegAsm(RegType.BX)),
                    new MovAsm(new RegAsm(RegType.AX), dst)
                );
            }
            case MOD -> {
                return List.of(
                    new MovAsm(src1, new RegAsm(RegType.AX)),
                    new CdqAsm(),
                    // NOTE: imm operand is not supported by `idiv` instruction
                    new MovAsm(src2, new RegAsm(RegType.BX)),
                    new IdivAsm(new RegAsm(RegType.BX)),
                    new MovAsm(new RegAsm(RegType.DX), dst)
                );
            }
            case EQUAL -> {
                return List.of(
                    // NOTE: src1 and src2 are swapped here
                    new CmpAsm(src2, src1),
                    // HACK: zero out*
                    new MovAsm(new ImmAsm(OptionalLong.of(0)), dst),
                    new SetCCAsm(CondCode.E, dst)
                );
            }
            case NOT_EQUAL -> {
                return List.of(
                    // NOTE: src1 and src2 are swapped here
                    new CmpAsm(src2, src1),
                    // HACK: zero out*
                    new MovAsm(new ImmAsm(OptionalLong.of(0)), dst),
                    new SetCCAsm(CondCode.NE, dst)
                );
            }
            case GREATER -> {
                return List.of(
                    // NOTE: src1 and src2 are swapped here
                    new CmpAsm(src2, src1),
                    // HACK: zero out*
                    new MovAsm(new ImmAsm(OptionalLong.of(0)), dst),
                    new SetCCAsm(CondCode.G, dst)
                );
            }
            case GREATER_EQUAL -> {
                return List.of(
                    // NOTE: src1 and src2 are swapped here
                    new CmpAsm(src2, src1),
                    // HACK: zero out*
                    new MovAsm(new ImmAsm(OptionalLong.of(0)), dst),
                    new SetCCAsm(CondCode.GE, dst)
                );
            }
            case LESS -> {
                return List.of(
                    new CmpAsm(src2, src1),
                    // HACK: zero out*
                    new MovAsm(new ImmAsm(OptionalLong.of(0)), dst),
                    new SetCCAsm(CondCode.L, dst)
                );
            }
            case LESS_EQUAL -> {
                return List.of(
                    // NOTE: src1 and src2 are swapped here
                    new CmpAsm(src2, src1),
                    // HACK: zero out*
                    new MovAsm(new ImmAsm(OptionalLong.of(0)), dst),
                    new SetCCAsm(CondCode.LE, dst)
                );
            }
            default -> {
                BinaryOperator operator = generateBinaryOperator(irBinary.operator());
                return List.of(
                    new MovAsm(src1, dst),
                    new BinaryAsm(operator, src2, dst)
                );
            }
        }
    }

    private BinaryOperator generateBinaryOperator(OperatorType irOperator) {
        switch (irOperator) {
            case PLUS -> {
                return BinaryOperator.ADD;
            }
            case NEGATE -> {
                return BinaryOperator.SUB;
            }
            case MUL -> {
                return BinaryOperator.MUL;
            }
            default -> throw new IllegalArgumentException("Unknown binary operator: " + irOperator.getClass().getName());
        }
    }

    private List<InstructionAsm> generateJump(Jump irJump) {
        return List.of(
            new JmpAsm(irJump.target())
        );
    }


    private List<InstructionAsm> generateJumpIfZero(JumpIfZero irJumpIfZero) {
        OperandAsm condition = generateOperand(irJumpIfZero.condition());
        String target = irJumpIfZero.target();
        return List.of(
            new CmpAsm(new ImmAsm(OptionalLong.of(0)), condition),
            new JmpCCAsm(CondCode.E, target)
        );
    }

    private List<InstructionAsm> generateJumpIfNotZero(JumpIfNotZero irJumpIfNotZero) {
        OperandAsm condition = generateOperand(irJumpIfNotZero.condition());
        String target = irJumpIfNotZero.target();
        return List.of(
            new CmpAsm(new ImmAsm(OptionalLong.of(0)), condition),
            new JmpCCAsm(CondCode.NE, target)
        );
    }

    private List<InstructionAsm> generateCopy(Copy irCopy) {
        OperandAsm src = generateOperand(irCopy.src());
        OperandAsm dst = generateOperand(irCopy.dst());
        return List.of(
            new MovAsm(src, dst)
        );
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

    private List<InstructionAsm> generateLabel(Label irLabel) {
        return List.of(
            new LabelAsm(irLabel.name())
        );
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


// *zero out: zero out dst before setting the condition code,
//            since it sets only the lowest byte of dst