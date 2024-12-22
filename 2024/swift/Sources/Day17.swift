import Foundation
import Parsing

precedencegroup Exponentiative {
    associativity: left
    higherThan: MultiplicationPrecedence
}

infix operator **: Exponentiative

public func ** <N: BinaryInteger>(base: N, power: N) -> N {
    return N.self(pow(Double(base), Double(power)))
}

enum Instruction: Int {
    case adv = 0, bxl, bst, jnz, bxc, out, bdv, cdv
}

enum ComboOperand {
    case literal, registerA, registerB, registerC, reserved
}

extension UInt8 {
    func comboOperand() -> ComboOperand {
        return switch self {
        case 0 ... 3: .literal
        case 4: .registerA
        case 5: .registerB
        case 6: .registerC
        case 7: .reserved
        default:
            preconditionFailure("Invalid operand")
        }
    }

    func toInt() -> Int {
        return Int(exactly: self)!
    }
}

enum ExecutionResult {
    case out(Int), jmp(Int), write(register: UInt8, value: Int), nop
}

struct Day17: AdventDay {
    var data: String

    struct Computer {
        var registers: (A: Int, B: Int, C: Int)
        let program: [UInt8]
        var instructionPtr: Int = 0

        let JMP_OCCURRED: Int8 = -1

        func getComboOperandValue(_ operand: UInt8) -> Int {
            return switch operand.comboOperand() {
            case .literal: operand.toInt()
            case .registerA: registers.A
            case .registerB: registers.B
            case .registerC: registers.C
            case .reserved:
                preconditionFailure("Invalid operand detected.")
            }
        }

        mutating func writeToRegister(_ register: UInt8?, _ value: Int) {
            precondition(register != nil, "Register can't be nil.")
            switch register! {
            case 0: registers.A = value
            case 1: registers.B = value
            case 2: registers.C = value
            default:
                preconditionFailure("Invalid register index detected.")
            }
        }

        mutating func execute(_ instruction: Instruction, _ operand: UInt8) -> ExecutionResult {
            var registerToWrite: UInt8? = nil
            switch instruction {
            case .adv:
                registerToWrite = registerToWrite ?? 0
                fallthrough
            case .bdv:
                registerToWrite = registerToWrite ?? 1
                fallthrough
            case .cdv:
                registerToWrite = registerToWrite ?? 2
                return .write(register: registerToWrite!, value: registers.A >> getComboOperandValue(operand))
            case .bxl:
                return .write(register: 1, value: registers.B ^ operand.toInt())
            case .bxc:
                return .write(register: 1, value: registers.B ^ registers.C)
            case .bst:
                return .write(register: 1, value: getComboOperandValue(operand) % 8)
            case .jnz:
                if registers.A != 0 {
                    return .jmp(operand.toInt())
                }
                return .nop
            case .out:
                return .out(getComboOperandValue(operand) % 8)
            }
        }

        mutating func iterate() {
            instructionPtr = instructionPtr + 2
        }

        mutating func run(untilFirstOut: Bool = false) -> [Int] {
            var outputs: [Int] = []
            execLoop: while instructionPtr < program.count {
                let instruction = program[instructionPtr], operand = program[instructionPtr + 1]

                switch execute(Instruction(rawValue: instruction.toInt())!, operand) {
                case .out(let value):
                    outputs.append(value)
                    if untilFirstOut { break execLoop } else { iterate() }
                case .write(let register, let value): writeToRegister(register, value); iterate()
                case .jmp(let instruction): instructionPtr = instruction
                case .nop: iterate()
                }
            }
            return outputs
        }
    }

    var entities: Computer {
        do {
            return try Parse { Computer(registers: ($0, $1, $2), program: $3) } with: {
                "Register A: "; Int.parser(); "\n"
                "Register B: "; Int.parser(); "\n"
                "Register C: "; Int.parser(); "\n\n"
                "Program: "; Many {
                    UInt8.parser()
                } separator: {
                    ","
                } terminator: {
                    End()
                }
            }.parse(data.trimmingCharacters(in: .whitespacesAndNewlines))
        } catch {
            fatalError("Unable to parse data: \(error)")
        }
    }

    func part1() -> Any {
        var computer = entities
        return computer.run().map(String.init).joined(separator: ",")
    }

    func part2Inner(program: [UInt8], index: Int, a: Int) -> Int {
        /// Explanation:
        /// - Every input for this day ends in 3,0 which means: jump to the beginning.
        /// - The program will only halt when Register A is 0 (jump gets ignored)
        /// - Register A only gets manipulated using the ADV instruction
        /// - In every input for this day, ADV is only ran once (in a random position) and it's always 0,3
        /// - Meaning, Register A is always // 2^3 which is ==  >> 3
        /// - Another thing is that in every input for this day, there is only 1 output per "iteration" (change in the value of register A)
        ///   based on the value of A at the time (programs output the value of register B or C, which gets set through BDV / CDV based on A).
        ///   Whenever the value of Register A is used in this way, it will pass through % 8 at some point in the computation graph, so the only values
        ///   that can matter are 0-7 added to whatever A is at the time
        ///
        /// In short, the program simply iterates, bit-shifting Register A by 3 bits each iteration, and each iteration it also outputs a number
        /// that relates to A's exact value at that iteration, and only the last 3 bits in this exact value actually matter.
        ///
        /// This results in the following backward induction algorithm:
        /// - Start from the end. We know A is gonna be 0 at the end else the program wouldn't halt.
        /// - Left bit shift A to regain the value of A at that iteration. Then loop through all possible last 3 bits of A at that iteration
        /// - For each possible value of those last 3 bits, compute the output of the program that iteration. If this matches the desired output (the program itself)
        ///   then this is a promising value for A for that iteration. In a depth-first manner, go back another iteration and run the procedure again. If it matches again
        ///   keep going... until we run out of output digits to check. If we did, then the value of A that got us there has to be the right one. Depth first searching the
        ///   3 bits in ascending order will get us the minimum solution so we can just exit.
        ///   If we didn't actually find any possible value of a that iteration that matches the output of the program for that iteration, then the value of A we have has to be wrong, and we can simply go back to testing the next value and so on.
        if index == -1 {
            return a // We've found a match for the entire input
        }

        for i in 0 ..< 8 { // Every possible next value of the A register
            let nextA = (a << 3) | i
            var computer = Computer(registers: (nextA, 0, 0), program: program)
            let out = computer.run(untilFirstOut: true)[0]

            if out == program[index] {
                // go next (recursively)
                let ret = part2Inner(program: program, index: index - 1, a: nextA)
                if ret != -1 { // we've found an actual value
                    return ret
                }
            }
        }

        return -1 // Failed to find a match for the current program idx
    }

    func part2() -> Any {
        return part2Inner(program: entities.program, index: entities.program.count - 1, a: 0)
    }
}
