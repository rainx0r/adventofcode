import Parsing

extension Bool {
    func toInt() -> UInt {
        self ? 1 : 0
    }
}

struct Day24: AdventDay {
    var data: String
    
    enum GateOperation: String {
        case AND, OR, XOR
        
        func evaluate(_ lhs: Bool, _ rhs: Bool) -> Bool {
            return switch self {
            case .AND:
                lhs && rhs
            case .OR:
                lhs || rhs
            case .XOR:
                lhs != rhs
            }
        }
    }
    
    struct Gate {
        let input: (Wire, Wire)
        let output: Wire
        let operation: GateOperation
    }
    
    struct Wire: Hashable {
        let name: String
        
        static func == (lhs: Wire, rhs: Wire) -> Bool {
            lhs.name == rhs.name
        }
    }
    
    struct Circuit {
        var wires: [Wire: Bool]
        var dependencies: [Wire: (w1: Wire, w2: Wire, gate: GateOperation)]
        
        mutating func resolve(_ wire: Wire) -> Bool {
            if wires[wire] != nil { return wires[wire]! }
            assert(dependencies[wire] != nil, "If the Wire doesn't have a value, it must have dependencies")
            let (w1, w2, gate) = dependencies[wire]!
            let value = gate.evaluate(resolve(w1), resolve(w2))
            wires[wire] = value
            return value
        }
        
        mutating func run() -> UInt {
            for wire in dependencies.keys {
                var _ = resolve(wire)
            }
            
            var res: UInt = 0
            
            for wire in wires.keys.sorted(by: { $0.name < $1.name }) {
                if wire.name.hasPrefix("z") {
                    var wireName = wire.name
                    wireName.trimPrefix("z")
                    let wireId = UInt(wireName)!
                    res = res | (wires[wire]!.toInt() << wireId)
                }
            }
            
            return res
        }
        
        func findDependant(_ wire: Wire, gate: GateOperation) -> (Wire, (w1: Wire, w2: Wire, gate: GateOperation))? {
            for (dependant, dependency) in dependencies {
                if dependency.w1 == wire || dependency.w2 == wire {
                    if dependency.gate == gate { return (dependant, dependency) }
                }
            }
            return nil
        }
        
        func gateInvalid(w1: Wire, gate: GateOperation, w2: Wire, out: Wire) -> Bool {
            // The intuition is that a Ripple Carry Adder is simply a chain of 1-bit adders:
            // z1 = (x1 XOR y1) XOR c1
            // c2 = (x1 AND y1) OR ((x1 XOR y1) AND c1)
            // We also know that as input wires we've got x__ and y__ and the adder output wires will be z__
            // We can use those facts to construct rules that can weed out invalid "primitive" gates in the circuit
            // https://www.reddit.com/r/adventofcode/comments/1hla5ql/comment/m3kws15

            // Namely:
            // 1. If a gate ouputs into z, then it needs to be XOR (output of gate where second operand is the carry). Unless it's the final bit, but in that case it should also be an XOR between x and y
            // 2. If a gate doesn't output in z, and it also doesn't have inputs x and y, then it's gotta be AND / OR
            // 3. If a gate has inputs in x and y, then there's gotta be another gate with this one's output as input
            // 4. We can observe that all AND gates in the circuit feed into OR gates. Therefore all AND gates must output into an OR gate.

            // rule 1
            var ret = false
            
            func xyInputs(_ w1: Wire, _ w2: Wire) -> Bool {
                func isXOrY(_ w: Wire) -> Bool {
                    var wireID = w.name
                    let hasPrefix = wireID.hasPrefix("x") || wireID.hasPrefix("y")
                    wireID.trimPrefix("x")
                    wireID.trimPrefix("y")
                    // First gate plays by different rules to the rest of the circuit
                    return hasPrefix && Int(wireID)! != 0
                }
                return isXOrY(w1) && isXOrY(w2) && w1.name.first != w2.name.first
            }
            
            if out.name.hasPrefix("z") {
                var wireId = out.name
                wireId.trimPrefix("z")
                // Last gate plays by different rules to the rest of the circuit
                ret = ret || gate != GateOperation.XOR && Int(wireId)! != 45
            }
            
            // rule 2
            if !out.name.hasPrefix("z") && !xyInputs(w1, w2) {
                ret = ret || (gate == GateOperation.XOR)
            }

            // rule 3
            if xyInputs(w1, w2) && gate == GateOperation.XOR {
                ret = ret || findDependant(out, gate: GateOperation.XOR) == nil
            }
            
            // rule 4
            if xyInputs(w1, w2) && gate == GateOperation.AND {
                ret = ret || findDependant(out, gate: GateOperation.OR) == nil
            }
            
            return ret
        }
        
        func findFaults() -> Set<Wire> {
            var ret: Set<Wire> = []
            for (wire, dependency) in dependencies {
                if gateInvalid(w1: dependency.w1, gate: dependency.gate, w2: dependency.w2, out: wire) {
                    ret.insert(wire)
                }
            }
            return ret
        }
    }
    
    struct WireParser: Parser {
        var body: some Parser<Substring, (Wire, Int)> {
            PrefixUpTo(":").map { Wire(name: String($0)) }; ": "; Int.parser()
        }
    }
    
    struct DependencyParser: Parser {
        var body: some Parser<Substring, (Wire, GateOperation, Wire, Wire)> {
            PrefixUpTo(" ").map { Wire(name: String($0)) }; " "
            PrefixUpTo(" ").map { GateOperation(rawValue: String($0))! }; " "
            PrefixUpTo(" -> ").map { Wire(name: String($0)) }; " -> "
            Prefix { $0 != "\n" }.map { Wire(name: String($0)) }
        }
    }

    var entities: Circuit {
        do {
            let inputParts = data.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: "\n\n")
            let wires = try Many { WireParser() } separator: { "\n" } terminator: { End() }.parse(inputParts[0])
            let circuitWires = wires.reduce(into: [:]) { result, value in
                result[value.0] = value.1 == 1
            }
            let dependencies = try Many { DependencyParser() } separator: { "\n" } terminator: { End() }.parse(inputParts[1])
            let circuitDependencies = dependencies.reduce(into: [:]) { result, value in
                result[value.3] = (value.0, value.2, value.1)
            }
            return Circuit(wires: circuitWires, dependencies: circuitDependencies)
        } catch {
            fatalError("Failed to parse input: \(error)")
        }
    }

    func part1() -> Any {
        var circuit = entities
        return circuit.run()
    }

    func part2() -> Any {
        return entities.findFaults().map { $0.name }.sorted().joined(separator: ",")
    }
}
