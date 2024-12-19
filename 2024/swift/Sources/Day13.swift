import Algorithms
import Parsing

struct Day13: AdventDay {
    var data: String
    
    struct ButtonParser: Parser {
        var body: some Parser<Substring, (Int, Int, Int, Int)> {
            "Button A: X+"
            Int.parser()
            ", Y+"
            Int.parser()
            "\n"
            "Button B: X+"
            Int.parser()
            ", Y+"
            Int.parser()
        }
    }
    
    struct PrizeParser: Parser {
        var body: some Parser<Substring, (Int, Int)> {
            "Prize: X="
            Int.parser()
            ", Y="
            Int.parser()
        }
    }
    
    struct MachineParser: Parser {
        var body: some Parser<Substring, (Int, Int, Int, Int, (Int, Int))> {
            ButtonParser()
            "\n"
            PrizeParser()
        }
    }
    
    struct InputParser: Parser {
        var body: some Parser<Substring, [(Int, Int, Int, Int, (Int, Int))]> {
            Many {
                MachineParser()
            } separator: {
                "\n\n"
            } terminator: {
                End()
            }
        }
    }
    
    struct Coord: Hashable {
        let x: Int
        let y: Int

        init(_ x: Int, _ y: Int) {
            self.x = x
            self.y = y
        }
        
        static func + (lhs: Coord, rhs: Coord) -> Coord {
            return Coord(lhs.x + rhs.x, lhs.y + rhs.y)
        }
        
        static func - (lhs: Coord, rhs: Coord) -> Coord {
            return Coord(lhs.x - rhs.x, lhs.y - rhs.y)
        }
    }
    
    struct Machine {
        let a: Coord
        let b: Coord
        let prize: Coord
        
        init(_ data: (Int, Int, Int, Int, (Int, Int))) {
            self.a = Coord(data.0, data.1)
            self.b = Coord(data.2, data.3)
            self.prize = Coord(data.4.0, data.4.1)
        }
        
        func solve() -> Int {
//            print(self)
            let det = a.x * b.y - a.y * b.x
            if det == 0 { return 0 }
            let inverse = [b.y, -b.x, -a.y, a.x]
            let x = Int((inverse[0] * prize.x + inverse[1] * prize.y) / det)
            let y = Int((inverse[2] * prize.x + inverse[3] * prize.y) / det)
            if (b.x * y + a.x * x) != prize.x || (b.y * y + a.y * x) != prize.y { return 0 }
            return 3 * x + y
        }
    }
    
    var entities: [Machine] {
        do {
            return try InputParser().parse(data.trimmingCharacters(in: .whitespacesAndNewlines)).map { Machine($0) }
        } catch {
            fatalError("Unable to parse data: \(error)")
        }
    }

    func part1() -> Int {
//        for machine in entities {
//            print(machine.solve())
//        }
        return entities.map { $0.solve() }.reduce(0, +)
    }

    func part2() -> Int {
        let machines = entities.map { Machine(($0.a.x, $0.a.y, $0.b.x, $0.b.y, ($0.prize.x + 10000000000000, $0.prize.y + 10000000000000))) }
        return machines.map { $0.solve() }.reduce(0, +)
    }
}
