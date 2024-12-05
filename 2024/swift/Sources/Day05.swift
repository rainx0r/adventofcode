import Algorithms
import Parsing

struct Day05: AdventDay {
    // Save your data in a corresponding text file in the `Data` directory.
    var data: String
    
    struct RuleParser: Parser {
        var body: some Parser<Substring, (Int, Int)> {
            Int.parser()
            "|"
            Int.parser()
        }
    }
    
    struct RulesParser: Parser {
        var body: some Parser<Substring, [(Int, Int)]> {
            Many {
                RuleParser()
            } separator: {
                "\n"
            }
        }
    }
    
    struct LineParser: Parser {
        var body: some Parser<Substring, [Int]> {
            Many {
                Int.parser()
            } separator: {
                ","
            }
        }
    }
    
    struct InputParser: Parser {
        var body: some Parser<Substring, ([(Int, Int)], [[Int]])> {
            RulesParser()
            "\n\n"
            Many {
                LineParser()
            } separator: {
                "\n"
            } terminator: {
                End()
            }
        }
    }

    // Splits input data into its component parts and convert from string.
    var entities: ([(Int, Int)], [[Int]]) {
        do {
            return try InputParser().parse(data)
        } catch {
            fatalError("Unable to parse data: \(error)")
        }
    }
    
    struct Pair: Hashable {
        let first: Int
        let second: Int

        init(_ first: Int, _ second: Int) {
            self.first = first
            self.second = second
        }
    }
    
    func ordering(_ rules: [(Int, Int)]) -> [Pair: Bool] {
        var dict: [Pair: Bool] = [:]
        dict.reserveCapacity(rules.count * 2)
        for (first, second) in rules {
            dict[Pair(first, second)] = true
            dict[Pair(second, first)] = false
        }
        
        return dict
    }
    
    func bookIsValid(_ ordering: [Pair: Bool]) -> ([Int]) -> Bool {
        {
            pages in
            let pageCount = pages.count
            for i in 0 ..< pageCount - 1 {
                for j in i + 1 ..< pageCount {
                    let pair = Pair(pages[i], pages[j])
                    if ordering[pair] ?? true { continue } else {
                        return false
                    }
                }
            }
            return true
        }
    }
    
    func middleValue(_ list: [Int]) -> Int { list[list.count / 2] }
    
    func sortingFunction(_ ordering: [Pair: Bool]) -> ((Int, Int) -> Bool) {
        { first, second in
            ordering[Pair(first, second)] ?? true
        }
    }

    // Replace this with your solution for the first part of the day's challenge.
    func part1() async throws -> Int {
        let (rules, books) = entities
        let ordering = ordering(rules)
        
        return books
            .filter { !$0.isEmpty }
            .filter(bookIsValid(ordering))
            .map(middleValue)
            .reduce(0, +)
    }

    // Replace this with your solution for the second part of the day's challenge.
    func part2() -> Int {
        let (rules, books) = entities
        let ordering = ordering(rules)
        
        return books
            .filter { !$0.isEmpty }
            .filter { !bookIsValid(ordering)($0) }
            .map { $0.sorted(by: sortingFunction(ordering)) }
            .map(middleValue)
            .reduce(0, +)
    }
}
