import Algorithms

struct Day08: AdventDay {
    // Save your data in a corresponding text file in the `Data` directory.
    var data: String
    
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
    
    struct CoordPair: Hashable {
        let coord1: Coord
        let coord2: Coord
        
        init(_ coord1: Coord, _ coord2: Coord) {
            self.coord1 = coord1
            self.coord2 = coord2
        }
        
        func getAntinodeCandidates() -> (Coord, Coord) {
            let diff = coord2 - coord1
            return (coord1 - diff, coord2 + diff)
        }
    }
    
    struct AntennaeMap {
        let antennae: [Character: [Coord]]
        let fullMap: [Coord: Character]
        let bounds: Coord
        
        init(_ antennae: [Character: [Coord]], _ fullMap: [Coord: Character], _ bounds: Coord) {
            self.antennae = antennae
            self.fullMap = fullMap
            self.bounds = bounds
        }
        
        func draw(drawAntinodes: Bool = false, part1: Bool = false) async {
            var drawStr = ""
            let antinodes = if drawAntinodes { part1 ? await self.getAntinodes() : await self.getAllAntinodes() } else { Set<Coord>() }
            
            for j in 0...(self.bounds.x - 1) {
                for i in 0...(self.bounds.y - 1) {
                    let currentCoord = Coord(i,j)
                    if antinodes.contains(currentCoord) {
                        drawStr.append(Character("#"))
                    } else {
                        drawStr.append(self.fullMap[currentCoord]!)
                    }
                }
                drawStr.append(Character("\n"))
            }
            print(drawStr)
        }
        
        func inBounds(_ coord: Coord) -> Bool {
            (coord.x >= 0 && coord.x < self.bounds.x) && (coord.y >= 0 && coord.y < self.bounds.y)
        }
        
        func _generatePairs(_ coords: [Coord]) -> Set<CoordPair> {
            var pairs = Set<CoordPair>()
            pairs.reserveCapacity(2 ^ coords.count + 1)
            
            for coord in coords {
                for coord2 in coords {
                    if !pairs.contains(CoordPair(coord, coord2)) && !pairs.contains(CoordPair(coord2, coord)) && coord != coord2 {
                        pairs.insert(CoordPair(coord, coord2))
                    }
                }
            }
            
            return pairs
        }
        
        func _antinodeValid(_ coord: Coord) -> Bool {
            self.inBounds(coord) // && self.fullMap[coord] == "."
        }
        
        func getAntinodes() async -> Set<Coord> {
            let antinodeResults: Set<Coord> = await withTaskGroup(of: [Coord].self) { group in
                for coords in self.antennae.values {
                    group.addTask {
                        let coordPairs = self._generatePairs(coords)
                        var antinodeCoords: [Coord] = []
                        antinodeCoords.reserveCapacity(coordPairs.count * 2)
                        for coordPair in coordPairs {
                            let (antinode1, antinode2) = coordPair.getAntinodeCandidates()
                            if self._antinodeValid(antinode1) { antinodeCoords.append(antinode1) }
                            if self._antinodeValid(antinode2) { antinodeCoords.append(antinode2) }
                        }
                        return antinodeCoords
                    }
                }
                return await group.reduce(into: Set<Coord>(), { result, coords in
                    result.formUnion(coords)
                })
            }
            return antinodeResults
        }
        
        func getAllAntinodes() async -> Set<Coord> {
            let antinodeResults: Set<Coord> = await withTaskGroup(of: [Coord].self) { group in
                for coords in self.antennae.values {
                    group.addTask {
                        let coordPairs = self._generatePairs(coords)
                        var antinodeCoords: [Coord] = []
                        let mapHypotenuse = Int(Double(self.bounds.x ^ 2 + self.bounds.y ^ 2).squareRoot().rounded(.up))
                        antinodeCoords.reserveCapacity(coordPairs.count * mapHypotenuse)
                        for coordPair in coordPairs {
                            antinodeCoords.append(coordPair.coord1)
                            antinodeCoords.append(coordPair.coord2)
                            let diff = coordPair.coord2 - coordPair.coord1
                            var leftAntinode = coordPair.coord1 - diff
                            while self.inBounds(leftAntinode) {
                                if self._antinodeValid(leftAntinode) { antinodeCoords.append(leftAntinode) }
                                leftAntinode = leftAntinode - diff
                            }
                            var rightAntinode = coordPair.coord2 + diff
                            while self.inBounds(rightAntinode) {
                                if self._antinodeValid(rightAntinode) { antinodeCoords.append(rightAntinode) }
                                rightAntinode = rightAntinode + diff
                            }
                        }
                        return antinodeCoords
                    }
                }
                return await group.reduce(into: Set<Coord>(), { result, coords in
                    result.formUnion(coords)
                })
            }
            return antinodeResults
        }
    }

    // Splits input data into its component parts and convert from string.
    var entities: AntennaeMap {
        let lines = self.data.split(separator: "\n")
        let firstLine = lines.first!
        var antennae: [Character: [Coord]] = [:]
        antennae.reserveCapacity(48)
        var fullMap: [Coord: Character] = [:]
        fullMap.reserveCapacity(lines.count * firstLine.count)
        
        for (j, line) in lines.enumerated() {
            for (i, char) in line.enumerated() {
                if char != "." {
                    if !antennae.keys.contains(char) {
                        antennae[char] = []
                    }
                    antennae[char]!.append(Coord(i,j))
                }
                fullMap[Coord(i,j)] = char
            }
        }
        
        return AntennaeMap(antennae, fullMap, Coord(firstLine.count, lines.count))
    }

    // Replace this with your solution for the first part of the day's challenge.
    func part1() async -> Int {
//        await entities.draw(drawAntinodes: true)
        return await entities.getAntinodes().count
    }

    // Replace this with your solution for the second part of the day's challenge.
    func part2() async -> Int {
//        await entities.draw(drawAntinodes: true, part1: false)
        return await entities.getAllAntinodes().count
    }
}
