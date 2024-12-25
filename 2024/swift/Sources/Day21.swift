struct Day21: AdventDay {
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
    }

    enum PadDirection: CaseIterable {
        case up, down, left, right
        func coord() -> Coord {
            return switch self {
            case .up: Coord(0, -1)
            case .down: Coord(0, 1)
            case .right: Coord(1, 0)
            case .left: Coord(-1, 0)
            }
        }
        func key() -> Key {
            return switch self {
            case .up: Key.KeyUp
            case .down: Key.KeyDown
            case .right: Key.KeyRight
            case .left: Key.KeyLeft
            }
        }
    }

    typealias Path = [Key]

    struct Keypad {
        let pad: [Coord: Key]
        let inverseLookup: [Key: Coord]
        let shortestPaths: [Key: [Key: [Path]]]

        init(_ pad: [[Key]]) {
            var _pad: [Coord: Key] = [:]
            _pad.reserveCapacity(pad.count * pad.first!.count)
            for (j, row) in pad.enumerated() {
                for (i, key) in row.enumerated() {
                    _pad[Coord(i, j)] = key
                }
            }
            self.pad = _pad
            let _inverseLookup = _pad.reduce(into: [:]) { result, pair in
                result[pair.value] = pair.key
            }
            self.inverseLookup = _inverseLookup
            self.shortestPaths = Day21.Keypad.computeShortestPaths(pad: _pad, inverseLookup: _inverseLookup)
        }

        static func computeShortestPaths(pad: [Coord: Key], inverseLookup: [Key: Coord]) -> [Key: [Key: [Path]]] {
            func neighbours(key: Key) -> [PadDirection] {
                var ret: [PadDirection] = []
                ret.reserveCapacity(4)
                for direction in PadDirection.allCases {
                    if pad[inverseLookup[key]! + direction.coord()] != nil {
                        ret.append(direction)
                    }
                }
                return ret
            }

            return inverseLookup.keys.reduce(into: [:]) { result, key in
                var paths: [Key: [Path]] = [key: []]
                var lens: [Key: Int] = [:]
                var queue: Deque<(Key, Path)> = [(key, [])]
                while let (currentKey, path) = queue.popLast() {
                    let currentShortestPathLen = lens[currentKey, default: Int.max]

                    if currentShortestPathLen > path.count {
                        paths[currentKey] = [path]
                        lens[currentKey] = path.count
                    } else if currentShortestPathLen == path.count {
                        paths[currentKey]!.append(path)
                    } else {
                        continue
                    }
                    neighboursLoop: for neighbour in neighbours(key: currentKey) {
                        let nextKey = pad[inverseLookup[currentKey]! + neighbour.coord()]!
                        if nextKey == key || nextKey == Key.None { continue neighboursLoop }
                        if paths[nextKey] == nil { paths[nextKey] = [] }
                        queue.prepend((nextKey, path + [neighbour.key()]))
                    }
                }
                for _key in paths.keys {
                    paths[_key] = paths[_key].map { _paths in _paths.map{ $0 + [Key.KeyA] } }
                }
                result[key] = paths
            }
        }
    }

    enum Key: Character {
        case None = "N"
        case KeyA = "A", Key0 = "0"
        case Key1 = "1", Key2 = "2", Key3 = "3"
        case Key4 = "4", Key5 = "5", Key6 = "6"
        case Key7 = "7", Key8 = "8", Key9 = "9"
        case KeyUp = "^", KeyRight = ">", KeyDown = "v", KeyLeft = "<"
    }

    let NUMPAD = Keypad([
        [Key.Key7, Key.Key8, Key.Key9],
        [Key.Key4, Key.Key5, Key.Key6],
        [Key.Key1, Key.Key2, Key.Key3],
        [Key.None, Key.Key0, Key.KeyA],
    ])
    let DPAD = Keypad([
        [Key.None, Key.KeyUp, Key.KeyA],
        [Key.KeyLeft, Key.KeyDown, Key.KeyRight],
    ])

    var entities: [(code: [Key], value: Int)] {
        return data.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: "\n").map { code in
            var codeCopy = code
            codeCopy.removeLast(1)
            return (code.map { letter in Key(rawValue: letter)! }, Int(codeCopy)!)
        }
    }

    struct CTRLSeqSearchKey: Hashable {
        let source: Key
        let target: Key
        let layer: Int

        init(_ source: Key, _ target: Key, _ layer: Int) {
            self.source = source; self.target = target; self.layer = layer
        }
    }

    func findCTRLSeqLen(
        _ source: Key, _ target: Key, _ currentLayer: Int, _ maxLayers: Int,
        cache: inout [CTRLSeqSearchKey: Int], last_key_at_layer: inout [Int: Key]
    ) -> Int {
        if currentLayer == maxLayers { return 1 }
        if cache[CTRLSeqSearchKey(source, target, currentLayer)] != nil { return cache[CTRLSeqSearchKey(source, target, currentLayer)]! }
        
        let pad = currentLayer == 0 ? NUMPAD : DPAD
        let possiblePathsAtLayer = pad.shortestPaths[source]![target]!
        var shortestPathLen = Int.max
        
        possiblePathsLoop: for path in possiblePathsAtLayer {
            var currentPathLen = 0
            var previousKey = Key.KeyA
            for key in path {
                let keyCost = findCTRLSeqLen(previousKey, key, currentLayer + 1, maxLayers, cache: &cache, last_key_at_layer: &last_key_at_layer)
                if currentPathLen + keyCost > shortestPathLen { continue possiblePathsLoop }
                currentPathLen += keyCost
                previousKey = key
            }
            if currentPathLen < shortestPathLen {
                shortestPathLen = currentPathLen
            }
        }
        
        cache[CTRLSeqSearchKey(source, target, currentLayer)] = shortestPathLen
        return shortestPathLen
    }

    func part1() -> Any {
        let maxLayers = 3
        var complexities = 0
        for (code, value) in entities {
            var ctrlSeqLen = 0
            var previousKey = Key.KeyA
            var cache: [CTRLSeqSearchKey: Int] = [:]
            var last_key_at_layer: [Int: Key] = [:]
            for key in code {
                ctrlSeqLen += findCTRLSeqLen(previousKey, key, 0, maxLayers, cache: &cache, last_key_at_layer: &last_key_at_layer)
                previousKey = key
            }
            complexities += ctrlSeqLen * value
        }
        return complexities
    }

    func part2() -> Any {
        let maxLayers = 26
        var complexities = 0
        for (code, value) in entities {
            var ctrlSeqLen = 0
            var previousKey = Key.KeyA
            var cache: [CTRLSeqSearchKey: Int] = [:]
            var last_key_at_layer: [Int: Key] = [:]
            for key in code {
                ctrlSeqLen += findCTRLSeqLen(previousKey, key, 0, maxLayers, cache: &cache, last_key_at_layer: &last_key_at_layer)
                previousKey = key
            }
            complexities += ctrlSeqLen * value
        }
        return complexities
    }
}
