#include <iostream>
#include <string>
#include <map>
#include <fstream>
#include <time.h>
#include <thread>
#include <unordered_map>
#include <set>


using namespace std;

enum CellType { EMPTY, HEAD, TAIL, WIRE };

char cellTypeChar(CellType cellType) {
    switch (cellType) {
        case HEAD: return 'H';
        case TAIL: return 'T';
        case WIRE: return '.';
        case EMPTY: return ' ';
    }
}

class Position {
public:
    Position(int x_, int y_);
    const int x;
    const int y;
};

Position::Position(int x_, int y_): x(x_), y(y_) {}

template <>
struct hash<Position>
{
    std::size_t operator()(const Position& p) const {
        return std::hash<int>()(p.x) ^ (std::hash<int>()(p.y) << 1);
    }
};

bool operator<(const Position& l, const Position& r )
{
    if (l.x < r.x)  return true;
    if (l.x > r.x)  return false;
    // Otherwise a are equal
    if (l.y < r.y)  return true;
    if (l.y > r.y)  return false;
    // Otherwise both are equal
    return false;
}

bool operator==(const Position& l, const Position& r) {
    return l.x == r.x && l.y == r.y;
}

class Cell {
public:
    Cell(Position pos_, CellType cellType_);
    const Position pos;
    const CellType cellType;
    string toString() {
        return "<Cell at " + to_string(pos.x) + "," + to_string(pos.y) + " of type " + cellTypeChar(cellType) + ">";
    }
};

Cell::Cell(Position pos_, CellType cellType_): pos(pos_), cellType(cellType_) {}

class World {
public:
    virtual void transition() {
        cout << "I AM NOT IMPLEMENTED" << endl;
    }

    void printWorld() {
        int x, y;
        for (y = 0; y < height; y++) {
            for (x = 0; x < width; x++) {
                cout << cellTypeChar(getCell(x, y));
            }
            cout << endl;
        }
    }

    virtual CellType getCell(int x, int y) {
        cout << "I AM NOT IMPLEMENTED" << endl;
        return EMPTY;
    }

    bool isHead(int x, int y) {
        return getCell(x, y) == HEAD;
    }

    int height = 0, width = 0, numberOfHeads = 0;

    void run(int transitions, bool verbose) {
        clock_t t = clock();

        for (int x = 0; x < transitions; x++) {
            if (verbose)
                printWorld();

            cout << "number of heads: " << numberOfHeads << endl;
            transition();
        }
        if (verbose) {
            printWorld();
            cout << endl;
        }

        t = clock() - t;
        cout << "that took " << ((float)t) / CLOCKS_PER_SEC / transitions << " seconds per transition." << endl;
    }
};

// before multithreading: that took 0.0610326 seconds per transition.



class MapWorld : public World {
public:

    std::map<Position, CellType> cells;
    std::map<Position, int> adjacentHeads;
    std::set<Position> headAdjacentCells;


    CellType getCell(int x, int y) {
        const Position pos = Position(x, y);
        if (cells.count(pos) == 1) {
            return cells.at(pos);
        } else {
            return EMPTY;
        }
    }

    void countAdjacentHeads() {
        adjacentHeads.clear();
        headAdjacentCells.clear();

        for (auto iter : cells) {
            Position pos = iter.first;
            CellType cellType = iter.second;

            switch (cellType) {
                case HEAD: {
                    int x = pos.x;
                    int y = pos.y;

                    for (int dx : { -1, 0, 1}) {
                        for (int dy : { -1, 0, 1}) {
                            if (dx != 0 || dy != 0) {
                                headAdjacentCells.insert(Position(x + dx, y + dy));
                                adjacentHeads[Position(x + dx, y + dy)]++;
                            }
//                                cout << x + dx << "," << y + dy << " " << adjacentHeads[thisThread][Position(x + dx, y + dy)] << endl;
                        }
                    }
                }
                default: break;
            };
        }

    }

    void transition() {
        numberOfHeads = 0;

        countAdjacentHeads();

        for (auto iter : cells) {
            Position pos = iter.first;
            CellType cellType = iter.second;

            switch (cellType) {
                case WIRE: {
                    if (headAdjacentCells.count(pos)) {
                        int count = adjacentHeads[pos];
                        if (count == 1 || count == 2) {
                            cells[pos] = HEAD;
                            numberOfHeads++;
                        }
                    }
                    break;
                }
                case TAIL: cells[pos] = WIRE; break;
                case HEAD: cells[pos] = TAIL; break;
                default: break;
            };
        }
    }

    MapWorld(std::map<Position, CellType> _cells): cells(_cells) {
        numberOfHeads = 0;
        for (auto iter : cells) {
            Position pos = iter.first;
            width = max(width, pos.x + 1);
            height = max(height, pos.y + 1);
            if (iter.second == HEAD)
                numberOfHeads++;
        }
    }
};

MapWorld loadFromFile(string filepath) {
    std::map<Position, CellType> cells;

    int y = 0;
    std::ifstream file(filepath);

    if (file.good()) {
        cout << "yeah it's good\n";
    } else {
        cout << "nope it's bad\n";
    }

    std::string line;
    while (getline(file, line))
    {
        y++;
        int x = 0;
        for (auto symbol: line) {
            switch (symbol) {
                case 'H': cells[Position(x, y)] = HEAD; break;
                case 'T': cells[Position(x, y)] = TAIL; break;
                case '.': cells[Position(x, y)] = WIRE; break;
                case ' ': break;
                default: cout << "SHIT, something's wrong" << endl; break;
            }
            x++;
        }
    }
    return MapWorld(cells);
};

int main() {
    MapWorld world = loadFromFile("/Users/bshlegeris/Dropbox/repos/wireworld_madness/wireworlds/langton11x11.txt");
//    cout << world.height << endl;
//    world.printWorld();
    world.run(500, false);
    return 0;
}
