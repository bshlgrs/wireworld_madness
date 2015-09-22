#include <iostream>
#include <string>
#include <map>

using namespace std;

enum CellType { HEAD, TAIL, WIRE, EMPTY };

char cellTypeChar(CellType cellType) {
    switch (cellType) {
        case HEAD: return '#';
        case TAIL: return '*';
        case WIRE: return 'o';
        case EMPTY: return '.';
    }
}

class Position {
    public:
        Position(int x_, int y_);
        const int x;
        const int y;
};

Position::Position(int x_, int y_): x(x_), y(y_) {}


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

        virtual void printWorld() {
            cout << "I AM NOT IMPLEMENTED" << endl;
        }

        virtual CellType getCell(int x, int y) {
            cout << "I AM NOT IMPLEMENTED" << endl;
            return EMPTY;
        }

        int height, width;
};

class ListWorld {
    public:
        std::map<Position, CellType> cells;

    CellType getCell(int x, int y) {
        const Position pos = Position(x, y);
        if (cells.count(pos) == 1) {
            return cells.at(pos);
        } else {
            return EMPTY;
        }
    }
};

int main() {
    cout << "Hello, World!" << endl;

    Position pos = Position(3, 4);
    Cell cell = Cell(pos, HEAD);

    cout << cell.toString() << endl;
    return 0;
}
