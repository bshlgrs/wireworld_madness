#include <iostream>
#include <string>

using namespace std;

enum CellType { HEAD, TAIL, WIRE };

char cellTypeChar(CellType cellType) {
    switch (cellType) {
        case HEAD: return 'x';
        case TAIL: return 'o';
        case WIRE: return '.';
    }
}

class Position {
    public:
        Position(int x_, int y_);
        const int x;
        const int y;
};

Position::Position(int x_, int y_): x(x_), y(y_) {}


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
    virtual void transition() {
        cout << "I AM NOT IMPLEMENTED" << endl;
    }

    virtual void printWorld() {
        cout << "I AM NOT IMPLEMENTED" << endl;
    }

    virtual int getHeight() {
        return 0;
    }

    virtual int getWidth() {
        return 0;
    }
};

class ListWorld {

};

int main() {
    cout << "Hello, World!" << endl;

    Position pos = Position(3, 4);
    Cell cell = Cell(pos, HEAD);

    cout << cell.toString() << endl;
    return 0;
}
