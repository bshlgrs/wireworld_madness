from PIL import Image
from enum import Enum
import time

class Cell(Enum):
  head = 1
  tail = 2
  metal = 3

class Wireworld:
  @classmethod
  def from_bmp(cls, filename):
    world_file = Image.open(filename).convert("RGB")
    width, height = world_file.size

    world = {}

    for x in range(width):
      for y in range(height):
        r, g, b = world_file.getpixel((x, y))
        if r > 250 and g > 250 and b < 5:
          world[(x, y)] = Cell.metal
        elif (r, g, b) == (255, 0, 0):
          world[(x, y)] = Cell.tail
        elif (r, g, b) == (0, 0, 255):  
          world[(x, y)] = Cell.head
        elif (r, g, b) == (0, 0, 0):
          pass
        else:
          raise Exception("this color shouldn't exist: %d %d %d, at (%d, %d)"%(r, g, b, x, y))
    
    return Wireworld(world)

  def __init__(self, cells):
    self.cells = cells
    self.height = max(y for (x,y) in cells.keys()) + 1
    self.width = max(x for (x,y) in cells.keys()) + 1

    self.post_init()

  def post_init(self):
    pass

  def number_of_heads(self):
    return sum(1 for x in self.cells.values() if x == Cell.head)

  def __repr__(self):
    output = []
    for x in range(self.width):
      for y in range(self.height):
        if (x, y) in self.cells:
          if self.cells[(x,y)] == Cell.head:
            output.append("H")
          elif self.cells[(x,y)] == Cell.tail:
            output.append("T")
          elif self.cells[(x,y)] == Cell.metal:
            output.append("o")
        else:
          output.append(" ")
      output.append("\n")    
    return "".join(output)

  def number_of_cells(self):
    return len(self.cells)

  def transition(self):
    def doesPositionContainHead(x, y):
      return self.cells.get((x, y)) == Cell.head

    new_states = {}
    for coord, cell in self.cells.items():
      if cell == Cell.head:
        new_states[coord] = Cell.tail
      elif cell == Cell.tail:
        new_states[coord] = Cell.metal
      elif cell == Cell.metal:
        x, y = coord
        neighboringHeads = sum(doesPositionContainHead(x + dx, y + dy) for dx in [-1, 0, 1] for dy in [-1, 0, 1])
        if neighboringHeads in [1, 2]:
          new_states[coord] = Cell.head
        else:
          new_states[coord] = Cell.metal

    for coord in new_states:
      self.cells[coord] = new_states[coord]

class FasterWireworld(Wireworld):
  pass

def run_world(filename, generations, verbose):
  world = Wireworld.from_bmp(filename)
  print("this world has %d cells in it"%(world.number_of_cells()))
  startTime = time.time()

  for x in range(generations):
    world.transition()
    print(world.number_of_heads())

    if verbose:
      print(world)

  return (time.time() - startTime, world)


def print_world(filename):
  print(Wireworld.from_bmp(filename))

def test_time(filename, generations):
  delta, world = run_world(filename, generations, False)
  
  print("time taken: %f total, %f per transition, %f per transition per cell" % 
    (delta, delta / generations, delta / generations / world.number_of_cells()))


# test_time("comp1100_haskell/Wireworlds/Langton's_Ant_5x5.bmp", 10)
# test_time("comp1100_haskell/Wireworlds/Cycle.bmp", 100)

print_world("wireworlds/Langton's_Ant_5x5.bmp")