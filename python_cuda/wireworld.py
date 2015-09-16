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

def test_time(world, generations):
  world = Wireworld.from_bmp(world)


# world = Wireworld.from_bmp("comp1100_haskell/Wireworlds/Cycle.bmp")


# print(world.height)
  startTime = time.time()

  for x in range(generations):
    world.transition()
  # print(world)

  delta = time.time() - startTime

  print("time taken: %f total, %f per transition" % (delta, delta / generations))


test_time("comp1100_haskell/Wireworlds/Langton's_Ant_3x3.bmp", 100)