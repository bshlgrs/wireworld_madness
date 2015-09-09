# wireworld madness
## crazy wireworld ideas:

Wireworld runner should take an initial world and a function from current_time to number_of_steps_to_run. It then prints out text (maybe just a hash?) of the world.

- make wireworld in python as the simplest case
- python script to turn .bmp into .txt, at least for debugging
- make it List2D style in C++
- make it OrderedList2D style in Python
- make it CUDA-accelerated
- make it split the map into segments, so that I only have to update segments which have exciting things happen
  - experimentally determine how large the segments need to be
- turn individual non-wire components into explicit FSMs.
  - If there's four wires in a blob. That's a tetronimo?
  - Precalculate all behaviors for tetronimos, calculate when the code runs the first time for larger blobs?
  - do it lazily, so it caches behaviors for particular blobs?
  - consider symmetry?
  - consider symmetry between combinations of blobs?
    - that requires solving graph isomorphisms...

Cases I need to consider:

- few heads
  - makes it better to update per-head
- many heads
  - makes it better to have cells look for heads near them

# wireworld_madness
