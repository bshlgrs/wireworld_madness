# wireworld madness
## crazy wireworld ideas:

Wireworld runner should take an initial world and a function from current_time to number_of_steps_to_run. It then prints out text (maybe just a hash?) of the world.

- make wireworld in python as the simplest case
- python script to turn .bmp into .txt, at least for debugging
- make it List2D style in C++
- make it OrderedList2D style in Python
- make it CUDA-accelerated
  - we can either draw directly from GPU. We don't want to copy back and forth
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

# timings

## trivial Haskell implementation

./Wireworld --test=1000 --model=List_2D --world=Wireworlds/Langton\'s_Ant_11x11.bmp
Active heads in the world: 53 Elapsed time: 27.057 sec  after 1000 transitions on 55965 cells

./Wireworld --test=1000 --model=List_2D

Active heads in the world: 36 Elapsed time: 0.290 sec after 1000 transitions on 655 cells


## trivial Python

$ python3 python_cuda/wireworld.py
time taken: 8.864523s on Langton 11x11 for 1000 transitions