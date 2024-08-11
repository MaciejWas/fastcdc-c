# FastCDC-C

Haskell bindings for FastCDC algorithm.

# Usage

```haskell
import FastCDC (chunkGenerator, ChunkSizeParams(..))

chunkGenerator 
        (ChunkSizeParams
          { minSize = 20000,
            avgSize = 50000,
            maxSize = 80000
          })
        filename
        (\start size -> undefined) -- Do something with the result

```

If you fancy parallel programming, you might consider putting tuples `(start, size)` in 

# References

* C code authored by https://github.com/sleepybishop/fastcdc