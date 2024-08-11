# FastCDC-C

Haskell bindings for sleepybishop's FastCDC implementation in C.

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

If you fancy parallel programming, there is also `chunkGeneratorAsync` which returns a `Chan`.

# References

* C code authored by https://github.com/sleepybishop/fastcdc
