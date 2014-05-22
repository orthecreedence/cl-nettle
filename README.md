Nettle bindings for Common Lisp
===============================
This is a set of low-level bindings to the [Nettle](http://www.lysator.liu.se/~nisse/nettle/)
library, as well as a thin layer of fixes and high(er)-level functionality over
the raw bindings.

The goal of this project is to provide people who know what they're doing a
method of performing cryptography in Common Lisp using a well tested library.
This library is best used if you have an understanding of the underlying crypto
primitives and how they function.

Documentation
-------------

TODO (sorry). For now, check high-level/package.lisp and look at the definitions
of the exported functions.

(re)Generating
--------------
If a new version of Nettle comes out, you can regenerate these bindings by doing
the following (if you have [swig](http://www.swig.org/) installed):

```bash
cd /path/to/cl-nettle
vim scripts/bindings.i      # update "%include" paths to point at your Nettle headers
./scripts/generate          # must be run in cl-nettle folder
```

This will generate new bindings in their entirety (it's fully automated).

License
-------
MIT Licensed.
