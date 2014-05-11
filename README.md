Nettle bindings for Common Lisp
===============================
This is a set of low-level bindings to the [Nettle](http://www.lysator.liu.se/~nisse/nettle/),
as well as a very thin layer of abstraction to make things a bit more lispy than
running raw C functions.

The goal of this project is to provide people who know what they're doing a
method of performing cryptography in Common Lisp using a well tested library.
This project does not come with any warranty, and most certainly will not try to
stop you from shooting yourself in the foot.

Conventions
-----------
Who needs documentation when you follow simple function-naming conventions?

- The package prefix is `ne:`
- Underscores become dashes

See the examples/ folder and also the [Nettle documentation](http://www.lysator.liu.se/~nisse/nettle/nettle.html).

### Example
```c
char * test;
```

Becomes:

```common-lisp
(let ((test nil)))
```

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
