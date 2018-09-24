# chip8-clj

![horns.ch8 running in chip8-clj](https://raw.githubusercontent.com/cesarolea/chip8-clj/master/resources/horns.png)

chip8-clj is a Chip8 interpreter written in Clojure. It mutates state all over the place, so please don't use it as an example of how to write proper Clojure code. It also doesn't implement sound and keypresses (yet).

## Usage

In the REPL:

```
(-main)
(read-rom-file "/path/to/a/rom")
(cpu/resume)
```

## License

Copyright © 2018 César Olea

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
