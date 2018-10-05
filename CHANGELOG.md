# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]
### Added
	- Options namespace to handle emulation options (scaling, cpu frequency, sound tone, etc)
	- UI to load roms and control emulation (start, stop, reset)
	- Improve rendering speed, reduces flicker.

## [0.1.1] - 2018-10-04
	Initial version!

	### Added
	- Everything is new I guess...

	### Changed
	- Unrecognized opcode now halts execution and prints the cpu state.

	### Removed
	- Removed dependency on core.async and lanterna.
	- Removed function to play a sequence of sound notes.

	### Fixed
	- Fixed opcodes 8xy5 and 8xy7.
	- Fixed read-fb function. Reading the state of the framebuffer was not working correctly, causing collisions (register VF set to 1) when there should been none. It was more obvious in games like BRIX, where the ball would hit an already cleared brick, causing it to appear again.

[Unreleased]: https://github.com/cesarolea/chip8-clj/compare/0.1.1...HEAD
[0.1.1]: https://github.com/cesarolea/chip8-clj/compare/0.1.0...0.1.1
