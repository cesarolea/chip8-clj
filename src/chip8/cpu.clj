(ns chip8.cpu
  (:require [clojure.core.match :refer [match]]))

(defn- num->digits
  [num]
  (loop [n num res []]
    (if (zero? n)
      res
      (recur (quot n 10) (cons (mod n 10) res)))))

;; Memory Map:
;; +---------------+= 0xFFF (4095) End of Chip-8 RAM
;; |               |
;; |               |
;; |               |
;; |               |
;; |               |
;; | 0x200 to 0xFFF|
;; |     Chip-8    |
;; | Program / Data|
;; |     Space     |
;; |               |
;; |               |
;; |               |
;; +- - - - - - - -+= 0x600 (1536) Start of ETI 660 Chip-8 programs
;; |               |
;; |               |
;; |               |
;; +---------------+= 0x200 (512) Start of most Chip-8 programs
;; | 0x000 to 0x1FF|
;; | Reserved for  |
;; |  interpreter  |
;; +---------------+= 0x000 (0) Start of Chip-8 RAM
(defonce memory (byte-array 4096))

;; Programs may also refer to a group of sprites representing the hexadecimal
;; digits 0 through F. These sprites are 5 bytes long, or 8x5 pixels.
;; The data should be stored in the interpreter area of Chip-8 memory
;; (0x000 to 0x1FF).
(defonce font-sprites  [0xF0 0x90 0x90 0x90 0xF0 ;; 0
                        0x20 0x60 0x20 0x20 0x70 ;; 1
                        0xF0 0x10 0xF0 0x80 0xF0 ;; 2
                        0xF0 0x10 0xF0 0x10 0xF0 ;; 3
                        0x90 0x90 0xF0 0x10 0x10 ;; 4
                        0xF0 0x80 0xF0 0x10 0xF0 ;; 5
                        0xF0 0x80 0xF0 0x90 0xF0 ;; 6
                        0xF0 0x10 0x20 0x40 0x40 ;; 7
                        0xF0 0x90 0xF0 0x90 0xF0 ;; 8
                        0xF0 0x90 0xF0 0x10 0xF0 ;; 9
                        0xF0 0x90 0xF0 0x90 0x90 ;; A
                        0xE0 0x90 0xE0 0x90 0xE0 ;; B
                        0xF0 0x80 0x80 0x80 0xF0 ;; C
                        0xE0 0x90 0x90 0x90 0xE0 ;; D
                        0xF0 0x80 0xF0 0x80 0xF0 ;; E
                        0xF0 0x80 0xF0 0x80 0x80 ;; F
                        ])

;; The stack is an array of 16 16-bit values, used to store the address that the interpreter shoud
;; return to when finished with a subroutine. Chip-8 allows for up to 16 levels of nested subroutines.
(defonce stack (short-array 16))

;; The original implementation of the Chip-8 language used a 64x32-pixel monochrome display with
;; this format:
;; (0,0)	(63,0)
;; (0,31)	(63,31)
(defonce framebuffer (object-array [(byte-array 8) (byte-array 8) (byte-array 8) (byte-array 8)
                                    (byte-array 8) (byte-array 8) (byte-array 8) (byte-array 8)
                                    (byte-array 8) (byte-array 8) (byte-array 8) (byte-array 8)
                                    (byte-array 8) (byte-array 8) (byte-array 8) (byte-array 8)
                                    (byte-array 8) (byte-array 8) (byte-array 8) (byte-array 8)
                                    (byte-array 8) (byte-array 8) (byte-array 8) (byte-array 8)
                                    (byte-array 8) (byte-array 8) (byte-array 8) (byte-array 8)
                                    (byte-array 8) (byte-array 8) (byte-array 8) (byte-array 8)]))

(defn write-fb [x y n sprite]
  (when (> n 0)
    (let [overflow-x (> x (- 64 8))
          overflow-y (>= (+ y (dec n)) 32)
          x1 (/ x 8)
          x2 (if overflow-x 0 (inc x1))
          v1 (bit-shift-right sprite (mod x 8))
          v2 (bit-shift-left sprite (- 8 (mod x 8)))]
      (doseq [row (if overflow-y (range y 32) (range y (+ y n)))]
        (let [y1 (get framebuffer row)]
          (aset-byte y1 x1 (unchecked-byte (bit-or (aget y1 x1) v1)))
          (when (and (< x2 8)
                     (> (mod x 8) 0))
            (aset-byte y1 x2 (unchecked-byte (bit-or (aget y1 x2) v2))))
          (aset framebuffer row y1)))
      (when overflow-y
        (doseq [row (range (- n (count (range y 32))))]
          (let [y1 (get framebuffer row)]
            (aset-byte y1 x1 (unchecked-byte (bit-or (aget y1 x1) v1)))
            (aset-byte y1 x2 (unchecked-byte (bit-or (aget y1 x2) v2)))
            (aset framebuffer row y1)))))))

(defn byte->ubyte [byte]
  (bit-and byte 0xFF))

(defn short->ushort [short]
  (bit-and short 0xFFFF))

(defn read-mem
  "Read a value from memory"
  [addr]
  (aget memory addr))

(defn write-mem
  "Writes a value to memory as an unchecked byte"
  [addr value]
  (aset-byte memory addr (unchecked-byte value)))

(defn- init-fonts [font-sprites idx]
  (loop [idx 0
         sprite (get font-sprites idx)]
    (when (< idx (count font-sprites))
      (write-mem idx sprite)
      (recur (inc idx) (get font-sprites (inc idx))))))

;; Chip-8 has 16 general purpose 8-bit registers, usually referred to as Vx, where x is a hexadecimal
;; digit (0 through F). There is also a 16-bit register called I. This register is generally used to
;; store memory addresses, so only the lowest (rightmost) 12 bits are usually used.

(defonce Vx-registers (byte-array 16))
(defonce I-register (short-array 1))

;; There are also some "pseudo-registers" which are not accessable from Chip-8 programs.
;; The program counter (PC) should be 16-bit, and is used to store the currently executing address.
;; The stack pointer (SP) can be 8-bit, it is used to point to the topmost level of the stack.
(defonce PC (short-array 1))
(defonce SP (byte-array 1))

(defn read-reg [reg]
  (cond
    (= reg :I) (aget I-register 0)
    (and (>= reg 0)
         (<= reg 0xF)) (aget Vx-registers reg)
    true (throw (Exception. "Undefined register exception"))))

(defn write-reg [reg value]
  (cond
    (= reg :I) (aset-short I-register 0 (unchecked-short value))
    (and (>= reg 0)
         (<= reg 0xF)) (aset-byte Vx-registers reg (unchecked-byte value))
    true (throw (Exception. "Undefined register exception"))))

(defn sp-push
  "Pushes value to the address pointed at by SP"
  [value]
  (let [sp (byte->ubyte (aget SP 0))]
    (aset-byte SP 0 (unchecked-byte (inc sp)))
    (aset-short stack sp (unchecked-short value))))

(defn sp-pop
  "Takes the value pointed at by SP"
  []
  (when (= (aget SP 0) 0) (throw (Exception. "SP underflow")))
  (let [sp (dec (byte->ubyte (aget SP 0)))]
    (aset-byte SP 0 (unchecked-byte sp))
    (aget stack sp)))

(defn reset []
  (doseq [addr (range 4096)]
    (write-mem addr 0))
  (doseq [addr (range 16)]
    (aset-byte stack addr (unchecked-byte 0)))
  (doseq [addr (range 16)]
    (aset-byte Vx-registers addr 0))
  (doseq [row (range 32)]
    (aset framebuffer row (byte-array 8)))
  (init-fonts font-sprites 0)
  (aset-short I-register 0 0)
  (aset-byte SP 0 0)
  (aset-short PC 0 0x200))

;; OPCODES

;; 0nnn - SYS addr
;; Jump to a machine code routine at nnn.
(defn opcode-0nnn
  "This instruction is only used on the old computers on which Chip-8 was originally implemented.
  It is ignored by modern interpreters."
  []
  (println "opcode-0nnn"))

;; 00E0 - CLS
;; Clear the display.
(defn opcode-00E0
  "Clear the display. "
  []
  (println "opcode-00E0")
  (doseq [row (range 32)]
    (aset framebuffer row (byte-array 8))))

;; 00EE - RET
;; Return from a subroutine.
(defn opcode-00EE
  "The interpreter sets the program counter to the address at the top of the stack, then subtracts 1
  from the stack pointer."
  []
  (println "opcode-00EE")
  (aset-short PC 0 (sp-pop)))

;; 1nnn - JMP
;; Jump to location nnn.
(defn opcode-1nnn
  "The interpreter sets the program counter to nnn."
  [arg1]
  (println "opcode-1nnn")
  (aset-short PC 0 (unchecked-short arg1)))

;; 2nnn - CALL addr
;; Call subroutine at nnn.
(defn opcode-2nnn
  "The interpreter increments the stack pointer, then puts the current PC on the top of the stack.
  The PC is then set to nnn."
  [arg1]
  (println "opcode-2nnn")
  (sp-push (aget PC 0))
  (aset-short PC 0 (unchecked-short arg1)))

;; 3xkk - SE Vx, byte
;; Skip next instruction if Vx = kk.
(defn opcode-3xkk
  "The interpreter compares register Vx to kk, and if they are equal, increments the program counter
  by 2."
  [arg1 arg2]
  (println "opcode-3xkk")
  (when (= (read-reg arg1) (unchecked-byte arg2))
    (aset-short PC 0 (unchecked-short (+ (aget PC 0) 2)))))

;; 4xkk - SNE Vx, byte
;; Skip next instruction if Vx != kk.
(defn opcode-4xkk
  "The interpreter compares register Vx to kk, and if they are not equal, increments the program
  counter by 2."
  [arg1 arg2]
  (println "opcode-4xkk")
  (when (not= (read-reg arg1) (unchecked-byte arg2))
    (aset-short PC 0 (unchecked-short (+ (aget PC 0) 2)))))

;; 5xy0 - SE Vx, Vy
;; Skip next instruction if Vx = Vy.
(defn opcode-5xy0
  "The interpreter compares register Vx to register Vy, and if they are equal, increments the
  program counter by 2."
  [arg1 arg2]
  (println "opcode-5xy0")
  (when (= (read-reg arg1) (read-reg arg2))
    (aset-short PC 0 (unchecked-short (+ (aget PC 0) 2)))))

;; 6xkk - LD Vx, byte
;; Set Vx = kk.
(defn opcode-6xkk
  "The interpreter puts the value kk into register Vx."
  [arg1 arg2]
  (println "opcode-6xkk")
  (write-reg arg1 arg2))

;; 7xkk - ADD Vx, byte
;; Set Vx = Vx + kk.
(defn opcode-7xkk
  "Adds the value kk to the value of register Vx, then stores the result in Vx."
  [arg1 arg2]
  (println "opcode-7xkk")
  (write-reg arg1 (+ arg2 (read-reg arg1))))

;; 8xy0 - LD Vx, Vy
;; Set Vx = Vy.
(defn opcode-8xy0
  "Stores the value of register Vy in register Vx."
  [arg1 arg2]
  (println "opcode-8xy0")
  (write-reg arg1 (read-reg arg2)))

;; 8xy1 - OR Vx, Vy
;; Set Vx = Vx OR Vy.
(defn opcode-8xy1
  "Performs a bitwise OR on the values of Vx and Vy, then stores the result in Vx.
  A bitwise OR compares the corresponding bits from two values, and if either bit is 1,
  then the same bit in the result is also 1. Otherwise, it is 0. "
  [arg1 arg2]
  (println "opcode-8xy1")
  (write-reg arg1 (bit-or (read-reg arg1) (read-reg arg2))))

;; 8xy2 - AND Vx, Vy
;; Set Vx = Vx AND Vy.
(defn opcode-8xy2
  "Performs a bitwise AND on the values of Vx and Vy, then stores the result in Vx.
  A bitwise AND compares the corresponding bits from two values, and if both bits are 1,
  then the same bit in the result is also 1. Otherwise, it is 0. "
  [arg1 arg2]
  (println "opcode-8xy2")
  (write-reg arg1 (bit-and (read-reg arg1) (read-reg arg2))))

;; 8xy3 - XOR Vx, Vy
;; Set Vx = Vx XOR Vy.
(defn opcode-8xy3
  "Performs a bitwise exclusive OR on the values of Vx and Vy, then stores the result in Vx.
  An exclusive OR compares the corresponding bits from two values, and if the bits are not both
  the same, then the corresponding bit in the result is set to 1. Otherwise, it is 0. "
  [arg1 arg2]
  (println "opcode-8xy3")
  (write-reg arg1 (bit-xor (read-reg arg1) (read-reg arg2))))

;; 8xy4 - ADD Vx, Vy
;; Set Vx = Vx + Vy, set VF = carry.
(defn opcode-8xy4
  "The values of Vx and Vy are added together. If the result is greater than 8 bits (i.e., > 255,)
  VF is set to 1, otherwise 0. Only the lowest 8 bits of the result are kept, and stored in Vx."
  [arg1 arg2]
  (println "opcode-8xy4")
  (let [result (+ (byte->ubyte (read-reg arg1)) (byte->ubyte (read-reg arg2)))]
    (write-reg 0xF (if (> result 255) 1 0))))

;; 8xy5 - SUB Vx, Vy
;; Set Vx = Vx - Vy, set VF = NOT borrow.
(defn opcode-8xy5
  "If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is subtracted from Vx,
  and the results stored in Vx."
  [arg1 arg2]
  (println "opcode-8xy5")
  (let [vx (byte->ubyte (read-reg arg1))
        vy (byte->ubyte (read-reg arg2))
        result (- vx vy)]
    (write-reg 0xF (if (> vx vy) 1 0))
    (write-reg arg1 result)))

;; 8xy6 - SHR Vx {, Vy}
;; Set Vx = Vx SHR 1.
(defn opcode-8xy6
  "If the least-significant bit of Vx is 1, then VF is set to 1, otherwise 0.
  Then Vx is divided by 2."
  [arg1 arg2]
  (println "opcode-8xy6")
  (write-reg 0xF (bit-and (read-reg arg1) 1))
  (write-reg arg1 (/ (byte->ubyte (read-reg arg1)) 2)))

;; 8xy7 - SUBN Vx, Vy
;; Set Vx = Vy - Vx, set VF = NOT borrow.
(defn opcode-8xy7
  "If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is subtracted from Vy,
  and the results stored in Vx."
  [arg1 arg2]
  (println "opcode-8xy7")
  (let [vx (byte->ubyte (read-reg arg1))
        vy (byte->ubyte (read-reg arg2))
        result (- vy vx)]
    (write-reg 0xF (if (> vy vx) 1 0))
    (write-reg arg1 result)))

;; 8xyE - SHL Vx {, Vy}
;; Set Vx = Vx SHL 1.
(defn opcode-8xye
  "If the most-significant bit of Vx is 1, then VF is set to 1, otherwise to 0.
  Then Vx is multiplied by 2."
  [arg1 arg2]
  (println "opcode-8xye")
  (if (> (bit-and (read-reg arg1) 2r10000000) 0)
    (write-reg 0xF 1)
    (write-reg 0xF 0))
  (write-reg arg1 (* (read-reg arg1) 2)))

;; 9xy0 - SNE Vx, Vy
;; Skip next instruction if Vx != Vy.
(defn opcode-9xy0
  "The values of Vx and Vy are compared, and if they are not equal, the program counter is increased
  by 2."
  [arg1 arg2]
  (println "opcode-9xy0")
  (when-not (= (read-reg arg1) (read-reg arg2))
    (aset-short PC 0 (unchecked-short (+ (aget PC 0) 2)))))

;; Annn - LD I, addr
;; Set I = nnn.
(defn opcode-annn
  "The value of register I is set to nnn."
  [arg1]
  (println "opcode-annn")
  (write-reg :I arg1))

;; Bnnn - JP V0, addr
;; Jump to location nnn + V0.
(defn opcode-bnnn
  "The program counter is set to nnn plus the value of V0."
  [arg1]
  (println "opcode-bnnn")
  (aset-short PC 0 (unchecked-short (+ arg1 (read-reg 0)))))

;; Cxkk - RND Vx, byte
;; Set Vx = random byte AND kk.
(defn opcode-cxkk
  "The interpreter generates a random number from 0 to 255, which is then ANDed with the value kk.
  The results are stored in Vx. See instruction 8xy2 for more information on AND."
  [arg1 arg2]
  (println "opcode-cxkk")
  (let [random (rand-int 256)]
    (println "random int" random)
    (write-reg arg1 (bit-and random arg2))))

;; Dxyn - DRW Vx, Vy, nibble
;; Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision.
;; A better explanation of this is:
;; Draw sprite in location Ireg to coordinate Vreg(x) and Vreg(y)
;; Draws N lines of the sprite.
(defn opcode-dxyn
  "The interpreter reads n bytes from memory, starting at the address stored in I.
  These bytes are then displayed as sprites on screen at coordinates (Vx, Vy).
  Sprites are XORed onto the existing screen. If this causes any pixels to be erased,
  VF is set to 1, otherwise it is set to 0. If the sprite is positioned so part of it is outside
  the coordinates of the display, it wraps around to the opposite side of the screen."
  [arg1 arg2 arg3]
  (println "opcode-dxyn")
  (let [mem-bytes
        (reduce (fn [n-bytes iter]
                  (conj n-bytes (read-mem (+ (short->ushort (read-reg :I))
                                             iter))))
                [] (range arg3))]

    mem-bytes))

;; Ex9E - SKP Vx
;; Skip next instruction if key with the value of Vx is pressed.
(defn opcode-ex9e
  "Checks the keyboard, and if the key corresponding to the value of Vx is currently in the down
  position, PC is increased by 2."
  [arg1]
  (println "opcode-ex9e"))

;; ExA1 - SKNP Vx
;; Skip next instruction if key with the value of Vx is not pressed.
(defn opcode-exa1
  "Checks the keyboard, and if the key corresponding to the value of Vx is currently in the up
  position, PC is increased by 2."
  [arg1]
  (println "opcode-exa1"))

;; Fx07 - LD Vx, DT
;; Set Vx = delay timer value.
(defn opcode-fx07
  "The value of DT is placed into Vx."
  [arg1]
  (println "opcode-fx07"))

;; Fx0A - LD Vx, K
;; Wait for a key press, store the value of the key in Vx.
(defn opcode-fx0a
  "All execution stops until a key is pressed, then the value of that key is stored in Vx."
  [arg1]
  (println "opcode-fx0a"))

;; Fx15 - LD DT, Vx
;; Set delay timer = Vx.
(defn opcode-fx15
  "DT is set equal to the value of Vx."
  [arg1]
  (println "opcode-fx15"))

;; Fx18 - LD ST, Vx
;; Set sound timer = Vx.
(defn opcode-fx18
  "ST is set equal to the value of Vx."
  [arg1]
  (println "opcode-fx18"))

;; Fx1E - ADD I, Vx
;; Set I = I + Vx.
(defn opcode-fx1e
  "The values of I and Vx are added, and the results are stored in I."
  [arg1]
  (println "opcode-fx1e")
  (let [i (short->ushort (read-reg :I))
        result (+ i (byte->ubyte (read-reg arg1)))]
    (write-reg :I result)))

;; Fx29 - LD F, Vx
;; Set I = location of sprite for digit Vx.
(defn opcode-fx29
  "The value of I is set to the location for the hexadecimal sprite corresponding to the value of
  Vx."
  [arg1]
  (println "opcode-fx29"))


;; Fx33 - LD B, Vx
;; Store BCD representation of Vx in memory locations I, I+1, and I+2.
(defn opcode-fx33
  "The interpreter takes the decimal value of Vx, and places the hundreds digit in memory at
  location in I, the tens digit at location I+1, and the ones digit at location I+2."
  [arg1]
  (println "opcode-fx33")
  (let [num (byte->ubyte (read-reg arg1))
        digits (num->digits num)]
    (write-mem (+ (short->ushort (read-reg :I)) 2) (nth digits 2 0))
    (write-mem (+ (short->ushort (read-reg :I)) 1) (nth digits 1 0))
    (write-mem (short->ushort (read-reg :I)) (nth digits 0 0))))

;; Fx55 - LD [I], Vx
;; Store registers V0 through Vx in memory starting at location I.
(defn opcode-fx55
  "The interpreter copies the values of registers V0 through Vx into memory, starting at the address
  in I."
  [arg1]
  (println "opcode-fx55")
  (doseq [offset (range (inc arg1))]
    (write-mem (+ (short->ushort (read-reg :I)) offset) (byte->ubyte (read-reg offset)))))

;; Fx65 - LD Vx, [I]
;; Read registers V0 through Vx from memory starting at location I.
(defn opcode-fx65
  "The interpreter reads values from memory starting at location I into registers V0 through Vx."
  [arg1]
  (println "opcode-fx65")
  (doseq [offset (range (inc arg1))]
    (write-reg offset (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) offset))))))

(defn evaluate
  [opcode]
  (let [opcode-match (vec (format "%04X" opcode))]
    (match opcode-match
           [\0 \0 \E \0] (opcode-00E0)
           [\0 \0 \E \E] (opcode-00EE)
           [\1  _  _  _] (opcode-1nnn (bit-and opcode 0x0FFF))
           [\2  _  _  _] (opcode-2nnn (bit-and opcode 0x0FFF))
           [\3  _  _  _] (opcode-3xkk (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-and opcode 0x00FF))
           [\4  _  _  _] (opcode-4xkk (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-and opcode 0x00FF))
           [\5  _  _ \0] (opcode-5xy0 (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-shift-right (bit-and opcode 0x00F0) 4))
           [\6  _  _  _] (opcode-6xkk (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-and opcode 0x00FF))
           [\7  _  _  _] (opcode-7xkk (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-and opcode 0x00FF))
           [\8  _  _ \0] (opcode-8xy0 (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-shift-right (bit-and opcode 0x00F0) 4))
           [\8  _  _ \1] (opcode-8xy1 (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-shift-right (bit-and opcode 0x00F0) 4))
           [\8  _  _ \2] (opcode-8xy2 (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-shift-right (bit-and opcode 0x00F0) 4))
           [\8  _  _ \3] (opcode-8xy3 (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-shift-right (bit-and opcode 0x00F0) 4))
           [\8  _  _ \4] (opcode-8xy4 (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-shift-right (bit-and opcode 0x00F0) 4))
           [\8  _  _ \5] (opcode-8xy5 (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-shift-right (bit-and opcode 0x00F0) 4))
           [\8  _  _ \6] (opcode-8xy6 (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-shift-right (bit-and opcode 0x00F0) 4))
           [\8  _  _ \7] (opcode-8xy7 (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-shift-right (bit-and opcode 0x00F0) 4))
           [\8  _  _ \E] (opcode-8xye (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-shift-right (bit-and opcode 0x00F0) 4))
           [\9  _  _ \0] (opcode-9xy0 (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-shift-right (bit-and opcode 0x00F0) 4))

           [\F  _ \1 \E] (opcode-fx1e (bit-shift-right (bit-and opcode 0x0F00) 8))
           [\F  _ \3 \3] (opcode-fx33 (bit-shift-right (bit-and opcode 0x0F00) 8))
           [\F  _ \5 \5] (opcode-fx55 (bit-shift-right (bit-and opcode 0x0F00) 8))
           [\F  _ \6 \5] (opcode-fx65 (bit-shift-right (bit-and opcode 0x0F00) 8))
           [\A  _  _  _] (opcode-annn (bit-and opcode 0x0FFF))
           [\B  _  _  _] (opcode-bnnn (bit-and opcode 0x0FFF))
           [\C  _  _  _] (opcode-cxkk (bit-shift-right (bit-and opcode 0x0F00) 8)
                                      (bit-and opcode 0x00FF))

           ;; and last...
           [\0  _  _  _] (opcode-0nnn))))
