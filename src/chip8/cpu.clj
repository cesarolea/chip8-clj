(ns chip8.cpu
  (:require [clojure.core.match :refer [match]]))

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

;; The stack is an array of 16 16-bit values, used to store the address that the interpreter shoud
;; return to when finished with a subroutine. Chip-8 allows for up to 16 levels of nested subroutines.
(defonce stack (short-array 16))

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
  (aset-short I-register 0 0)
  (aset-byte SP 0 0)
  (aset-short PC 0 0x200))

;; opcodes (implement with pattern matching) https://www.youtube.com/watch?v=mi3OtBc73-k

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
  (doseq [addr (range 511)]
    (write-mem addr 0)))

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

(defn opcode-6xkk
  "The interpreter puts the value kk into register Vx."
  [arg1 arg2]
  (println "opcode-6xkk")
  (write-reg arg1 (unchecked-byte arg2)))

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
           ;; and last...
           [\0  _  _  _] (opcode-0nnn))))
