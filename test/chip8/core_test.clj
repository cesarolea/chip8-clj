(ns chip8.core-test
  (:require [clojure.test :refer :all]
            [chip8.cpu :refer :all]))

(defn setup []
  (reset))
(defn teardown [])

(defn each-fixture [f]
  (setup)
  (f)
  (teardown))

(use-fixtures :each each-fixture)

(deftest memory-map
  (testing "Size of map is 4096"
    (is (= (count memory) 4096))))

(deftest byte-juggling
  (testing "Byte conversion roundtrip"
    (is (and (= (-> 0xFF unchecked-byte byte->ubyte) 255)
             (= (type (-> 0xFF unchecked-byte byte->ubyte)) java.lang.Long)))))

(deftest fonts
  (testing "Font sprites are initialized"
    (is (= (byte->ubyte (read-mem 0)) 0xF0))
    (is (= (byte->ubyte (read-mem 0x4F)) 0x80))))

(deftest memory-read-write
  (testing "Reads and Writes works as expected"
    (write-mem 0x0000 1)
    (is (= (read-mem 0x0000) 1)))
  (testing "Read a byte"
    (write-mem 0x0001 0xFF)
    (is (= (byte->ubyte (read-mem 0x0001)) 0xFF))))

(deftest register-read-write
  (testing "Read and write to an existing register works"
    (write-reg 0x8 0xFF)
    (write-reg 0xB 0xCC)
    (write-reg :I 0xFFFF)
    (is (= (byte->ubyte (read-reg 0x8)) 0xFF)))
  (testing "I register can store 16 bytes"
    (is (= (short->ushort (read-reg :I)) 0xFFFF))))

(deftest stack-read-write
  (testing "Push"
    (sp-push 0xA)
    (sp-push 0xB)
    (sp-push 0xC)
    (is (= (aget SP 0) 3))
    (is (= (sp-pop) 0xC))
    (is (= (sp-pop) 0xB))
    (is (= (sp-pop) 0xA))
    (is (= (aget SP 0) 0))))

(deftest op-00E0
  (println (:doc (meta #'opcode-00E0)))
  (write-mem 0x80 19)
  (is (= (read-mem 0x80) 19))
  (evaluate 0x00E0)
  (is (= (read-mem 0x80) 0)))

(deftest op-1nnn
  (println (:doc (meta #'opcode-1nnn)))
  (is (= (short->ushort (aget PC 0)) 0x200))
  (evaluate 0x1234)
  (is (= (short->ushort (aget PC 0)) 0x234)))

(deftest op-00EE
  (println (:doc (meta #'opcode-00EE)))
  (is (= (short->ushort (aget PC 0)) 0x200))
  (sp-push 0x2FF)
  (evaluate 0x00EE)
  (is (= (short->ushort (aget PC 0)) 0x2FF)))

(deftest op-2nnn
  (println (:doc (meta #'opcode-2nnn)))
  (is (= (short->ushort (aget PC 0)) 0x200))
  (evaluate 0x2FFF)
  (is (= (short->ushort (aget PC 0)) 0xFFF))
  (is (= (short->ushort (aget stack 0)) 0x200)))

(deftest op-3xkk
  (println (:doc (meta #'opcode-3xkk)))
  (is (= (short->ushort (aget PC 0)) 0x200))
  (write-reg 0xB 0xCC)
  (evaluate 0x3BCC)
  (is (= (short->ushort (aget PC 0)) (+ 0x200 2))))

(deftest op-4xkk
  (println (:doc (meta #'opcode-4xkk)))
  (is (= (short->ushort (aget PC 0)) 0x200))
  (write-reg 0xB 0xCC)
  (evaluate 0x4BAA)
  (is (= (short->ushort (aget PC 0)) (+ 0x200 2))))

(deftest op-5xy0
  (println (:doc (meta #'opcode-5xy0)))
  (is (= (short->ushort (aget PC 0)) 0x200))
  (write-reg 0xA 0xAA)
  (write-reg 0xB 0xAA)
  (evaluate 0x5AB0)
  (is (= (short->ushort (aget PC 0)) (+ 0x200 2))))

(deftest op-6xkk
  (println (:doc (meta #'opcode-6xkk)))
  (is (= (byte->ubyte (read-reg 0xA)) 0))
  (evaluate 0x6ACC)
  (is (= (byte->ubyte (read-reg 0xA)) 0xCC)))

(deftest op-7xkk
  (println (:doc (meta #'opcode-7xkk)))
  (is (= (byte->ubyte (read-reg 0xA)) 0x0))
  (write-reg 0xA 0x11)
  (evaluate 0x7A22)
  (is (= (byte->ubyte (read-reg 0xA)) 0x33))
  (reset)
  (write-reg 0xA 0xBB)
  (evaluate 0x7ACC)
  (is (= (byte->ubyte (read-reg 0xA)) 0x87)))

(deftest op-8xye
  (println (:doc (meta #'opcode-8xye)))
  (is (= (byte->ubyte (read-reg 0xF)) 0))
  (write-reg 0xA 2r10000000)
  (is (= (byte->ubyte (read-reg 0xA)) 0x80))
  (evaluate 0x8A0E)
  (is (= (byte->ubyte (read-reg 0xF)) 1))
  (is (= (byte->ubyte (read-reg 0xA)) 0))
  (evaluate 0x8A0E)
  (is (= (byte->ubyte (read-reg 0xF)) 0))
  (is (= (byte->ubyte (read-reg 0xA)) 0))
  (write-reg 0xA 2r10101010)
  (evaluate 0x8A0E)
  (is (= (byte->ubyte (read-reg 0xF)) 1))
  (is (= (byte->ubyte (read-reg 0xA)) 2r01010100)))

(deftest op-9xy0
  (println (:doc (meta #'opcode-9xy0)))
  (is (= (short->ushort (aget PC 0)) 0x200))
  (write-reg 0xA 0xA)
  (write-reg 0xB 0xB)
  (evaluate 0x9AB0)
  (is (= (short->ushort (aget PC 0)) (+ 0x200 2)))
  (reset)
  (write-reg 0xA 0xA)
  (write-reg 0xB 0xA)
  (evaluate 0x9AB0)
  (is (= (short->ushort (aget PC 0)) 0x200)))

(deftest op-annn
  (println (:doc (meta #'opcode-annn)))
  (is (= (short->ushort (read-reg :I)) 0))
  (evaluate 0xAFEA)
  (is (= (short->ushort (read-reg :I)) 0xFEA)))

(deftest op-bnnn
  (println (:doc (meta #'opcode-bnnn)))
  (is (= (short->ushort (aget PC 0)) 0x200))
  (write-reg 0 1)
  (evaluate 0xB201)
  (is (= (short->ushort (aget PC 0)) 0x202))
  (reset)
  (aset-short PC 0 (unchecked-short 0xFDFD))
  (write-reg 0 0xA)
  (evaluate 0xB201)
  (is (= (short->ushort (aget PC 0)) 0x20B)))

(deftest op-8xy0
  (println (:doc (meta #'opcode-8xy0)))
  (write-reg 0xB 0xB)
  (evaluate 0x8AB0)
  (is (= (byte->ubyte (read-reg 0xA)) 0xB)))

(deftest op-8xy1
  (println (:doc (meta #'opcode-8xy1)))
  (write-reg 0xA 0xAA)
  (write-reg 0x5 0x55)
  (evaluate 0x8A51)
  (is (= (byte->ubyte (read-reg 0xA)) 0xFF))
  (reset)
  (write-reg 0xA 0xF0)
  (write-reg 0x5 0x0F)
  (evaluate 0x8A51)
  (is (= (byte->ubyte (read-reg 0xA)) 0xFF)))

(deftest op-8xy2
  (println (:doc (meta #'opcode-8xy2)))
  (write-reg 0xA 0xAA)
  (write-reg 0x5 0x55)
  (evaluate 0x8A52)
  (is (= (byte->ubyte (read-reg 0xA)) 0x0))
  (reset)
  (write-reg 0xA 0xFF)
  (write-reg 0x5 0x0F)
  (evaluate 0x8A52)
  (is (= (byte->ubyte (read-reg 0xA)) 0x0F)))

(deftest op-8xy3
  (println (:doc (meta #'opcode-8xy3)))
  (write-reg 0xA 0xFF)
  (write-reg 0x5 0xF0)
  (evaluate 0x8A53)
  (is (= (byte->ubyte (read-reg 0xA)) 0xF))
  (reset)
  (write-reg 0xA 0xAA)
  (write-reg 0x5 0x55)
  (evaluate 0x8A53)
  (is (= (byte->ubyte (read-reg 0xA)) 0xFF)))

(deftest op-8xy4
  (println (:doc (meta #'opcode-8xy4)))
  (is (= (byte->ubyte (read-reg 0xF)) 0))
  (write-reg 0xA 0xFE)
  (write-reg 0xB 0x1)
  (evaluate 0x8AB4)
  (is (= (byte->ubyte (read-reg 0xF)) 0))
  (reset)
  (write-reg 0xA 0xFF)
  (write-reg 0xB 0x1)
  (evaluate 0x8AB4)
  (is (= (byte->ubyte (read-reg 0xF)) 1)))

(deftest op-8xy5
  (println (:doc (meta #'opcode-8xy5)))
  (is (= (byte->ubyte (read-reg 0xF)) 0))
  (write-reg 1 0xA)
  (write-reg 2 2)
  (evaluate 0x8125)
  (testing "VF is set to 1 when Vx > Vy"
    (is (= (byte->ubyte (read-reg 0xF)) 1)))
  (testing "Vx = Vx - Vy"
    (is (= (byte->ubyte (read-reg 0x1)) 8)))
  (reset)
  (write-reg 1 1)
  (write-reg 2 2)
  (evaluate 0x8125)
  (is (= (byte->ubyte (read-reg 0xF)) 0))
  (is (= (byte->ubyte (read-reg 0x1)) 0xFF)))

(deftest op-8xy6
  (println (:doc (meta #'opcode-8xy6)))
  (is (= (byte->ubyte (read-reg 0xF)) 0))
  (write-reg 1 2r10000001)
  (evaluate 0x8116)
  (testing "VF is set to 1 when least significant bit is 1"
    (is (= (byte->ubyte (read-reg 0xF)) 1)))
  (reset)
  (write-reg 1 2r10000000)
  (testing "VF is set to 0 when least significant bit is 0"
    (is (= (byte->ubyte (read-reg 0xF)) 0)))
  (reset)
  (write-reg 1 2r10000000)
  (evaluate 0x8116)
  (testing "Vx is divided by 2"
    (is (= (byte->ubyte (read-reg 1)) 2r01000000))))

(deftest op-8xy7
  (println (:doc (meta #'opcode-8xy7)))
  (is (= (byte->ubyte (read-reg 0xF)) 0))
  (write-reg 2 0xA)
  (write-reg 1 2)
  (evaluate 0x8127)
  (testing "VF is set to 1 when Vy > Vx"
    (is (= (byte->ubyte (read-reg 0xF)) 1)))
  (testing "Vx = Vy - Vx"
    (is (= (byte->ubyte (read-reg 0x1)) 8)))
  (reset)
  (write-reg 2 1)
  (write-reg 1 2)
  (evaluate 0x8127)
  (is (= (byte->ubyte (read-reg 0xF)) 0))
  (is (= (byte->ubyte (read-reg 0x1)) 0xFF)))

(deftest op-fx1e
  (println (:doc (meta #'opcode-fx1e)))
  (write-reg :I 0x1111)
  (write-reg 1 0x11)
  (evaluate 0xF11E)
  (is (= (short->ushort (read-reg :I)) 0x1122)))

(deftest op-fx33
  (println (:doc (meta #'opcode-fx33)))
  (is (= (short->ushort (read-reg :I)) 0))
  (write-reg :I 0xA)
  (write-reg 1 0xAC)
  (evaluate 0xF133)
  (is (= (byte->ubyte (read-mem (short->ushort (read-reg :I)))) 1))
  (is (= (byte->ubyte (read-mem (inc (short->ushort (read-reg :I))))) 7))
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 2))) 2)))

(deftest op-fx55
  (println (:doc (meta #'opcode-fx55)))
  (is (= (short->ushort (read-reg :I)) 0))
  (write-reg :I 0xA)
  (write-reg 0 0)
  (write-reg 1 1)
  (write-reg 2 2)
  (write-reg 3 3)
  (write-reg 4 4)
  (write-reg 5 5)
  (write-reg 6 6)
  (write-reg 7 7)
  (write-reg 8 8)
  (write-reg 9 9)
  (write-reg 0xA 0xA)
  (write-reg 0xB 0xB)
  (write-reg 0xC 0xC)
  (write-reg 0xD 0xD)
  (write-reg 0xE 0xE)
  (evaluate 0xfe55)
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 0))) 0))
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 1))) 1))
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 2))) 2))
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 3))) 3))
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 4))) 4))
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 5))) 5))
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 6))) 6))
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 7))) 7))
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 8))) 8))
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 9))) 9))
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 0xA))) 0xA))
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 0xB))) 0xB))
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 0xC))) 0xC))
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 0xD))) 0xD))
  (is (= (byte->ubyte (read-mem (+ (short->ushort (read-reg :I)) 0xE))) 0xE)))

(deftest op-fx65
  (println (:doc (meta #'opcode-fx65)))
  (is (= (short->ushort (read-reg :I)) 0))
  (write-reg :I 0)
  (write-mem 0 0)
  (write-mem 1 1)
  (write-mem 2 2)
  (write-mem 3 3)
  (write-mem 4 4)
  (write-mem 5 5)
  (write-mem 6 6)
  (write-mem 7 7)
  (write-mem 8 8)
  (write-mem 9 9)
  (write-mem 0xA 0xA)
  (write-mem 0xB 0xB)
  (write-mem 0xC 0xC)
  (write-mem 0xD 0xD)
  (write-mem 0xE 0xE)
  (evaluate 0xfe65)
  (is (= (byte->ubyte (read-reg 0)) (read-mem (+ (short->ushort (read-reg :I)) 0))))
  (is (= (byte->ubyte (read-reg 1)) (read-mem (+ (short->ushort (read-reg :I)) 1))))
  (is (= (byte->ubyte (read-reg 2)) (read-mem (+ (short->ushort (read-reg :I)) 2))))
  (is (= (byte->ubyte (read-reg 3)) (read-mem (+ (short->ushort (read-reg :I)) 3))))
  (is (= (byte->ubyte (read-reg 4)) (read-mem (+ (short->ushort (read-reg :I)) 4))))
  (is (= (byte->ubyte (read-reg 5)) (read-mem (+ (short->ushort (read-reg :I)) 5))))
  (is (= (byte->ubyte (read-reg 6)) (read-mem (+ (short->ushort (read-reg :I)) 6))))
  (is (= (byte->ubyte (read-reg 7)) (read-mem (+ (short->ushort (read-reg :I)) 7))))
  (is (= (byte->ubyte (read-reg 8)) (read-mem (+ (short->ushort (read-reg :I)) 8))))
  (is (= (byte->ubyte (read-reg 9)) (read-mem (+ (short->ushort (read-reg :I)) 9))))
  (is (= (byte->ubyte (read-reg 0xA)) (read-mem (+ (short->ushort (read-reg :I)) 0xA))))
  (is (= (byte->ubyte (read-reg 0xB)) (read-mem (+ (short->ushort (read-reg :I)) 0xB))))
  (is (= (byte->ubyte (read-reg 0xC)) (read-mem (+ (short->ushort (read-reg :I)) 0xC))))
  (is (= (byte->ubyte (read-reg 0xD)) (read-mem (+ (short->ushort (read-reg :I)) 0xD))))
  (is (= (byte->ubyte (read-reg 0xE)) (read-mem (+ (short->ushort (read-reg :I)) 0xE)))))
