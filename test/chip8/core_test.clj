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

(deftest memory-initialize
  (testing "Memory is initialized to 0"
    (is (= (reduce + memory) 0))))

(deftest memory-read-write
  (testing "Reads and Writes works as expected"
    (write-mem 0x0000 1)
    (is (= (read-mem 0x0000) 1)))
  (testing "Read a byte"
    (write-mem 0x0001 0xFF)
    (is (= (byte->ubyte (read-mem 0x0001)) 0xFF))))

(deftest register-read-write
  (testing "Read and write to an existing register works"
    (write-reg :V8 0xFF)
    (write-reg :I 0xFFFF)
    (is (= (byte->ubyte (read-reg :V8)) 0xFF)))
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
  (write-mem 0x1FA 19)
  (is (= (read-mem 0x1FA) 19))
  (evaluate 0x00E0)
  (is (= (read-mem 0x1FA) 0)))

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
