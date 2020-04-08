(rule
 (targets option-none.1.dot
          option-none.2.dot
          option-some.1.dot
          option-some.2.dot
          pair.1.dot
          state.1.dot
          v1.1.dot
          v1.2.dot
          v1.3.dot
          v1c.1.dot
          v1c.2.dot
          v1c.3.dot
          v2.1.dot
          v2.2.dot
          v2.3.dot
          v2.4.dot
          v3.1.dot
          v3.2.dot
          v3.3.dot
          v4.1.dot
          v4.2.dot
          v4.3.dot
          v5.1.dot
          v5.2.dot
          v5.3.dot
          v5n.1.dot
          v5n.2.dot
          v5n.3.dot)
 (action (run ./test.exe)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/option-none.1.dot option-none.1.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/option-none.2.dot option-none.2.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/option-some.1.dot option-some.1.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/option-some.2.dot option-some.2.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/pair.1.dot pair.1.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/state.1.dot state.1.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v1.1.dot v1.1.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v1.2.dot v1.2.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v1.3.dot v1.3.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v1c.1.dot v1c.1.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v1c.2.dot v1c.2.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v1c.3.dot v1c.3.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v2.1.dot v2.1.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v2.2.dot v2.2.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v2.3.dot v2.3.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v2.4.dot v2.4.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v3.1.dot v3.1.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v3.2.dot v3.2.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v3.3.dot v3.3.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v4.1.dot v4.1.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v4.2.dot v4.2.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v4.3.dot v4.3.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v5.1.dot v5.1.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v5.2.dot v5.2.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v5.3.dot v5.3.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v5n.1.dot v5n.1.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v5n.2.dot v5n.2.dot)))

(rule
 (alias runtest)
 (package current)
 (action (diff expected/v5n.3.dot v5n.3.dot)))
