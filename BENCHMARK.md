# Benchmark: 0.1.0 against 0.1.1

From `spago build && node bench.mjs --limit 30` on 2026-11-21 (an M-series MacBook Pro, 16 GB, node 22), then
`node bench.mjs --limit 180 --sizes 12000 --measure collapse`. One measurement to a process; the flow is made before
the clock starts. "0.1.0" is `test/Test/Flow/Reference.purs`, the old functions word for word. `> 30 s`: stopped at
the limit; `(not run)`: a smaller size of the same case had already passed the limit. `faults-*` is `projectionFaults`.

| function | flow | wires | steps | 0.1.0 (ms) | now (ms) |
|---|---|---:|---:|---:|---:|
| problems | chain | 1,000 | 500 | 69 | 24 |
| problems | chain | 10,000 | 5,000 | 4,668 | 133 |
| problems | chain | 50,000 | 25,000 | > 30 s | 529 |
| problems | fan-outs | 1,000 | 500 | 64 | 22 |
| problems | fan-outs | 10,000 | 5,000 | 4,566 | 134 |
| problems | fan-outs | 50,000 | 25,000 | > 30 s | 536 |
| problems | modules | 798 | 108 | 30 | 15 |
| problems | modules | 9,456 | 1,086 | 2,673 | 92 |
| problems | modules | 48,133 | 5,434 | > 30 s | 449 |
| project-Holders | chain | 1,000 | 500 | 17 | 13 |
| project-Holders | chain | 10,000 | 5,000 | 1,182 | 67 |
| project-Holders | chain | 50,000 | 25,000 | > 30 s | 182 |
| project-Holders | fan-outs | 1,000 | 500 | 17 | 12 |
| project-Holders | fan-outs | 10,000 | 5,000 | 1,163 | 68 |
| project-Holders | fan-outs | 50,000 | 25,000 | > 30 s | 186 |
| project-Holders | modules | 798 | 108 | 6.4 | 6.7 |
| project-Holders | modules | 9,456 | 1,086 | 305 | 46 |
| project-Holders | modules | 48,133 | 5,434 | 7,943 | 120 |
| project-Steps | chain | 1,000 | 500 | 15 | 13 |
| project-Steps | chain | 10,000 | 5,000 | 1,015 | 68 |
| project-Steps | chain | 50,000 | 25,000 | 27,212 | 186 |
| project-Steps | fan-outs | 1,000 | 500 | 14 | 11 |
| project-Steps | fan-outs | 10,000 | 5,000 | 1,015 | 65 |
| project-Steps | fan-outs | 50,000 | 25,000 | 26,475 | 182 |
| project-Steps | modules | 798 | 108 | 17 | 8.1 |
| project-Steps | modules | 9,456 | 1,086 | 1,648 | 54 |
| project-Steps | modules | 48,133 | 5,434 | > 30 s | 141 |
| project-Both | chain | 1,000 | 500 | 2.5 | 11 |
| project-Both | chain | 10,000 | 5,000 | 23 | 61 |
| project-Both | chain | 50,000 | 25,000 | 41 | 184 |
| project-Both | fan-outs | 1,000 | 500 | 2.6 | 12 |
| project-Both | fan-outs | 10,000 | 5,000 | 25 | 61 |
| project-Both | fan-outs | 50,000 | 25,000 | 39 | 175 |
| project-Both | modules | 798 | 108 | 1.4 | 5.7 |
| project-Both | modules | 9,456 | 1,086 | 9.4 | 38 |
| project-Both | modules | 48,133 | 5,434 | 30 | 106 |
| project-Chosen | chain | 1,000 | 500 | 13 | 13 |
| project-Chosen | chain | 10,000 | 5,000 | 625 | 70 |
| project-Chosen | chain | 50,000 | 25,000 | 16,311 | 202 |
| project-Chosen | fan-outs | 1,000 | 500 | 13 | 15 |
| project-Chosen | fan-outs | 10,000 | 5,000 | 627 | 71 |
| project-Chosen | fan-outs | 50,000 | 25,000 | 15,634 | 218 |
| project-Chosen | modules | 798 | 108 | 6.1 | 7.1 |
| project-Chosen | modules | 9,456 | 1,086 | 170 | 42 |
| project-Chosen | modules | 48,133 | 5,434 | 4,024 | 129 |
| layers-Steps | chain | 1,000 | 500 | 34 | 21 |
| layers-Steps | chain | 10,000 | 5,000 | 2,209 | 122 |
| layers-Steps | chain | 50,000 | 25,000 | > 30 s | 596 |
| layers-Steps | fan-outs | 1,000 | 500 | 34 | 20 |
| layers-Steps | fan-outs | 10,000 | 5,000 | 2,293 | 120 |
| layers-Steps | fan-outs | 50,000 | 25,000 | > 30 s | 512 |
| layers-Steps | modules | 798 | 108 | 12 | 13 |
| layers-Steps | modules | 9,456 | 1,086 | 692 | 87 |
| layers-Steps | modules | 48,133 | 5,434 | 20,133 | 436 |
| faults-Holders | chain | 1,000 | 500 | 155 | 53 |
| faults-Holders | chain | 10,000 | 5,000 | 12,409 | 298 |
| faults-Holders | chain | 50,000 | 25,000 | > 30 s | 1,249 |
| faults-Holders | fan-outs | 1,000 | 500 | 166 | 149 |
| faults-Holders | fan-outs | 10,000 | 5,000 | 12,675 | 289 |
| faults-Holders | fan-outs | 50,000 | 25,000 | > 30 s | 1,266 |
| faults-Holders | modules | 798 | 108 | 171 | 60 |
| faults-Holders | modules | 9,456 | 1,086 | 18,497 | 370 |
| faults-Holders | modules | 48,133 | 5,434 | > 30 s | 2,198 |
| faults-Steps | chain | 1,000 | 500 | 155 | 53 |
| faults-Steps | chain | 10,000 | 5,000 | 12,454 | 295 |
| faults-Steps | chain | 50,000 | 25,000 | > 30 s | 1,204 |
| faults-Steps | fan-outs | 1,000 | 500 | 137 | 49 |
| faults-Steps | fan-outs | 10,000 | 5,000 | 11,918 | 288 |
| faults-Steps | fan-outs | 50,000 | 25,000 | > 30 s | 1,217 |
| faults-Steps | modules | 798 | 108 | 178 | 53 |
| faults-Steps | modules | 9,456 | 1,086 | 21,583 | 380 |
| faults-Steps | modules | 48,133 | 5,434 | > 30 s | 2,513 |
| faults-Both | chain | 1,000 | 500 | 219 | 62 |
| faults-Both | chain | 10,000 | 5,000 | 19,074 | 343 |
| faults-Both | chain | 50,000 | 25,000 | > 30 s | 1,502 |
| faults-Both | fan-outs | 1,000 | 500 | 216 | 61 |
| faults-Both | fan-outs | 10,000 | 5,000 | 19,625 | 338 |
| faults-Both | fan-outs | 50,000 | 25,000 | > 30 s | 1,576 |
| faults-Both | modules | 798 | 108 | 153 | 47 |
| faults-Both | modules | 9,456 | 1,086 | 17,566 | 330 |
| faults-Both | modules | 48,133 | 5,434 | > 30 s | 2,249 |
| collapse | chain | 1,000 | 500 | > 30 s | 33 |
| collapse | chain | 10,000 | 5,000 | (not run) | 176 |
| collapse | chain | 50,000 | 25,000 | (not run) | 747 |
| collapse | fan-outs | 1,000 | 500 | 1,074 | 49 |
| collapse | fan-outs | 10,000 | 5,000 | > 30 s | 305 |
| collapse | fan-outs | 50,000 | 25,000 | (not run) | 1,326 |
| collapse | modules | 798 | 108 | 720 | 43 |
| collapse | modules | 9,456 | 1,086 | > 30 s | 256 |
| collapse | modules | 48,133 | 5,434 | (not run) | 1,268 |
| function | flow | wires | steps | 0.1.0 (ms) | now (ms) |
|---|---|---:|---:|---:|---:|
| collapse | chain | 12,000 | 6,000 | > 180 s | 203 |
| collapse | fan-outs | 12,000 | 6,000 | 152,519 | 350 |
| collapse | modules | 11,392 | 1,304 | 146,414 | 314 |

## 0.2.0: groups that hold items

From `spago build && node bench.mjs --only library --limit 60` on 2026-11-21, the same machine, under more load
than the run above (every function, `collapse` aside, is unchanged in 0.2.0 and came out up to a third slower than
above). So `collapse` is set against 0.1.1's `collapse` timed in the same run: `collapse-0.1.1` is
`test/Test/Flow/Reference011.purs`, 0.1.1's word for word; `collapse` is 0.2.0's on the same grouping of steps alone;
`collapse-holding` is 0.2.0's with every item held by the group of the step that writes it.

| function | flow | wires | steps | ms |
|---|---|---:|---:|---:|
| collapse | chain | 1,000 | 500 | 40 |
| collapse | chain | 10,000 | 5,000 | 233 |
| collapse | chain | 50,000 | 25,000 | 999 |
| collapse | fan-outs | 1,000 | 500 | 57 |
| collapse | fan-outs | 10,000 | 5,000 | 354 |
| collapse | fan-outs | 50,000 | 25,000 | 1,740 |
| collapse | modules | 798 | 108 | 47 |
| collapse | modules | 9,456 | 1,086 | 344 |
| collapse | modules | 48,133 | 5,434 | 1,736 |
| collapse-0.1.1 | chain | 1,000 | 500 | 39 |
| collapse-0.1.1 | chain | 10,000 | 5,000 | 205 |
| collapse-0.1.1 | chain | 50,000 | 25,000 | 909 |
| collapse-0.1.1 | fan-outs | 1,000 | 500 | 56 |
| collapse-0.1.1 | fan-outs | 10,000 | 5,000 | 343 |
| collapse-0.1.1 | fan-outs | 50,000 | 25,000 | 1,639 |
| collapse-0.1.1 | modules | 798 | 108 | 45 |
| collapse-0.1.1 | modules | 9,456 | 1,086 | 341 |
| collapse-0.1.1 | modules | 48,133 | 5,434 | 1,811 |
| collapse-holding | chain | 1,000 | 500 | 42 |
| collapse-holding | chain | 10,000 | 5,000 | 230 |
| collapse-holding | chain | 50,000 | 25,000 | 1,064 |
| collapse-holding | fan-outs | 1,000 | 500 | 54 |
| collapse-holding | fan-outs | 10,000 | 5,000 | 321 |
| collapse-holding | fan-outs | 50,000 | 25,000 | 1,557 |
| collapse-holding | modules | 798 | 108 | 44 |
| collapse-holding | modules | 9,456 | 1,086 | 320 |
| collapse-holding | modules | 48,133 | 5,434 | 1,810 |
