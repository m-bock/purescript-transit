
```mermaid
---
  config:
    themeVariables:
        xyChart:
            plotColorPalette: "#ff3456, #00ff00, #0000ff, #ffff00, #ff00ff, #00ffff"
---
xychart
  title "Update Functions"
  x-axis "Input Size" [5, 10, 15, 20]
  y-axis "Time (in ms)" 0 --> 0.05
  line [0.0029, 0.0011, 0.0008, 0.0012]
  line [0.0006, 0.0012, 0.0011, 0.0004]
  line [0.0005, 0.0008, 0.0009, 0.0007]
```
![ff3456](https://placehold.co/8x8/ff3456/ff3456.png) update (Transit with ADTs)&nbsp;&nbsp;![00ff00](https://placehold.co/8x8/00ff00/00ff00.png) updateClassic&nbsp;&nbsp;![0000ff](https://placehold.co/8x8/0000ff/0000ff.png) updateV (Transit with Variants)