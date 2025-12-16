
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
  y-axis "Time (in ms)" 0 --> 0.025
  line [0.0151, 0.019, 0.0297, 0.0437]
  line [0.0008, 0.0009, 0.0013, 0.0015]
  line [0.0035, 0.004, 0.006, 0.0062]
```
![ff3456](https://placehold.co/8x8/ff3456/ff3456.png) update (Transit with ADTs)&nbsp;&nbsp;![00ff00](https://placehold.co/8x8/00ff00/00ff00.png) updateClassic&nbsp;&nbsp;![0000ff](https://placehold.co/8x8/0000ff/0000ff.png) updateV (Transit with Variants)