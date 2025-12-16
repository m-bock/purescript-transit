
```mermaid
---
  config:
    themeVariables:
        xyChart:
            plotColorPalette: "#ff3456, #00ff00, #0000ff, #ffff00, #ff00ff, #00ffff"
---
xychart
  title "Update Functions"
  x-axis "Input Size" [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
  y-axis "Time (in ms)" 0 --> 0.05
  line [0.00638, 0.00586, 0.00644, 0.00851, 0.0101, 0.01102, 0.0111, 0.0134, 0.01627, 0.02063, 0.02128, 0.02237, 0.02568, 0.02819, 0.02958, 0.03083, 0.03362, 0.04161, 0.04285]
  line [0.00143, 0.00131, 0.00129, 0.00132, 0.00158, 0.00142, 0.00138, 0.00136, 0.00135, 0.00185, 0.00143, 0.00137, 0.00141, 0.00136, 0.0017, 0.00161, 0.00138, 0.00144, 0.00145]
  line [0.002, 0.00224, 0.00214, 0.00262, 0.00255, 0.00317, 0.00337, 0.00342, 0.00359, 0.00388, 0.00406, 0.00406, 0.00409, 0.0047, 0.00484, 0.00512, 0.00484, 0.00534, 0.00592]
```
![ff3456](https://placehold.co/8x8/ff3456/ff3456.png) update (Transit with ADTs)&nbsp;&nbsp;![00ff00](https://placehold.co/8x8/00ff00/00ff00.png) updateClassic&nbsp;&nbsp;![0000ff](https://placehold.co/8x8/0000ff/0000ff.png) updateV (Transit with Variants)