| State      |       | Message |       | Guard        |       | State      |
| ---------- | ----- | ------- | ----- | ------------ | ----- | ---------- |
| DoorOpen   | **⟶** | Close   |       |              | **⟶** | DoorClosed |
| DoorClosed | **⟶** | Open    |       |              | **⟶** | DoorOpen   |
| DoorClosed | **⟶** | Lock    |       |              | **⟶** | DoorLocked |
| DoorLocked | **⟶** | Unlock  | **?** | PinIncorrect | **⟶** | DoorLocked |
| DoorLocked | **⟶** | Unlock  | **?** | PinCorrect   | **⟶** | DoorClosed |
