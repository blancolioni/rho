package Rho.Devices.Keyboard is

   type Key_Type is range 0 .. 65535;

   subtype Character_Key is Key_Type range 32 .. 126;

   Home  : constant Key_Type := 16#FF50#;
   Left  : constant Key_Type := 16#FF51#;
   Up    : constant Key_Type := 16#FF52#;
   Right : constant Key_Type := 16#FF53#;
   Down  : constant Key_Type := 16#FF54#;
   F1    : constant Key_Type := 16#FFBE#;
   F2    : constant Key_Type := 16#FFBF#;
   F3    : constant Key_Type := 16#FFC0#;
   F4    : constant Key_Type := 16#FFC1#;
   F5    : constant Key_Type := 16#FFC2#;
   F6    : constant Key_Type := 16#FFC3#;
   F7    : constant Key_Type := 16#FFC4#;
   F8    : constant Key_Type := 16#FFC5#;
   F9    : constant Key_Type := 16#FFC6#;
   F10   : constant Key_Type := 16#FFC7#;
   F11   : constant Key_Type := 16#FFC8#;
   F12   : constant Key_Type := 16#FFC9#;

end Rho.Devices.Keyboard;
