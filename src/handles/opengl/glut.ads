with Interfaces.C.Strings;

package GLUT is

   --  Display mode bit masks.

   RGB                 : constant := 0;
   RGBA                : constant := 0;
   INDEX               : constant := 1;
   SINGLE              : constant := 0;
   DOUBLE              : constant := 2;
   ACCUM               : constant := 4;
   ALPHA               : constant := 8;
   DEPTH               : constant := 16;
   STENCIL             : constant := 32;
   MULTISAMPLE         : constant := 128;
   STEREO              : constant := 256;
   LUMINANCE           : constant := 512;

   --  Mouse buttons.

   LEFT_BUTTON         : constant := 0;
   MIDDLE_BUTTON       : constant := 1;
   RIGHT_BUTTON        : constant := 2;
   SCROLL_UP_BUTTON    : constant := 3;
   SCROLL_DOWN_BUTTON  : constant := 4;

   subtype Mouse_Buttons is Integer range 0 .. 2;
   subtype Mouse_Wheel_Buttons is Integer range 3 .. 4;

   --  Mouse button callback state.

   DOWN                : constant := 0;
   UP                  : constant := 1;

   --  function keys

   KEY_F1              : constant := 1;
   KEY_F2              : constant := 2;
   KEY_F3              : constant := 3;
   KEY_F4              : constant := 4;
   KEY_F5              : constant := 5;
   KEY_F6              : constant := 6;
   KEY_F7              : constant := 7;
   KEY_F8              : constant := 8;
   KEY_F9              : constant := 9;
   KEY_F10             : constant := 10;
   KEY_F11             : constant := 11;
   KEY_F12             : constant := 12;

   --  directional keys

   KEY_LEFT            : constant := 100;
   KEY_UP              : constant := 101;
   KEY_RIGHT           : constant := 102;
   KEY_DOWN            : constant := 103;
   KEY_PAGE_UP         : constant := 104;
   KEY_PAGE_DOWN       : constant := 105;
   KEY_HOME            : constant := 106;
   KEY_END             : constant := 107;
   KEY_INSERT          : constant := 108;

   --  Entry/exit callback state.

   LEFT                : constant := 0;
   ENTERED             : constant := 1;

   --  Menu usage callback state.

   MENU_NOT_IN_USE     : constant := 0;
   MENU_IN_USE         : constant := 1;

   --  Visibility callback state.

   NOT_VISIBLE         : constant := 0;
   VISIBLE             : constant := 1;

   --  Window status callback state.

   HIDDEN              : constant := 0;
   FULLY_RETAINED      : constant := 1;
   PARTIALLY_RETAINED  : constant := 2;
   FULLY_COVERED       : constant := 3;

   --  Color index component selection values.

   RED                 : constant := 0;
   GREEN               : constant := 1;
   BLUE                : constant := 2;

   --  glutGameModeGet

   GAME_MODE_ACTIVE          : constant := 0;
   GAME_MODE_POSSIBLE        : constant := 1;
   GAME_MODE_WIDTH           : constant := 2;
   GAME_MODE_HEIGHT          : constant := 3;
   GAME_MODE_PIXEL_DEPTH     : constant := 4;
   GAME_MODE_REFRESH_RATE    : constant := 5;
   GAME_MODE_DISPLAY_CHANGED : constant := 6;

   --  glutSetKeyRepeat modes

   KEY_REPEAT_OFF            : constant := 0;
   KEY_REPEAT_ON             : constant := 1;
   KEY_REPEAT_DEFAULT        : constant := 2;

   --  Joystick button masks

   JOYSTICK_BUTTON_A          : constant := 1;
   JOYSTICK_BUTTON_B          : constant := 2;
   JOYSTICK_BUTTON_C          : constant := 4;
   JOYSTICK_BUTTON_D          : constant := 8;

   --  Compatibility profiles
   CORE_PROFILE          : constant := 1;
   COMPATIBILITY_PROFILE : constant := 2;

   subtype Key_Type is Interfaces.C.unsigned_char;

   procedure Init;

   procedure Init_Display_Mode (Mode : Interfaces.C.unsigned);
   pragma Import (StdCall, Init_Display_Mode, "glutInitDisplayMode");

   procedure Init_Context_Version (Major, Minor : Interfaces.C.int);
   pragma Import (StdCall, Init_Context_Version, "glutInitContextVersion");

   procedure Init_Context_Profile (Profile : Integer);
   pragma Import (StdCall, Init_Context_Profile, "glutInitContextProfile");

   procedure Init_Window_Size
     (Width  : Integer;
      Height : Integer);
   pragma Import (StdCall, Init_Window_Size, "glutInitWindowSize");

   procedure Init_Window_Position
     (X, Y  : Integer);
   pragma Import (StdCall, Init_Window_Position, "glutInitWindowPosition");

   procedure Swap_Buffers;
   pragma Import (StdCall, Swap_Buffers, "glutSwapBuffers");

   procedure Post_Redisplay;
   pragma Import (StdCall, Post_Redisplay, "glutPostRedisplay");

   procedure Full_Screen;
   pragma Import (StdCall, Full_Screen, "glutFullScreen");

   procedure Main_Loop;
   pragma Import (StdCall, Main_Loop, "glutMainLoop");

   procedure Leave_Main_Loop;
   pragma Import (StdCall, Leave_Main_Loop, "glutLeaveMainLoop");

   function Create_Window
     (Title : Interfaces.C.Strings.chars_ptr)
      return Integer;
   pragma Import (StdCall, Create_Window, "glutCreateWindow");

   function Create_Window (Title : String) return Integer
   is (Create_Window (Interfaces.C.Strings.New_String (Title)));

   type Display_Callback is access procedure;
   --  pragma Convention (C, Display_Callback);
   procedure Display_Function (Callback : Display_Callback);
   pragma Import (StdCall, Display_Function, "glutDisplayFunc");

   type Reshape_Callback is access
     procedure (W, H : Integer);
--     pragma Convention (StdCall, Reshape_Callback);
   procedure Reshape_Function (Callback : Reshape_Callback);
   pragma Import (StdCall, Reshape_Function, "glutReshapeFunc");

   type Mouse_Callback is access
     procedure (Button : Integer;
                State  : Integer;
                X      : Integer;
                Y      : Integer);
--     pragma Convention (StdCall, Mouse_Callback);

   procedure Mouse_Function (Callback : Mouse_Callback);
   pragma Import (StdCall, Mouse_Function, "glutMouseFunc");

   type Motion_Callback is access
     procedure (X, Y : Integer);
--     pragma Convention (StdCall, Motion_Callback);

   procedure Motion_Function (Callback : Motion_Callback);
   pragma Import (StdCall, Motion_Function, "glutMotionFunc");

   procedure Passive_Motion_Function (Callback : Motion_Callback);
   pragma Import (StdCall, Passive_Motion_Function, "glutPassiveMotionFunc");

   type Keyboard_Callback is access
     procedure (Key : Key_Type;
                X, Y : Integer);
--     pragma Convention (StdCall, Keyboard_Callback);

   procedure Keyboard_Function (Callback : Keyboard_Callback);
   pragma Import (StdCall, Keyboard_Function, "glutKeyboardFunc");
   procedure Keyboard_Up_Function (Callback : Keyboard_Callback);
   pragma Import (StdCall, Keyboard_Up_Function, "glutKeyboardUpFunc");

   type Special_Key_Callback is access
     procedure (Key, X, Y : Integer);
--     pragma Convention (StdCall, Special_Key_Callback);

   procedure Special_Function (Callback : Special_Key_Callback);
   pragma Import (StdCall, Special_Function, "glutSpecialFunc");

   type Idle_Callback is access procedure;
--     pragma Convention (StdCall, Idle_Callback);

   procedure Idle_Function (Callback : Idle_Callback);
   pragma Import (StdCall, Idle_Function, "glutIdleFunc");

end GLUT;
