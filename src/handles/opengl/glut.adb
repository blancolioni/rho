with Ada.Command_Line;

package body GLUT is

   procedure Glutinit (Argcp : access Integer;
      Argv : access Interfaces.C.Strings.chars_ptr);
   --  pragma Import (C, Glutinit, "glutInit", "glutInit"); -- APEX
   pragma Import (StdCall, Glutinit, "glutInit"); -- GNAT/OA

   --  Pure Ada method, from IBM / Rational Apex support:

   --  "This procedure may be a useful replacement when porting an
   --  Ada program written for Gnat, which imports argc and argv like this:
   --  argc : aliased integer;
   --  pragma Import (C, argc, "gnat_argc");
   --
   --  argv : chars_ptr_ptr;
   --  pragma Import (C, argv, "gnat_argv");
   --  "

   --  http://www-1.ibm.com/support/docview.wss?uid=swg21125019

   ----------
   -- Init --
   ----------

   procedure Init is
      use Ada.Command_Line;
      use Interfaces.C.Strings;

      Argc : aliased Integer := 1;
      Argv : aliased Interfaces.C.Strings.chars_ptr :=
               New_String (Command_Name);
   begin
      Glutinit (Argc'Access, Argv'Access);
   end Init;

end GLUT;
