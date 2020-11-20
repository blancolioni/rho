package body Tau.Objects is

   function To_Link_Name
     (Object_Name : String)
      return String;

   ----------------------
   -- Initial_Property --
   ----------------------

   procedure Initial_Property
     (Object        : in out Root_Tau_Object'Class;
      Name          : String;
      Initial_Value : String := "")
   is
   begin
      Object.Properties.Insert (Name, Initial_Value);
   end Initial_Property;

   -----------------------
   -- Initialize_Object --
   -----------------------

   procedure Initialize_Object
     (Object      : in out Root_Tau_Object'Class;
      Declaration : GCS.Positions.File_Position;
      Name        : String)
   is
   begin
      Object.Initialize_Node (Declaration);
      Object.Object_Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Object.Object_Link_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (To_Link_Name (Name));
   end Initialize_Object;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : in out Root_Tau_Object'Class;
      Name   : String;
      Value  : String)
   is
   begin
      Object.Properties.Replace (Name, Value);
   end Set_Property;

   ------------------
   -- To_Link_Name --
   ------------------

   function To_Link_Name
     (Object_Name : String)
      return String
   is
      Result : String (1 .. Object_Name'Length * 2);
      Last   : Natural := 0;
   begin
      for Ch of Object_Name loop
         if Ch = '.' then
            Last := Last + 1;
            Result (Last) := '_';
            Last := Last + 1;
            Result (Last) := '_';
         else
            Last := Last + 1;
            Result (Last) := Ch;
         end if;
      end loop;
      return Result (1 .. Last);
   end To_Link_Name;

end Tau.Objects;
