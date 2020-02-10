package body Tau.Objects is

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
   end Initialize_Object;

end Tau.Objects;
