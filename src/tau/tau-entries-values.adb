package body Tau.Entries.Values is

   type Constant_Entry_Record is
     new Value_Entry_Record with
      record
         Value : Tau.Values.Tau_Value;
      end record;

   overriding function To_Source
     (Object    : Constant_Entry_Record;
      Generator : in out Tau.Objects.Generator_Interface'Class)
      return String;

   type Formal_Argument_Record is
     new Value_Entry_Record with
      record
         null;
      end record;

   type Global_Entry_Record is
     new Value_Entry_Record with
      record
         Qualifier : Rho.Storage_Qualifier;
      end record;

   overriding function To_Source
     (Object    : Global_Entry_Record;
      Generator : in out Tau.Objects.Generator_Interface'Class)
      return String;

   ---------------------------
   -- Formal_Argument_Entry --
   ---------------------------

   function Formal_Argument_Entry
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Entry_Type  : Tau.Types.Tau_Type)
      return Value_Entry_Record'Class
   is
      Result : Formal_Argument_Record := Formal_Argument_Record'
        (Tau.Objects.Root_Tau_Object with
         Typ       => Entry_Type);
   begin
      Result.Initialize_Object (Declaration, Name);
      return Result;
   end Formal_Argument_Entry;

   ------------------
   -- Global_Entry --
   ------------------

   function Global_Entry
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Entry_Type  : Tau.Types.Tau_Type;
      Qualifier   : Rho.Storage_Qualifier)
      return Value_Entry_Record'Class
   is
      Result : Global_Entry_Record := Global_Entry_Record'
        (Tau.Objects.Root_Tau_Object with
         Typ       => Entry_Type,
         Qualifier => Qualifier);
   begin
      Result.Initialize_Object (Declaration, Name);
      return Result;
   end Global_Entry;

   ---------------
   -- To_Source --
   ---------------

   overriding function To_Source
     (Object    : Constant_Entry_Record;
      Generator : in out Tau.Objects.Generator_Interface'Class)
      return String
   is
   begin
      if Object.Value.Has_Source_Text then
         return Object.Value.To_Source (Generator);
      else
         return Object.Name;
      end if;
   end To_Source;

   ---------------
   -- To_Source --
   ---------------

   overriding function To_Source
     (Object    : Global_Entry_Record;
      Generator : in out Tau.Objects.Generator_Interface'Class)
      return String
   is
      use all type Rho.Shader_Stage;
   begin
      return
        (if Generator.Current_Stage = Vertex_Shader
         then ""
         else Stage_Name (Generator.Current_Stage) & "_")
        & Object.Name;
   end To_Source;

   -----------------
   -- Value_Entry --
   -----------------

   function Value_Entry
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Entry_Value : Tau.Values.Tau_Value)
      return Value_Entry_Record'Class
   is
      Result : Constant_Entry_Record := Constant_Entry_Record'
        (Tau.Objects.Root_Tau_Object with
         Typ   => Entry_Value.Value_Type,
         Value => Entry_Value);
   begin
      Result.Initialize_Object (Declaration, Name);
      return Result;
   end Value_Entry;

end Tau.Entries.Values;
