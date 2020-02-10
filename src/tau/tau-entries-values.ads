with Tau.Values;

private package Tau.Entries.Values is

   type Value_Entry_Record is abstract new Root_Tau_Entry
   with null record;

   overriding function Is_Value_Entry
     (Object : Value_Entry_Record)
      return Boolean
   is (True);

   function Value_Entry
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Entry_Value : Tau.Values.Tau_Value)
      return Value_Entry_Record'Class;

   function Formal_Argument_Entry
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Entry_Type  : Tau.Types.Tau_Type)
      return Value_Entry_Record'Class;

   function Global_Entry
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Entry_Type  : Tau.Types.Tau_Type;
      Qualifier   : Rho.Storage_Qualifier)
      return Value_Entry_Record'Class;

end Tau.Entries.Values;
