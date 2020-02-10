with Tau.Entries.Values;

package body Tau.Entries is

   type Type_Entry_Record is new Root_Tau_Entry with null record;

   overriding function Is_Type_Entry (Item : Type_Entry_Record) return Boolean
   is (True);

   ---------------------------
   -- Formal_Argument_Entry --
   ---------------------------

   function Formal_Argument_Entry
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Entry_Type  : Tau.Types.Tau_Type)
      return Tau_Entry
   is
   begin
      return new Tau.Entries.Values.Value_Entry_Record'Class'
        (Values.Formal_Argument_Entry
           (Declaration, Name, Entry_Type));
   end Formal_Argument_Entry;

   ------------------
   -- Global_Entry --
   ------------------

   function Global_Entry
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Entry_Type  : Tau.Types.Tau_Type;
      Qualifier   : Rho.Storage_Qualifier)
      return Tau_Entry
   is
   begin
      return new Tau.Entries.Values.Value_Entry_Record'Class'
        (Values.Global_Entry (Declaration, Name, Entry_Type, Qualifier));
   end Global_Entry;

   ----------------
   -- Type_Entry --
   ----------------

   function Type_Entry
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Entry_Type  : Tau.Types.Tau_Type)
      return Tau_Entry
   is
      Result : Type_Entry_Record := Type_Entry_Record'
        (Tau.Objects.Root_Tau_Object with
         Typ => Entry_Type);
   begin
      Result.Initialize_Object (Declaration, Name);
      return new Type_Entry_Record'(Result);
   end Type_Entry;

   -----------------
   -- Value_Entry --
   -----------------

   function Value_Entry
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Entry_Value : Tau.Values.Tau_Value)
      return Tau_Entry
   is
   begin
      return new Tau.Entries.Values.Value_Entry_Record'Class'
        (Values.Value_Entry (Declaration, Name, Entry_Value));
   end Value_Entry;

end Tau.Entries;
