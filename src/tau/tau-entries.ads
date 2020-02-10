with Rho;

with Tau.Objects;
with Tau.Types;
with Tau.Values;

package Tau.Entries is

   type Root_Tau_Entry is
     abstract new Tau.Objects.Root_Tau_Object with private;

   type Tau_Entry is access all Root_Tau_Entry'Class;

   function Entry_Type
     (Object : Root_Tau_Entry)
      return Tau.Types.Tau_Type;

   function Is_Function_Entry
     (Object : Root_Tau_Entry)
      return Boolean;

   function Is_Value_Entry
     (Object : Root_Tau_Entry)
      return Boolean;

   function Is_Type_Entry
     (Object : Root_Tau_Entry)
      return Boolean;

   function To_Source
     (Object : Root_Tau_Entry)
      return String;

   function Type_Entry
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Entry_Type  : Tau.Types.Tau_Type)
      return Tau_Entry;

   function Value_Entry
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Entry_Value : Tau.Values.Tau_Value)
      return Tau_Entry;

   function Formal_Argument_Entry
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Entry_Type  : Tau.Types.Tau_Type)
      return Tau_Entry;

   function Global_Entry
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Entry_Type  : Tau.Types.Tau_Type;
      Qualifier   : Rho.Storage_Qualifier)
      return Tau_Entry;

private

   type Root_Tau_Entry is
     abstract new Tau.Objects.Root_Tau_Object with
      record
         Typ : Tau.Types.Tau_Type;
      end record;

   function Entry_Type
     (Object : Root_Tau_Entry)
      return Tau.Types.Tau_Type
   is (Object.Typ);

   function Is_Function_Entry
     (Object : Root_Tau_Entry)
      return Boolean
   is (False);

   function Is_Value_Entry
     (Object : Root_Tau_Entry)
      return Boolean
   is (False);

   function Is_Type_Entry
     (Object : Root_Tau_Entry)
      return Boolean
   is (False);

   function To_Source
     (Object : Root_Tau_Entry)
      return String
   is (Object.Name);

end Tau.Entries;
