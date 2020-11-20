private with Ada.Containers.Doubly_Linked_Lists;
private with WL.String_Maps;

with Tau.Entries;
with Tau.Objects;
with Tau.Types;
with Tau.Values;

package Tau.Environment is

   type Root_Tau_Environment is
     new Tau.Objects.Root_Tau_Object with private;

   type Tau_Environment is access all Root_Tau_Environment'Class;

   function Create_Child
     (Environment : not null access Root_Tau_Environment'Class;
      Name        : String)
      return Tau_Environment;

   function Contains
     (Environment : Root_Tau_Environment;
      Name        : String)
      return Boolean;

   function Get
     (Environment : Root_Tau_Environment;
      Name        : String)
      return Tau.Entries.Tau_Entry
     with Pre => Contains (Environment, Name);

   procedure Set_Return_Type
     (Environment : in out Root_Tau_Environment'Class;
      Return_Type : Tau.Types.Tau_Type);

   procedure Insert
     (Environment : in out Root_Tau_Environment;
      Name        : String;
      Item        : not null access
        Tau.Entries.Root_Tau_Entry'Class);

   procedure Insert
     (Environment : in out Root_Tau_Environment'Class;
      Name        : String;
      Value       : not null access
        Tau.Values.Root_Tau_Value'Class);

   function Has_Return_Type
     (Environment : Root_Tau_Environment)
      return Boolean;

   function Return_Type
     (Environment : Root_Tau_Environment)
      return Tau.Types.Tau_Type
     with Pre'Class => Environment.Has_Return_Type;

   procedure Iterate
     (Environment : Root_Tau_Environment;
      Process     : not null access
        procedure (Item : Tau.Entries.Tau_Entry));

   function Standard_Library return Tau_Environment;

   function Global_Environment return Tau_Environment;

   procedure Create_Standard_Library;

private

   package Tau_Entry_Maps is
     new WL.String_Maps (Tau.Entries.Tau_Entry, Tau.Entries."=");

   package Error_Stacks is
     new Ada.Containers.Doubly_Linked_Lists
       (Error_Lists.List, Error_Lists."=");

   package Environment_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Tau_Environment);

   type Root_Tau_Environment is
     new Tau.Objects.Root_Tau_Object with
      record
         Parent      : Tau_Environment;
         Map         : Tau_Entry_Maps.Map;
         Env_Type    : Tau.Types.Tau_Type;
         Errors      : Error_Lists.List;
         Error_Stack : Error_Stacks.List;
         Children    : Environment_Lists.List;
      end record;

   overriding function Class_Name
     (Environment : Root_Tau_Environment)
      return String
   is ("environment");

   function Contains
     (Environment : Root_Tau_Environment;
      Name        : String)
      return Boolean
   is (Environment.Map.Contains (Name)
       or else (Environment.Parent /= null
                and then Environment.Parent.Contains (Name)));

   function Get
     (Environment : Root_Tau_Environment;
      Name        : String)
      return Tau.Entries.Tau_Entry
   is (if Environment.Map.Contains (Name)
       then Environment.Map.Element (Name)
       else Environment.Parent.Get (Name));

   function Has_Return_Type
     (Environment : Root_Tau_Environment)
      return Boolean
   is (Tau.Types."/=" (Environment.Env_Type, null)
       or else (Environment.Parent /= null
                and then Environment.Parent.Has_Return_Type));

   function Return_Type
     (Environment : Root_Tau_Environment)
      return Tau.Types.Tau_Type
   is (if Tau.Types."=" (Environment.Env_Type, null)
       then Environment.Parent.Return_Type
       else Environment.Env_Type);

end Tau.Environment;
