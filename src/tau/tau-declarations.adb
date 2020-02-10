with Ada.Strings.Unbounded;

package body Tau.Declarations is

   type Global_Variable_Type is
     new Root_Tau_Declaration with
      record
         Qualifier : Tau_Storage_Qualifier;
         Type_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure Elaborate
     (Declaration : in out Global_Variable_Type;
      Environment : Tau.Environment.Tau_Environment);

   overriding procedure Compile
     (Declaration : Global_Variable_Type;
      Generator   : in out Tau.Generators.Root_Tau_Generator'Class);

   type Local_Variable_Type is
     new Root_Tau_Declaration with
      record
         Type_Name      : Ada.Strings.Unbounded.Unbounded_String;
         Initialization : Tau.Expressions.Tau_Expression;
      end record;

   overriding procedure Elaborate
     (Declaration : in out Local_Variable_Type;
      Environment : Tau.Environment.Tau_Environment);

   overriding procedure Compile
     (Declaration : Local_Variable_Type;
      Generator   : in out Tau.Generators.Root_Tau_Generator'Class);

   -------------
   -- Compile --
   -------------

   overriding procedure Compile
     (Declaration : Global_Variable_Type;
      Generator   : in out Tau.Generators.Root_Tau_Generator'Class)
   is
   begin
      Generator.Global_Declaration
        (Name      => Declaration.Name,
         Qualifier => Declaration.Qualifier,
         Type_Name =>
           Generator.Shader_Type_Name
             (Ada.Strings.Unbounded.To_String (Declaration.Type_Name)));
   end Compile;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile
     (Declaration : Local_Variable_Type;
      Generator   : in out Tau.Generators.Root_Tau_Generator'Class)
   is
      use type Tau.Expressions.Tau_Expression;
   begin
      Generator.Local_Declaration
        (Name           => Declaration.Name,
         Type_Name      =>
           Generator.Shader_Type_Name
             (Ada.Strings.Unbounded.To_String (Declaration.Type_Name)),
         Initialization =>
           (if Declaration.Initialization = null
            then ""
            else Declaration.Initialization.To_String (Generator)));
   end Compile;

   ---------------
   -- Elaborate --
   ---------------

   overriding procedure Elaborate
     (Declaration : in out Global_Variable_Type;
      Environment : Tau.Environment.Tau_Environment)
   is
      Type_Name : constant String :=
        Ada.Strings.Unbounded.To_String (Declaration.Type_Name);
      T_Entry   : Tau.Entries.Tau_Entry;
   begin
      if not Environment.Contains (Type_Name) then
         Environment.Error
           (Declaration.Defined_At,
            "undefined: " & Type_Name);
         T_Entry := Environment.Get ("integer");
      else
         T_Entry := Environment.Get (Type_Name);

         if not T_Entry.Is_Type_Entry then
            Environment.Error
              (Declaration.Defined_At,
               "expected a type name but found "
               & Type_Name);
            T_Entry := Environment.Get ("integer");
         end if;
      end if;

      Declaration.Dec_Type := T_Entry.Entry_Type;
      Declaration.Dec_Entry :=
        Tau.Entries.Global_Entry
          (Declaration => Declaration.Defined_At,
           Name        => Declaration.Name,
           Entry_Type  => T_Entry.Entry_Type,
           Qualifier   => Declaration.Qualifier);

      Environment.Insert
        (Declaration.Name, Declaration.Dec_Entry);

   end Elaborate;

   ---------------
   -- Elaborate --
   ---------------

   overriding procedure Elaborate
     (Declaration : in out Local_Variable_Type;
      Environment : Tau.Environment.Tau_Environment)
   is
      use type Tau.Expressions.Tau_Expression;
      Type_Name : constant String :=
                    Ada.Strings.Unbounded.To_String (Declaration.Type_Name);
      T_Entry   : Tau.Entries.Tau_Entry;
   begin
      if not Environment.Contains (Type_Name) then
         Environment.Error
           (Declaration.Defined_At,
            "undefined: " & Type_Name);
         T_Entry := Environment.Get ("integer");
      else
         T_Entry := Environment.Get (Type_Name);

         if not T_Entry.Is_Type_Entry then
            Environment.Error
              (Declaration.Defined_At,
               "expected a type name but found "
               & Type_Name);
            T_Entry := Environment.Get ("integer");
         end if;
      end if;

      if Declaration.Initialization /= null then
         Declaration.Initialization.Check (Environment, T_Entry.Entry_Type);
      end if;

      Declaration.Dec_Type := T_Entry.Entry_Type;
      Declaration.Dec_Entry :=
        Tau.Entries.Formal_Argument_Entry
          (Declaration => Declaration.Defined_At,
           Name        => Declaration.Name,
           Entry_Type  => T_Entry.Entry_Type);

      Environment.Insert
        (Declaration.Name, Declaration.Dec_Entry);

   end Elaborate;

   ---------------------------------
   -- Global_Variable_Declaration --
   ---------------------------------

   function Global_Variable_Declaration
     (Position  : GCS.Positions.File_Position;
      Name      : String;
      Qualifier : Tau_Storage_Qualifier;
      Type_Name : String)
      return Tau_Declaration
   is
      Declaration : Global_Variable_Type := Global_Variable_Type'
        (Root_Tau_Declaration with
         Qualifier        => Qualifier,
         Type_Name        =>
           Ada.Strings.Unbounded.To_Unbounded_String (Type_Name));
   begin
      Declaration.Initialize_Object (Position, Name);
      return new Global_Variable_Type'(Declaration);
   end Global_Variable_Declaration;

   --------------------------------
   -- Local_Variable_Declaration --
   --------------------------------

   function Local_Variable_Declaration
     (Position      : GCS.Positions.File_Position;
      Name          : String;
      Type_Name     : String;
      Initial_Value : Tau.Expressions.Tau_Expression)
      return Tau_Declaration
   is
      Declaration : Local_Variable_Type := Local_Variable_Type'
        (Root_Tau_Declaration with
         Type_Name        =>
           Ada.Strings.Unbounded.To_Unbounded_String (Type_Name),
         Initialization   => Initial_Value);
   begin
      Declaration.Initialize_Object (Position, Name);
      return new Local_Variable_Type'(Declaration);
   end Local_Variable_Declaration;

end Tau.Declarations;
