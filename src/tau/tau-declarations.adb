with Ada.Strings.Unbounded;

package body Tau.Declarations is

   type Generic_Formal_Type is
     new Root_Tau_Declaration with
      record
         Type_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure Elaborate
     (Declaration : in out Generic_Formal_Type;
      Environment : Tau.Environment.Tau_Environment);

   overriding procedure Compile
     (Declaration : Generic_Formal_Type;
      Generator   : in out Tau.Generators.Root_Tau_Generator'Class)
   is null;

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

   overriding function Children
     (Declaration : Local_Variable_Type)
      return Tau_Node_Array;

   ----------------
   -- Add_Aspect --
   ----------------

   procedure Add_Aspect
     (Declaration : in out Root_Tau_Declaration'Class;
      Name        : String;
      Value       : String)
   is
   begin
      Declaration.Aspects.Insert (Name, Value);
   end Add_Aspect;

   --------------
   -- Children --
   --------------

   overriding function Children
     (Declaration : Local_Variable_Type)
      return Tau_Node_Array
   is
      use type Tau.Expressions.Tau_Expression;
      Nodes : constant Tau_Node_Array :=
        Root_Tau_Declaration (Declaration).Children;
   begin
      if Declaration.Initialization = null then
         return Nodes;
      else
         return Nodes & (1 => Tau_Node (Declaration.Initialization));
      end if;
   end Children;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile
     (Declaration : Global_Variable_Type;
      Generator   : in out Tau.Generators.Root_Tau_Generator'Class)
   is
   begin
      Generator.Global_Declaration
        (Name      =>
           Generator.Normalize_Reference (Declaration.Name),
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
        (Name           =>
           Generator.Normalize_Reference (Declaration.Name),
         Type_Name      =>
           Generator.Shader_Type_Name
             (Ada.Strings.Unbounded.To_String (Declaration.Type_Name)),
         Initialization =>
           (if Declaration.Initialization = null
            then ""
            else Declaration.Initialization.To_String (Generator)));
   end Compile;

   --------------------------
   -- Constant_Declaration --
   --------------------------

   function Constant_Declaration
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
   end Constant_Declaration;

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
         Declaration.Error
           ("undefined: " & Type_Name);
         T_Entry := Environment.Get ("integer");
      else
         T_Entry := Environment.Get (Type_Name);

         if not T_Entry.Is_Type_Entry then
            Declaration.Error
              ("expected a type name but found "
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
     (Declaration : in out Generic_Formal_Type;
      Environment : Tau.Environment.Tau_Environment)
   is
      Type_Name : constant String :=
        Ada.Strings.Unbounded.To_String (Declaration.Type_Name);
      T_Entry   : Tau.Entries.Tau_Entry;
   begin
      if not Environment.Contains (Type_Name) then
         Declaration.Error
           ("undefined: " & Type_Name);
         T_Entry := Environment.Get ("integer");
      else
         T_Entry := Environment.Get (Type_Name);

         if not T_Entry.Is_Type_Entry then
            Declaration.Error
              ("expected a type name but found "
               & Type_Name);
            T_Entry := Environment.Get ("integer");
         end if;
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
         Declaration.Error
           ("undefined: " & Type_Name);
         T_Entry := Environment.Get ("integer");
      else
         T_Entry := Environment.Get (Type_Name);

         if not T_Entry.Is_Type_Entry then
            Declaration.Error
              ("expected a type name but found "
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

   ---------------------
   -- Get_Initializer --
   ---------------------

   function Get_Initializer
     (Declaration : Root_Tau_Declaration'Class)
      return Tau.Expressions.Tau_Expression
   is
   begin
      if Declaration in Local_Variable_Type'Class then
         return Local_Variable_Type (Declaration).Initialization;
      else
         return null;
      end if;
   end Get_Initializer;

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

   ---------------------
   -- Set_Initializer --
   ---------------------

   procedure Set_Initializer
     (Declaration : in out Root_Tau_Declaration'Class;
      Initializer : Tau.Expressions.Tau_Expression)
   is
   begin
      if Declaration in Local_Variable_Type'Class then
         Local_Variable_Type (Declaration).Initialization := Initializer;
      else
         raise Constraint_Error with
           "declaration at " & GCS.Positions.Image (Declaration.Defined_At)
           & " has no initializer";
      end if;
   end Set_Initializer;

   ---------------------------------
   -- Shader_Argument_Declaration --
   ---------------------------------

   function Shader_Argument_Declaration
     (Position  : GCS.Positions.File_Position;
      Name      : String;
      Type_Name : String)
      return Tau_Declaration
   is
      Declaration : Generic_Formal_Type := Generic_Formal_Type'
        (Root_Tau_Declaration with
         Type_Name        =>
           Ada.Strings.Unbounded.To_Unbounded_String (Type_Name));
   begin
      Declaration.Initialize_Object (Position, Name);
      return new Generic_Formal_Type'(Declaration);
   end Shader_Argument_Declaration;

end Tau.Declarations;
