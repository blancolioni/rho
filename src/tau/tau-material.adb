with Tau.Entries;

package body Tau.Material is

   -----------
   -- Apply --
   -----------

   function Apply
     (Material : Root_Tau_Material;
      Values   : Tau.Values.Tau_Value_Array)
      return Tau_Material
   is
      Result : constant Tau_Material :=
        new Root_Tau_Material'
          (Tau.Objects.Root_Tau_Object with
           Is_Abstract       => Material.Is_Abstract,
           Is_Generic        => False,
           Generic_Arguments => <>,
           Arguments         => Material.Arguments,
           Bindings          =>
             Tau.Environment.Standard_Library.Create_Child
               (Material.Name),
           Shaders           => Material.Shaders);
   begin
      Result.Initialize_Object
        (Material.Defined_At, "#" & Material.Name);
      for Value of Values loop
         Result.Bindings.Insert
           (Result.Arguments.First_Element.Name, Value);
         Result.Arguments.Delete_First;
      end loop;
      for Shader of Result.Shaders loop
         Shader := Shader.Bind (Result.Bindings);
      end loop;

      return Result;
   end Apply;

   -----------
   -- Check --
   -----------

   function Check
     (Material : Root_Tau_Material)
      return Boolean
   is
      Environment : constant Tau.Environment.Tau_Environment :=
                      Tau.Environment.Global_Environment.Create_Child
                        (Material.Name);
   begin

      for Formal of Material.Generic_Arguments loop
         Formal.Elaborate (Environment);
      end loop;

      for Argument of Material.Arguments loop
         Argument.Elaborate (Environment);
      end loop;

      for Shader of Material.Shaders loop
         declare
            Shader_Env : constant Tau.Environment.Tau_Environment :=
                           Environment.Create_Child (Shader.Name);
         begin
            Shader.Check (Shader_Env);
         end;
      end loop;

      return not Material.Has_Errors;
   end Check;

   --------------
   -- Children --
   --------------

   overriding function Children
     (Material : Root_Tau_Material)
      return Tau_Node_Array
   is
      function Declaration_List_Children
        (List : Tau.Declarations.Lists.List)
         return Tau_Node_Array;

      function Shader_List_Children
        (List : Tau.Shaders.Lists.List)
         return Tau_Node_Array;

      -------------------------------
      -- Declaration_List_Children --
      -------------------------------

      function Declaration_List_Children
        (List : Tau.Declarations.Lists.List)
         return Tau_Node_Array
      is
         Count  : constant Natural := Natural (List.Length);
         Index  : Positive := 1;
      begin
         return Result : Tau_Node_Array (1 .. Count) do
            for Child of List loop
               Result (Index) := Tau_Node (Child);
               Index := Index + 1;
            end loop;
         end return;
      end Declaration_List_Children;

      --------------------------
      -- Shader_List_Children --
      --------------------------

      function Shader_List_Children
        (List : Tau.Shaders.Lists.List)
         return Tau_Node_Array
      is
         Count : constant Natural := Natural (List.Length);
         Index  : Positive := 1;
      begin
         return Result : Tau_Node_Array (1 .. Count) do
            for Child of List loop
               Result (Index) := Tau_Node (Child);
               Index := Index + 1;
            end loop;
         end return;
      end Shader_List_Children;

   begin
      return Declaration_List_Children (Material.Generic_Arguments)
        & Declaration_List_Children (Material.Arguments)
        & Shader_List_Children (Material.Shaders);
   end Children;

   -----------------
   -- Find_Shader --
   -----------------

   function Find_Shader
     (Material : Root_Tau_Material'Class;
      Name     : String)
      return Tau.Shaders.Tau_Shader
   is
   begin
      for Shader of Material.Shaders loop
         if Shader.Name = Name then
            return Shader;
         end if;
      end loop;
      return null;
   end Find_Shader;

   -----------------
   -- Instantiate --
   -----------------

   function Instantiate
     (Material  : in out Root_Tau_Material'Class)
      return Rho.Material.Material_Type
   is
      Arguments : Tau.Values.Tau_Value_Array (1 .. 0);
   begin
      return Material.Instantiate (Arguments);
   end Instantiate;

   -----------------
   -- Instantiate --
   -----------------

   function Instantiate
     (Material : in out Root_Tau_Material'Class;
      Argument : Tau.Values.Tau_Value)
      return Rho.Material.Material_Type
   is
   begin
      return Material.Instantiate ((1 => Argument));
   end Instantiate;

   -----------------
   -- Instantiate --
   -----------------

   function Instantiate
     (Material  : in out Root_Tau_Material;
      Arguments : Tau.Values.Tau_Value_Array)
      return Rho.Material.Material_Type
   is
      Expected_Count : constant Natural :=
        Natural (Material.Generic_Arguments.Length);
   begin
      if Arguments'Length /= Expected_Count then
         if Arguments'Length < Expected_Count then
            Material.Error ("insufficient arguments in instantiation of "
                            & Material.Name
                            & ": expected"
                            & Expected_Count'Image
                            & " but found"
                            & Arguments'Length'Image);
         else
            Material.Error ("too many arguments in instantiation of "
                            & Material.Name
                            & ": expected"
                            & Expected_Count'Image
                            & " but found"
                            & Arguments'Length'Image);
         end if;

         return null;
      end if;

      declare
         Bindings : constant Tau.Environment.Tau_Environment :=
                      Tau.Environment.Global_Environment.Create_Child
                        (Material.Name);
         Index    : Positive := Arguments'First;
      begin
         for Formal of Material.Generic_Arguments loop
            if not Formal.Get_Type.Is_Convertible_From
              (Arguments (Index).Value_Type)
            then
               raise Constraint_Error with
                 "in argument '" & Formal.Name & "': expected type "
                 & Formal.Get_Type.Name & " but found "
                 & Arguments (Index).Value_Type.Name;
            end if;

            Bindings.Insert
              (Name => Formal.Name,
               Item =>
                 Tau.Entries.Value_Entry
                   (Arguments (Index).Defined_At,
                    Formal.Name, Arguments (Index)));
            Index := Index + 1;
         end loop;

         return Rho.Material.Create_Material
           (Name     => Material.Name,
            Bindings => Bindings,
            Shaders  => Material.Shaders);
      end;

   end Instantiate;

end Tau.Material;
