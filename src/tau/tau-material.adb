with Tau.Entries;

package body Tau.Material is

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

      if Environment.Has_Errors then
         Environment.Write_Errors;
      end if;

      return not Environment.Has_Errors;
   end Check;

   -----------------
   -- Instantiate --
   -----------------

   function Instantiate
     (Material : Root_Tau_Material'Class)
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
     (Material : Root_Tau_Material'Class;
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
     (Material  : Root_Tau_Material;
      Arguments : Tau.Values.Tau_Value_Array)
      return Rho.Material.Material_Type
   is
      Expected_Count : constant Natural := Natural (Material.Arguments.Length);
   begin
      if Arguments'Length /= Expected_Count then
         raise Constraint_Error with
           "bad instantiation of " & Material.Name;
      end if;

      declare
         Bindings : constant Tau.Environment.Tau_Environment :=
                      Tau.Environment.Global_Environment.Create_Child
                        (Material.Name);
         Index    : Positive := Arguments'First;
      begin
         for Formal of Material.Arguments loop
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
