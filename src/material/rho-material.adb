with Ada.Containers.Doubly_Linked_Lists;

with Rho.Shaders.Builder;
with Rho.Shaders.Stages;
with Rho.Shaders.Variables;

with Rho.Logging;

package body Rho.Material is

   -------------------
   -- Before_Render --
   -------------------

   overriding procedure Before_Render
     (Material   : in out Root_Material_Type;
      Target     : not null access Rho.Render.Render_Target'Class)
   is
   begin
      Target.Activate_Shader (Material.Program);
      for Texture of Material.Textures loop
         Texture.Activate (Target);
      end loop;
   end Before_Render;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile
     (Material : in out Root_Material_Type;
      Target   : not null access Rho.Render.Render_Target'Class)
   is

      package Variable_Lists is
        new Ada.Containers.Doubly_Linked_Lists
          (Rho.Shaders.Variables.Variable_Type,
           Rho.Shaders.Variables."=");

      Bound_Variables : Variable_Lists.List;

      function Compile_Shader
        (Stage : Shader_Stage)
         return Rho.Shaders.Stages.Shader_Type;

      --------------------
      -- Compile_Shader --
      --------------------

      function Compile_Shader
        (Stage : Shader_Stage)
         return Rho.Shaders.Stages.Shader_Type
      is

         use Rho.Shaders;

         Builder : Rho.Shaders.Builder.Root_Builder_Type;

         procedure Add_Partial
           (Partial : Rho.Shaders.Partial.Partial_Shader_Type);

         procedure Check_Attribute
           (Name    : String;
            Binding : Standard_Variable_Binding;
            Count   : Positive := 1);

         procedure Check_Uniform
           (Name    : String;
            Binding : Standard_Variable_Binding;
            Count   : Positive := 1);

         -----------------
         -- Add_Partial --
         -----------------

         procedure Add_Partial
           (Partial : Rho.Shaders.Partial.Partial_Shader_Type)
         is
         begin
            Builder.Add (Partial);
         end Add_Partial;

         ---------------------
         -- Check_Attribute --
         ---------------------

         procedure Check_Attribute
           (Name    : String;
            Binding : Standard_Variable_Binding;
            Count   : Positive := 1)
         is
         begin
            if Builder.Has_Attribute (Name) then
               Bound_Variables.Append
                 (Rho.Shaders.Variables.New_Attribute_Binding
                    (Name, Binding, Count));
               Rho.Logging.Log
                 (Material.Name & ": bound attribute " & Name
                  & " to " & Binding'Image);
            end if;
         end Check_Attribute;

         -------------------
         -- Check_Uniform --
         -------------------

         procedure Check_Uniform
           (Name    : String;
            Binding : Standard_Variable_Binding;
            Count   : Positive := 1)
         is
         begin
            if Builder.Has_Uniform (Name) then
               Bound_Variables.Append
                 (Rho.Shaders.Variables.New_Uniform_Binding
                    (Name, Binding, Count));
               Rho.Logging.Log
                 (Material.Name & ": bound uniform " & Name
                  & " to " & Binding'Image);
            end if;
         end Check_Uniform;

      begin
         Material.Partials.Iterate (Stage, Add_Partial'Access);
         Builder.Build;

         for Binding in Rho.Shaders.Standard_Attribute_Binding loop
            Check_Attribute
              (Name    => Rho.Shaders.Standard_Binding_Name (Binding),
               Binding => Binding,
               Count   =>
                 Rho.Shaders.Standard_Binding_Element_Count (Binding));
         end loop;

         for Binding in Rho.Shaders.Standard_Uniform_Binding loop
            Check_Uniform
              (Name    => Rho.Shaders.Standard_Binding_Name (Binding),
               Binding => Binding,
               Count   =>
                 Rho.Shaders.Standard_Binding_Element_Count (Binding));
         end loop;

         --  Check_Attribute ("position", Position_Attribute, 3);
         --  Check_Attribute ("vertexNormal", Vertex_Normal_Attribute, 3);
         --  Check_Attribute ("texCoord", Vertex_Texture_Coord_Attribute, 2);
         --
         --  Check_Uniform ("model", Model_Uniform);
         --  Check_Uniform ("camera", View_Uniform);
         --  Check_Uniform ("cameraPosition", Camera_Position_Uniform);
         --  Check_Uniform ("ambientColor", Ambient_Color_Uniform);
         --  Check_Uniform ("ambientCoefficient", Ambient_Coefficient_Uniform);
         --  Check_Uniform ("spotColor", Spot_Color_Uniform);
         --  --  Check_Uniform ("attenuation", Spot_Attenuation_Uniform);

         declare
            Shader : constant Rho.Shaders.Stages.Shader_Type :=
                       Rho.Shaders.Stages.Create
                         (Name   => Material.Name & "-" & Stage_Name (Stage),
                          Stage  => Stage,
                          Source => Builder.Source);
         begin
            Target.Compile_Shader (Shader);
            return Shader;
         end;
      end Compile_Shader;

   begin
      if Material.Compiled then
         return;
      end if;

      Material.Program :=
        Target.Create_Program
          (Name    => Material.Name,
           Shaders => (Compile_Shader (Vertex_Shader),
                       Compile_Shader (Fragment_Shader)));

      for Binding of Bound_Variables loop
         if not Material.Program.Has_Variable (Binding.Name) then
            Rho.Logging.Log ("binding: " & Binding.Name);
            Material.Program.Add_Variable (Binding);
            Target.Bind_Variable (Material.Program, Binding);
         end if;
      end loop;

      for Static_Binding of Material.Static_Bindings loop
         declare
            Name : constant String :=
                     Ada.Strings.Unbounded.To_String
                       (Static_Binding.Name);
            Binding : constant Rho.Shaders.Variables.Variable_Type :=
                        Rho.Shaders.Variables.New_Uniform_Binding (Name);
         begin
            Rho.Logging.Log ("binding: " & Binding.Name);
            Material.Program.Add_Variable (Binding);
            Target.Bind_Variable (Material.Program, Binding);
            Target.Add_Uniform (Name, Static_Binding.Value);
         end;
      end loop;

      --  for Binding of Material.Static_Bindings loop
      --     Material.Program.Add_Variable
      --       (Rho.Shaders.Variables.New_Uniform_Binding
      --          (Material.Program,
      --           Ada.Strings.Unbounded.To_String (Binding.Name), 1));
      --  end loop;

      Material.Compiled := True;

   end Compile;

   ---------------------
   -- Default_Shaders --
   ---------------------

   procedure Default_Shaders (This : in out Root_Material_Type'Class) is
   begin
      This.Shader_Names.Include ("node");
   end Default_Shaders;

   --------------------
   -- Execute_Render --
   --------------------

   overriding procedure Execute_Render
     (Material   : in out Root_Material_Type;
      Target     : not null access Rho.Render.Render_Target'Class)
   is
   begin
      Target.Bind_Shader (Material.Program);
   end Execute_Render;

   ---------------
   -- Get_Value --
   ---------------

   overriding function Get_Value
     (This : Root_Material_Type;
      Prop : Rho.Properties.Property)
      return String
   is
   begin
      return This.Property_Bag.Get_Value (Prop);
   end Get_Value;

   -------------
   -- Iterate --
   -------------

   overriding procedure Iterate
     (Material   : Root_Material_Type;
      Stage      : Shader_Stage;
      Process    : not null access
        procedure (Partial : Rho.Shaders.Partial.Partial_Shader_Type))
   is
   begin
      Material.Partials.Iterate (Stage, Process);
   end Iterate;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Material : in out Root_Material_Type;
      Target   : not null access Rho.Render.Render_Target'Class)
   is
   begin
      if Material.Loaded then
         return;
      end if;

      declare
         procedure Add (Shader_Name : String);

         ---------
         -- Add --
         ---------

         procedure Add (Shader_Name : String) is
         begin
            Material.Partials.Append
              (Target.Assets.Partial_Shader (Shader_Name, Vertex_Shader));
            Material.Partials.Append
              (Target.Assets.Partial_Shader (Shader_Name, Fragment_Shader));
         end Add;
      begin
         Material.Shader_Names.Iterate (Add'Access);
      end;

      for Texture of Material.Textures loop
         Texture.Load (Target);
      end loop;

      Material.Loaded := True;

   end Load;

   ---------------
   -- Set_Value --
   ---------------

   overriding procedure Set_Value
     (This  : not null access Root_Material_Type;
      Prop  : Rho.Properties.Property;
      Value : String)
   is
   begin
      This.Property_Bag.Set_Value (Prop, Value);
   end Set_Value;

end Rho.Material;
