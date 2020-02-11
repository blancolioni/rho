with Rho.Logging;

package body Tau.Material.Create is

   ------------
   -- Append --
   ------------

   procedure Append
     (Linker   : in out Material_Linker;
      Material : Tau_Material)
   is
   begin
      if Material.Is_Abstract then
         Linker.List.Append (Material);
      elsif Linker.Main = null then
         Linker.Main := Material;
      else
         raise Constraint_Error with
           "only one non-abstract material allowed";
      end if;
   end Append;

   -----------------
   -- Instantiate --
   -----------------

   function Instantiate
     (Linker : Material_Linker)
      return Rho.Material.Material_Type
   is
   begin
      return Linker.Result.Instantiate (Linker.Bindings.Element);
   end Instantiate;

   ----------
   -- Link --
   ----------

   procedure Link
     (Linker   : in out Material_Linker)
   is
      Main_Material : constant Tau_Material := Linker.Main;
      Generic_Count : constant Natural :=
        Natural (Main_Material.Generic_Arguments.Length);
      Bindings      : Tau.Values.Tau_Value_Array  (1 .. Generic_Count);
      Binding_Count : Natural := 0;
   begin

      for Gen of Main_Material.Generic_Arguments loop
         for Mat of Linker.List loop
            declare
               use Tau.Shaders;
               Shader : constant Tau_Shader :=
                 Mat.Find_Shader (Gen.Name);
            begin
               if Shader /= null then
                  Binding_Count := Binding_Count + 1;
                  Bindings (Binding_Count) :=
                    Tau.Shaders.To_Value (Shader);

                  Rho.Logging.Log
                    (Main_Material.Name
                     & ": " & Gen.Name & " => "
                     & Mat.Name & "." & Shader.Name);
                  exit;
               end if;
            end;
         end loop;
      end loop;

      Linker.Bindings := Binding_Holders.To_Holder (Bindings);
      Linker.Result := Main_Material;

   end Link;

   ---------------------------
   -- New_Abstract_Material --
   ---------------------------

   function New_Abstract_Material
     (Declaration : GCS.Positions.File_Position;
      Name        : String;
      Arguments   : Tau.Declarations.Lists.List;
      Shaders     : Tau.Shaders.Lists.List) return Tau_Material
   is
   begin
      return Material : constant Tau_Material :=
        new Root_Tau_Material'
          (Tau.Objects.Root_Tau_Object with
           Is_Abstract       => True,
           Is_Generic        => False,
           Generic_Arguments => <>,
           Bindings          => <>,
           Arguments         => Arguments,
           Shaders           => Shaders)
      do
         Material.Initialize_Object (Declaration, Name);
      end return;
   end New_Abstract_Material;

   ------------------
   -- New_Material --
   ------------------

   function New_Material
     (Declaration     : GCS.Positions.File_Position;
      Is_Generic      : Boolean;
      Is_Abstract     : Boolean;
      Name            : String;
      Generic_Formals : Tau.Declarations.Lists.List;
      Arguments       : Tau.Declarations.Lists.List;
      Shaders         : Tau.Shaders.Lists.List)
      return Tau_Material
   is
   begin
      return Material : constant Tau_Material :=
        new Root_Tau_Material'
          (Tau.Objects.Root_Tau_Object with
           Is_Abstract       => Is_Abstract,
           Is_Generic        => Is_Generic,
           Generic_Arguments => Generic_Formals,
           Bindings          => <>,
           Arguments         => Arguments,
           Shaders           => Shaders)
      do
         Material.Initialize_Object (Declaration, Name);
      end return;
   end New_Material;

end Tau.Material.Create;
