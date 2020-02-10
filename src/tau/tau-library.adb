with WL.String_Maps;

with Tau.Environment;

package body Tau.Library is

   package Shader_Maps is
     new WL.String_Maps (Tau.Shaders.Tau_Shader,
                         Tau.Shaders."=");

   Map : Shader_Maps.Map;

   -----------------
   -- Find_Shader --
   -----------------

   function Find_Shader (Name : String) return Tau.Shaders.Tau_Shader is
   begin
      if Map.Contains (Name) then
         return Map.Element (Name);
      else
         return null;
      end if;
   end Find_Shader;

   ------------------
   -- Load_Library --
   ------------------

   procedure Load_Library is
   begin

      Tau.Environment.Create_Standard_Library;

   end Load_Library;

end Tau.Library;
