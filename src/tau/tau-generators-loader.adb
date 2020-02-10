with Tau.Generators.GLSL;

package body Tau.Generators.Loader is

   --------------------
   -- Load_Generator --
   --------------------

   function Load_Generator (Name : String) return Root_Tau_Generator'Class is
   begin
      if Name = "glsl" then
         return Tau.Generators.GLSL.GLSL_Generator;
      else
         raise Constraint_Error with
           "unknown generator: " & Name;
      end if;
   end Load_Generator;

end Tau.Generators.Loader;
