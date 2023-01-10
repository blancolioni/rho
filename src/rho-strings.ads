private with Ada.Strings.Unbounded;

package Rho.Strings is

   type Rho_String is private;

   function "+" (X : String) return Rho_String;
   function "-" (X : Rho_String) return String;
   function "=" (Left : Rho_String; Right : String) return Boolean;

private

   type Rho_String is
      record
         U : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function "+" (X : String) return Rho_String
   is (U => Ada.Strings.Unbounded.To_Unbounded_String (X));

   function "-" (X : Rho_String) return String
   is (Ada.Strings.Unbounded.To_String (X.U));

   function "=" (Left : Rho_String; Right : String) return Boolean
   is (-Left = Right);

end Rho.Strings;
