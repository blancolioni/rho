with WL.String_Maps;

package body Tau.Types.Library is

   package Type_Maps is new WL.String_Maps (Tau_Type);

   Map : Type_Maps.Map;

   ------------
   -- Exists --
   ------------

   function Exists (Name : String) return Boolean is
   begin
      return Map.Contains (Name);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Tau_Type is
   begin
      return Map.Element (Name);
   end Get;

   ------------
   -- Insert --
   ------------

   procedure Insert (Name : String; Item : Tau_Type) is
   begin
      Map.Insert (Name, Item);
   end Insert;

end Tau.Types.Library;
